//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2010-06-06 CNHume]Created Class
//
// Conditionals:
//
//#define Magic
#define SafeEquals
#define HashPieces
#define InitDeBruijn
#define HalfDeBruijn
#define DebugInit

namespace Engine {
  using static Board.BoardSide;
  using static Logging.Logger;

  using System;
  using System.Diagnostics;
  using System.Linq;

  partial class Board : ICloneable, IEquatable<Board> {
    /*
     *            Hungarian Notation
     *
     * Precision      Signed    Unsigned    Float
     *      8            z         v
     *     16            m         w
     *     32            n         u          f
     *     64            l         q          d
     *   Plane                     qp
     *   Object                    o
     *   Boolean                   b
     * Char/String       c         s
     *    Type           t
     */
    #region Constants
    protected const Int32 nPerByte = 8;
    internal const Int32 nPerNibble = 4;
    internal const Int32 nPerTwoBits = 2;
    protected const Byte vMod4 = 1 << nPerTwoBits;
    internal const Byte vNibble = (1 << nPerNibble) - 1;
    internal const Byte vTwoBits = vMod4 - 1;

    public const Int32 nFiles = 8;
    public const Int32 nRanks = nFiles;
    public const Int32 nSquares = nRanks * nFiles;

    public const Int32 nRankLast = nSquares - nFiles;
    internal const UInt32 uSquareMask = (1 << 6) - 1;

    public const Int32 nA8H1 = 7;
    public const Int32 nA1H8 = 9;
    #endregion

    #region Constructors
    static Board() {
#if DebugInit
      LogLine("Initializing Board...");
#endif
      #region Assertions
      Trace.Assert((UInt32)Piece._ == 0,// Assumed by both CaptiveMask and PromoteMask
                   "Undefined Piece must be Zero");
      Trace.Assert(vK6 == nPieces - 1,  // Assumed by eval() and appendPiece()
                   "King assumed to be final Piece");
      Trace.Assert((UInt32)HiFlags.CanOOO << 1 == 1 << nBishopPairBit,
                   "Bishop Mask in unexpected position");

      Trace.Assert(Move.ShortMask == (Move)UInt16.MaxValue,
                   "ShortMask is not equal to UInt16.MaxValue");
      Trace.Assert(Move.EmptyMove == (Move.EmptyMove & Move.LimitMask),
                   "LimitMask does not preserve Empty Move");
      Trace.Assert(Move.NullMove == (Move.NullMove & Move.LimitMask),
                   "LimitMask does not preserve Null Move");
      Trace.Assert(Move.Undefined == (Move.Undefined & Move.LimitMask),
                   "LimitMask does not preserve Undefined Move");
      #endregion

      //
      // Initialize static data used to find attacks:
      //
      initStatic();
    }

    public Board() {
      Side = new BoardSide[nSides];
      foreach (var sideName in (SideName[])Enum.GetValues(typeof(SideName))) {
        var hashcode = sideName == SideName.Black ? ZobristBlack : ZobristWhite;
        Side[(Int32)sideName] = new BoardSide(sideName, hashcode);
      }
    }

    //
    // Copy Constructor:
    //
    public Board(Board board) {
      board.CopyTo(this);
    }

    public Object Clone() {
      return new Board(this);
    }

    //
    // Deep Copy:
    //
    public void CopyFlagsTo(Board board) {
      for (var nSide = 0; nSide < Side.Length; nSide++)
        board.Side[nSide].FlagsHi = Side[nSide].FlagsHi & HiFlags.Copy;

      board.FlagsLo = FlagsLo & LoFlags.Copy;
      board.FlagsEG = FlagsEG & EGFlags.Copy;
      board.FlagsDraw = FlagsDraw & DrawFlags.Copy;
      board.FlagsMode = FlagsMode & ModeFlags.Copy;
    }

    //
    // The Board base class represents the state of the board, including Ply counts, 8 Planes (a.k.a, bit-boards), three rotations,
    // Flags (e.g., Castle Rights and EnPassant) a 100-ply HalfMoveClock to detect 50-move draws, Piece Counters and two Hashcodes.
    // LoFlags include WTM.  WTM determines which side is to move; and should agree with the Ply Parity.
    // ~186 Bytes for simple Rotation [24 Bytes less for Magic]
    //
    public void CopyTo(Board board) {
      CopyFlagsTo(board);                       // 6-bytes

      board.NullPly = NullPly;                  // 2-bytes
      board.GamePly = GamePly;                  // 2-bytes
      board.HalfMoveClock = HalfMoveClock;      // 1-byte
      board.Hash = Hash;                        // 8-bytes
      board.HashPawn = HashPawn;                // 8-bytes

      board.Pawn = Pawn;                        // 8-bytes
      board.King = King;                        // 8-bytes
      board.Knight = Knight;                    // 8-bytes
      board.DiagPiece = DiagPiece;              // 8-bytes
      board.RectPiece = RectPiece;              // 8-bytes

      board.RankPiece = RankPiece;              // 8-bytes
#if !Magic
      board.FilePiece = FilePiece;              // 8-bytes
      board.A1H8Piece = A1H8Piece;              // 8-bytes
      board.A8H1Piece = A8H1Piece;              // 8-bytes
#endif
      for (var nSide = 0; nSide < Side.Length; nSide++) {           // 42 bytes + 1 nullable byte
#if HashPieces
        board.Side[nSide].PieceHash = Side[nSide].PieceHash;        // 8-bytes
#endif
        board.Side[nSide].Zobrist = Side[nSide].Zobrist;            // 8-bytes
        board.Side[nSide].Counts = Side[nSide].Counts;              // 2-bytes
        board.Side[nSide].KingPos = Side[nSide].KingPos;            // 1-byte (nullable)

        board.Side[nSide].Piece = Side[nSide].Piece;                // 8-bytes
        board.Side[nSide].PawnA1H8Atx = Side[nSide].PawnA1H8Atx;    // 8-bytes optimizing resetPawnAtx()
        board.Side[nSide].PawnA8H1Atx = Side[nSide].PawnA8H1Atx;    // 8-bytes
      }
    }
    #endregion

    #region Static Initialization
    public static void SetPieceSymbols(String sLanguage) {
      if (sLanguage is null)
        PieceSymbols = default;
      else {
        var found = Locales.FirstOrDefault(
          locale => sLanguage.Equals(locale.Language, StringComparison.InvariantCultureIgnoreCase));
        PieceSymbols = found?.Symbols;

        if (PieceSymbols is null) {
          BlackSymbol = "B";
          WhiteSymbol = "W";
        }
        else {
          Trace.Assert(nSymbols <= PieceSymbols.Length, "Insufficient number of Piece Symbols");
          BlackSymbol = PieceSymbols[vBlack].ToString();
          WhiteSymbol = PieceSymbols[vWhite].ToString();
        }
      }
    }

    protected static void initStatic() {
      //
      // Whether the Ray Atx planes are Magic or Rotated,
      // rotateRank() will be needed by [rank|rect]Atx().
      //
      newRankOffset();
      loadRankOffset();
#if Magic
      newMagic();
      loadMagic();
#else
      newRotation();
      loadRotation();

      newRectBit();
      newDiagBit();

      loadRectBit();
      loadDiagBit();
#endif
      newKingAtx();
      newKnightAtx();

      loadPieceAtx();

      newRectAtx();
      newDiagAtx();

      //
      // Note the order dependency here:  If Magic is defined loadDiagAtx() and loadRectAtx()
      // require that A1H8Magic[], A8H1Magic[] and FileMagic[] have been built by loadMagic().
      //
      loadRectAtx();                    // Each of the following loads takes around 0.333 ms
      loadDiagAtx();

      newZobrist();
      loadZobrist();
#if InitDeBruijn
      deBruijnByte = newDeBruijn(3);
      loadDeBruijn(deBruijnByte, 3, vDeBruijn);
#if HalfDeBruijn
      deBruijnHalf = newDeBruijn(5);    // 32 == 1 << 5
      loadDeBruijn(deBruijnHalf, 5, uDeBruijn);
#else
      deBruijnFull = newDeBruijn(6);    // 64 == 1 << 6
      loadDeBruijn(deBruijnFull, 6, qDeBruijn);
#endif
#endif
      colorSquares();
    }
    #endregion

    #region IEquatable Interface Methods
    public override Int32 GetHashCode() {
      var uHi = (UInt32)(Hash >> 32);
      var uLo = (UInt32)Hash;
      return (Int32)(uHi ^ uLo);
    }

    public Boolean Equals(Board board) {
      if (board is null)
        return false;
#if SafeEquals
      if (board.Hash != Hash)           //[Shortcut]
        return false;

      var bSideEqual = true;
      for (var nSide = 0; nSide < Side.Length; nSide++) {
        if (board.Side[nSide].Piece != Side[nSide].Piece ||
            ((board.Side[nSide].FlagsHi ^ Side[nSide].FlagsHi) & HiFlags.Copy) != 0) {
          bSideEqual = false;
          break;
        }
      }

      var bEqual = bSideEqual &&
        board.Pawn == Pawn &&
        ((board.FlagsLo ^ FlagsLo) & LoFlags.Copy) == 0 &&
        board.King == King &&
        board.Knight == Knight &&
        board.DiagPiece == DiagPiece &&
        board.RectPiece == RectPiece;

      Trace.Assert(bEqual, "Hashcode Collision Detected");
      return bEqual;
#else                                   // SafeEquals
      return board.Hash == Hash;
#endif
    }

    public override Boolean Equals(Object obj) {
      return Equals(obj as Board);
    }

    public static Boolean operator ==(Board board1, Board board2) {
      return Equals(board1, board2);
    }

    public static Boolean operator !=(Board board1, Board board2) {
      return !Equals(board1, board2);
    }
    #endregion
  }
}
