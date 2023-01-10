//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2010-06-06 CNHume]Created Class
//
// Conditionals:
//
//#define BuildAtxTo
//#define BuildAtxToCount                 //[ToDo]
//#define Magic
#define SafeEquals
#define HashPieces
//#define InitDeBruijn
//#define ByteDeBruijn
//#define DeBruijn                        // DeBruijn vs Mask
//#define FullData                        // Full vs Half
#define DebugInit

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Diagnostics.CodeAnalysis;
  using System.Linq;
  using System.Runtime.CompilerServices;

  using static System.String;
  using static Command.Parser;
  using static Logging.Logger;
  using static Position;
  using static Position.PositionSide;

  //
  // Type Aliases:
  //
  using Plane = UInt64;
  using Ply = UInt16;

  partial class Board : ICloneable, IEquatable<Board> {
    /*
     *            Hungarian Notation
     *
     * Precision      Signed    Unsigned    Float
     *      8            z         v
     *     16            m         w
     *     32            n         u          r
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
    protected const Byte vNibble = (1 << nPerNibble) - 1;
    protected const Byte vTwoBits = vMod4 - 1;

    internal const Int32 nFiles = 8;
    internal const Int32 nRanks = nFiles;
    internal const Int32 nSquares = nRanks * nFiles;
    internal const Int32 nSquareUndefined = nSquares;

    protected const Int32 nRankLast = nSquares - nFiles;
    private const UInt32 uSquareMask = (1 << 6) - 1;

    private const Int32 nA8H1 = 7;
    private const Int32 nA1H8 = 9;
    #endregion

    #region Constructors
    static Board() {
#if DebugInit
      LogLine("Initializing Board...");
#endif
      #region Assertions
      Trace.Assert((UInt32)Piece.None == 0,     // Assumed by both CaptiveMask and PromoteMask
                   "Undefined Piece must be Zero");
      Trace.Assert(vK6 == nPieces - 1,          // Assumed by eval() and appendPiece()
                   "King assumed to be final Piece");
      Trace.Assert((UInt32)SideFlags.CanOOO << 1 == 1 << nBishopPairBit,
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
      initAttacks();

      Parameter = new PositionParameter[nSides];
      ensureParameters();
    }

    private static void ensureParameters() {
      var sideNames = (SideName[])Enum.GetValues(typeof(SideName));
      foreach (var sideName in sideNames) {
        var nSide = (Int32)sideName;
        if (Parameter[nSide] == null) {
          Parameter[nSide] = new PositionParameter(sideName);
        }
      }
    }

    public Board() {
      Side = new PositionSide[nSides];
      initSides();
      newAtxToCount();               //[Conditional]
    }

    #region Initialization Methods
    protected void EnsureSides(Position position) {
      foreach (var parameter in Parameter) {
        var nSide = (Int32)parameter.SideName;
        if (Side[nSide] == null)
          Side[nSide] = new PositionSide(position, parameter);
      }
    }

    [MemberNotNull(nameof(Friend), nameof(Foe))]
    private void initSides() {
      //[Note]Friend and Foe must always correspond to TurnFlags.WTM
      (Friend, Foe) = GetSides(WTM());
    }
#if BuildAtxTo
    [Conditional("BuildAtxTo")]
    [MemberNotNull(nameof(AtxTo))]
    private void newAtxTo() {
      AtxTo = new Plane[nSquares];
    }

    [Conditional("BuildAtxTo")]
    private void ensureAtxTo() {
      if (AtxTo == null)
        newAtxTo();
    }
#endif
    [Conditional("BuildAtxToCount")]
    [MemberNotNull(nameof(AtxToCount))]
    private void newAtxToCount() {
      AtxToCount = new SByte[nSquares];
    }

    [Conditional("BuildAtxToCount")]
    private void ensureAtxToCount() {
      if (AtxToCount == null)
        newAtxToCount();
    }

    // Called by Position.Clear()
    public virtual void Clear() {
      foreach (var side in Side)
        side.Clear();

      RankPiece = Pawn = King = Knight = DiagPiece = OrthPiece = 0UL;
#if !Magic
      A1H8Piece = A8H1Piece = FilePiece = 0UL;
#endif
      HashPawn = Hash = 0UL;
    }
    #endregion                          // Initialization Methods

    #region Static Initialization
    public static void SetPieceSymbols(String? sLanguage) {
      if (sLanguage == null)
        PieceSymbols = default;
      else {
        var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

        var found = Locales.FirstOrDefault(
          locale => sLanguage.Equals(locale.Language, StringComparison.InvariantCultureIgnoreCase));
        PieceSymbols = found?.Symbols;

        if (PieceSymbols == null) {
          blackParameter.Symbol = "B";
          whiteParameter.Symbol = "W";
        }
        else {
          Trace.Assert(nSymbols <= PieceSymbols.Length, "Insufficient number of Piece Symbols");
          blackParameter.Symbol = PieceSymbols[vBlack].ToString();
          whiteParameter.Symbol = PieceSymbols[vWhite].ToString();
        }
      }
    }

    private static void initAttacks() {
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

      newOrthBit();
      newDiagBit();

      loadOrthBit();
      loadDiagBit();
#endif
      newKingAtx();
      newKnightAtx();

      loadPieceAtx();

      newOrthAtx();
      newDiagAtx();

      //
      // Note the order dependency here:  If Magic is defined loadDiagAtx() and loadOrthAtx()
      // require that MagicA1H8[], MagicA8H1[] and MagicFile[] have been built by loadMagic().
      //
      loadOrthAtx();                    // Each of the following loads takes around 0.333 ms
      loadDiagAtx();

      newZobrist();
      loadZobrist();
#if InitDeBruijn
#if ByteDeBruijn
      deBruijnByte = newDeBruijn(3);    // 8 == 1 << 3
      loadDeBruijn(deBruijnByte, 3, vDeBruijn);
#endif
#if DeBruijn
#if FullData
      deBruijnFull = newDeBruijn(6);    // 64 == 1 << 6
      loadDeBruijn(deBruijnFull, 6, qDeBruijn);
#else                                   //!FullData
      deBruijnHalf = newDeBruijn(5);    // 32 == 1 << 5
      loadDeBruijn(deBruijnHalf, 5, uDeBruijn);
#endif                                  // FullData
#endif                                  // DeBruijn
#endif                                  // InitDeBruijn
      colorSquares();
    }
    #endregion                          // Static Initialization

    //
    // Copy Constructor:
    //
    public Board(Board board) {
      board.CopyTo(this);
    }

    public Object Clone() {
      return new Board(this);
    }

    #region Copy Methods
    //
    // Deep Copy:
    //
    [Conditional("BuildAtxToCount")]
    [MemberNotNull(nameof(AtxToCount))]
    protected void CopyAtxToCountTo(Board board) {
      if (AtxToCount == null)
        throw new ArgumentNullException(nameof(AtxToCount));

      Array.Copy(AtxToCount, board.AtxToCount, AtxToCount.Length);
    }

    public void CopyFlagsTo(Board board) {                      // 1 byte
      for (var nSide = 0; nSide < Side?.Length; nSide++)
        board.Side[nSide].FlagsSide = Side[nSide].FlagsSide & SideFlags.Copy;

      board.FlagsTurn = FlagsTurn & TurnFlags.Copy;
      board.FlagsGame = FlagsGame & GameFlags.Copy;
      board.FlagsDraw = FlagsDraw & DrawFlags.Copy;
      board.FlagsMode = FlagsMode & ModeFlags.Copy;
    }

    #region BoardSide
    [MemberNotNull(nameof(Friend), nameof(Foe))]
    protected void CopySidesTo(Board board) {
      for (var nSide = 0; nSide < Side?.Length; nSide++) {      // 34 bytes + 1 nullable byte
#if HashPieces
        board.Side[nSide].PieceHash = Side[nSide].PieceHash;    // 8-bytes
#endif
        board.Side[nSide].Counts = Side[nSide].Counts;          // 2-bytes
        board.Side[nSide].KingPos = Side[nSide].KingPos;        // 1-byte (nullable)

        board.Side[nSide].Piece = Side[nSide].Piece;            // 8-bytes
        board.Side[nSide].PawnA1H8Atx = Side[nSide].PawnA1H8Atx;// 8-bytes optimizing resetPawnAtx()
        board.Side[nSide].PawnA8H1Atx = Side[nSide].PawnA8H1Atx;// 8-bytes
      }

      //[Note]Friend and Foe must always correspond to TurnFlags.WTM
      (board.Friend, board.Foe) = board.GetSides(WTM());
    }
    #endregion                          // BoardSide

    //
    // The Board base class represents the state of the board, including Ply counts, 8 Planes (a.k.a, bit-boards), three rotations,
    // Flags (e.g., Castle Rights and EnPassant) a 100-ply HalfMoveClock to detect 50-move draws, Piece Counters and two Hashcodes.
    // TurnFlags include WTM.  WTM determines which side is to move; and should agree with the Ply Parity.
    // ~186 Bytes for simple Rotation [24 Bytes less for Magic]
    //
    public void CopyTo(Board board) {
      CopyFlagsTo(board);                   // 6-bytes for Flags

      board.NullPly = NullPly;              // 2-bytes
      board.GamePly = GamePly;              // 2-bytes
      board.HalfMoveClock = HalfMoveClock;  // 1-byte
      board.Hash = Hash;                    // 8-bytes
      board.HashPawn = HashPawn;            // 8-bytes

      board.Pawn = Pawn;                    // 8-bytes
      board.King = King;                    // 8-bytes
      board.Knight = Knight;                // 8-bytes
      board.DiagPiece = DiagPiece;          // 8-bytes
      board.OrthPiece = OrthPiece;          // 8-bytes

      board.RankPiece = RankPiece;          // 8-bytes
#if !Magic
      board.FilePiece = FilePiece;          // 8-bytes
      board.A1H8Piece = A1H8Piece;          // 8-bytes
      board.A8H1Piece = A8H1Piece;          // 8-bytes
#endif
      CopySidesTo(board);                   // 2 x 35 = 70-bytes

      board.ensureAtxToCount();         //[Conditional]
      CopyAtxToCountTo(board);          //[Conditional] 64-bytes
    }
    #endregion                          // Copy Methods
    #endregion                          // Constructors

    #region IEquatable Interface Methods
    public override Int32 GetHashCode() {
      var uHi = (UInt32)(Hash >> 32);
      var uLo = (UInt32)Hash;
      return (Int32)(uHi ^ uLo);
    }

    public Boolean Equals(Board? board) {
      if (board is null)
        return false;
#if SafeEquals
      if (board.Hash != Hash)           //[Shortcut]
        return false;

      var bSideEqual = true;
      for (var nSide = 0; nSide < Side?.Length; nSide++) {
        if (board.Side[nSide].Piece != Side[nSide].Piece) {
          bSideEqual = false;
          break;
        }

        var fSideDelta = board.Side[nSide].FlagsSide ^ Side[nSide].FlagsSide;
        if (fSideDelta.Has(SideFlags.Copy)) {
          bSideEqual = false;
          break;
        }
      }

      var bEqual = bSideEqual &&
        board.Pawn == Pawn &&
        ((board.FlagsTurn ^ FlagsTurn) & TurnFlags.Copy) == 0 &&
        board.King == King &&
        board.Knight == Knight &&
        board.DiagPiece == DiagPiece &&
        board.OrthPiece == OrthPiece;

      Trace.Assert(bEqual, "Hashcode Collision Detected");
      return bEqual;
#else                                   // SafeEquals
      return board.Hash == Hash;
#endif
    }

    public override Boolean Equals(Object? obj) {
      return Equals(obj as Board);
    }

    public static Boolean operator ==(Board board1, Board board2) {
      return Equals(board1, board2);
    }

    public static Boolean operator !=(Board board1, Board board2) {
      return !Equals(board1, board2);
    }
    #endregion                          // IEquatable Interface Methods

    #region Ply Methods
    protected static UInt16 MoveDelta(Ply wPly) {
      return (UInt16)((wPly + 1) / 2);
    }

    // Inverse of plyCount()
    public static UInt16 MoveNumber(Ply wPly) {
      return (UInt16)(wPly / 2 + 1);
    }

    // Inverse of MoveNumber()
    private static Ply plyCount(Ply wMove) {
      return (Ply)((wMove - 1) * 2);
    }
    #endregion                          // Ply Methods

    #region Move Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static UInt32 uBit(Int32 n) {
      return 1U << n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Plane bit(Int32 n) {
      return BIT0 << n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static bool IsShowRank(Move move) {
      return (move & Move.HideRank) == 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean IsShowFile(Move move) {
      return (move & Move.HideFile) == 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean IsShowFrom(Move move) {
      return (move & Move.HideFrom) == 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean IsDefinite(Move move) {
      return IsDefined(move) && !IsEmptyMove(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean IsDefined(Move move) {
      return (move & Move.NormalMask) != Move.Undefined;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Boolean IsEmptyMove(Move move) {
      return (move & Move.NormalMask) == Move.EmptyMove;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean IsNullMove(Move move) {
      return (move & Move.NormalMask) == Move.NullMove;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Boolean IsCastles(Move move) {
      return move.Has(Move.Castles);
    }

    //[Compiler]move.Has(Move.CaptiveMask) does not work here!
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Boolean IsCapture(Move move) {
      return move.Has(Move.CaptiveMask);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean EqualMoves(Move move1, Move move2) {
      if ((move1 & Move.LimitMask) != (move2 & Move.LimitMask))
        return false;

      return IsCapture(move1) == IsCapture(move2);
    }

    #region Move Setter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move FromMove(Int32 nFrom) {
      return (Move)(nFrom << nFromBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move ToMove(Int32 nTo) {
      return (Move)(nTo << nToBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move FromToMove(Int32 nFrom, Int32 nTo) {
      return ToMove(nTo) | FromMove(nFrom);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Move pieceMove(Piece piece) {
      return (Move)((UInt32)piece << nPieceBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move CaptureMove(Piece capture) {
      return (Move)((UInt32)capture << nCaptiveBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move PromotionMove(Piece p) {
      return (Move)((UInt32)p << nPromoteBit);
    }
    #endregion

    #region Move Getter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Int32 From(Move move) {
      return (Int32)move >> nFromBit & (Int32)uSquareMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Int32 To(Move move) {
      return (Int32)move >> nToBit & (Int32)uSquareMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Byte PieceIndex(UInt32 uPiece) {
      return (Byte)(uPiece - vFirst);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Piece IndexPiece(Byte vPiece) {
      return (Piece)(vPiece + vFirst);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static UInt32 moved(Move move) {
      return (UInt32)move >> nPieceBit & vPieceMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static UInt32 Captured(Move move) {
      return (UInt32)move >> nCaptiveBit & vPieceMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static UInt32 Promoted(Move move) {
      return (UInt32)move >> nPromoteBit & vPieceMask;
    }
    #endregion

    #region Unpack Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpack1(
      Move move, out Int32 nFrom, out Int32 nTo,
      out UInt32 uPiece, out Boolean bCapture) {
      Debug.Assert(IsDefinite(move), "Indefinite Move");
      nFrom = From(move);
      nTo = To(move);

      uPiece = moved(move);
      bCapture = IsCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static void unpack2(
      Move move, out Int32 nFrom, out Int32 nTo,
      out UInt32 uPiece, out UInt32 uPromotion,
      out Boolean bCastles, out Boolean bCapture) {
      Debug.Assert(IsDefinite(move), "Indefinite Move");
      nFrom = From(move);
      nTo = To(move);

      uPiece = moved(move);
      uPromotion = Promoted(move);

      bCastles = IsCastles(move);
      bCapture = IsCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackMove1(
      Move move, out Sq sqFrom, out Sq sqTo,
      out Piece piece, out Piece promotion, out Boolean bCapture) {
      Debug.Assert(IsDefinite(move), "Indefinite Move");
      sqFrom = (Sq)From(move);
      sqTo = (Sq)To(move);

      piece = (Piece)moved(move);
      promotion = (Piece)Promoted(move);

      bCapture = IsCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackMove2(
      Move move, out Sq sqFrom, out Sq sqTo,
      out Piece piece, out Piece promotion, out Piece capture,
      out Boolean bCastles, out Boolean bCapture) {
      Debug.Assert(IsDefinite(move), "Indefinite Move");
      sqFrom = (Sq)From(move);
      sqTo = (Sq)To(move);

      piece = (Piece)moved(move);
      promotion = (Piece)Promoted(move);
      capture = (Piece)Captured(move);

      bCastles = IsCastles(move);
      bCapture = IsCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackShort(
      Move move, out Int32 nFrom, out Int32 nTo,
      out UInt32 uPromotion, out Boolean bCastles) {
      nFrom = From(move);
      nTo = To(move);

      uPromotion = Promoted(move);
      bCastles = IsCastles(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackHistory(
      Move move, out Int32 nFrom, out Int32 nTo) {
#if HistoryFromTo
      nFrom = from(move);
#else
      nFrom = PieceIndex((Byte)moved(move));
#endif
      nTo = To(move);
    }
    #endregion                          // Unpack Methods
    #endregion                          // Move Methods

    #region EPD Operation Methods
    private void addOperation(
      Dictionary<String, List<String>?> operations, String sKey, params String[] sValues) {
      if (operations != null) {
        var values = new List<String>(sValues);
        operations.Add(sKey, values);
      }
    }

    private void newOperations() {
      Operations = new Dictionary<String, List<String>?>(4);
      var nMove = GamePly / 2 + 1;
      addOperation(Operations, "fmvn", nMove.ToString());
      addOperation(Operations, "hmvc", HalfMoveClock.ToString());

      if (!IsNullOrEmpty(Name)) {
        var sValue = StringToVerbatimLiteral(Name);
        addOperation(Operations, "id", sValue);
      }
    }
    #endregion                          // EPD Operation Methods
  }
}
