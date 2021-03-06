﻿//
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
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;
  using System.Runtime.CompilerServices;

  using static Command.Parser;
  using static Logging.Logger;
  using static Position;
  using static System.String;

  //
  // Type Aliases:
  //
  using Hashcode = System.UInt64;
  using Ply = System.UInt16;

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
      Trace.Assert((UInt32)Piece.None == 0,     // Assumed by both CaptiveMask and PromoteMask
                   "Undefined Piece must be Zero");
      Trace.Assert(vK6 == nPieces - 1,          // Assumed by eval() and appendPiece()
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
      initAttacks();

      Parameter = new PositionParameter[nSides];
      newParameters();
    }

    private static void newParameters() {
      foreach (var sideName in (SideName[])Enum.GetValues(typeof(SideName))) {
        var nSide = (Int32)sideName;
        Parameter[nSide] = new PositionParameter(sideName);
      }
    }

    public Board() {
      Side = new BoardSide[nSides];
      newSides();
    }

    private void newSides() {
      foreach (var parameter in Parameter) {
        var nSide = (Int32)parameter.SideName;
        Side[nSide] = new BoardSide(parameter);
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
          Parameter[Black].Symbol = "B";
          Parameter[White].Symbol = "W";
        }
        else {
          Trace.Assert(nSymbols <= PieceSymbols.Length, "Insufficient number of Piece Symbols");
          Parameter[Black].Symbol = PieceSymbols[vBlack].ToString();
          Parameter[White].Symbol = PieceSymbols[vWhite].ToString();
        }
      }
    }

    protected static void initAttacks() {
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

    #region Ply Methods
    protected static UInt16 moveDelta(Ply wPly) {
      return (UInt16)((wPly + 1) / 2);
    }

    // Inverse of plyCount()
    internal static UInt16 moveNumber(Ply wPly) {
      return (UInt16)(wPly / 2 + 1);
    }

    // Inverse of moveNumber()
    private static Ply plyCount(Ply wMove) {
      return (Ply)((wMove - 1) * 2);
    }
    #endregion

    #region Move Methods
    internal static Boolean isDefinite(Move move) {
      return isDefined(move) && !isEmptyMove(move);
    }

    internal static Boolean isDefined(Move move) {
      return (move & Move.NormalMask) != Move.Undefined;
    }

    internal static Boolean isEmptyMove(Move move) {
      return (move & Move.NormalMask) == Move.EmptyMove;
    }

    internal static Boolean isNullMove(Move move) {
      return (move & Move.NormalMask) == Move.NullMove;
    }

    internal static Boolean isCastles(Move move) {
      return (move & Move.Castles) != 0;
    }

    internal static bool isCapture(Move move) {
      return (move & Move.CaptiveMask) != 0;
    }

    internal static Boolean equalMoves(Move move1, Move move2) {
      if ((move1 & Move.LimitMask) != (move2 & Move.LimitMask))
        return false;

      return isCapture(move1) == isCapture(move2);
    }

    internal static Boolean isAbove(Int32 nTo, Boolean bWTM) {
      return bWTM ? nTo >= (Int32)sq.a5 : nTo < (Int32)sq.a5;
    }

    #region Move Setter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move fromMove(Int32 nFrom) {
      return (Move)(nFrom << nFromBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move toMove(Int32 nTo) {
      return (Move)(nTo << nToBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move fromToMove(Int32 nFrom, Int32 nTo) {
      return toMove(nTo) | fromMove(nFrom);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move pieceMove(Piece piece) {
      return (Move)((UInt32)piece << nPieceBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move captureMove(Piece capture) {
      return (Move)((UInt32)capture << nCaptiveBit);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Move promotionMove(Piece p) {
      return (Move)((UInt32)p << nPromoteBit);
    }
    #endregion

    #region Move Getter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Int32 from(Move move) {
      return (Int32)move >> nFromBit & (Int32)uSquareMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 to(Move move) {
      return (Int32)move >> nToBit & (Int32)uSquareMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Byte pieceIndex(UInt32 uPiece) {
      return (Byte)(uPiece - vFirst);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Piece indexPiece(Byte vPiece) {
      return (Piece)(vPiece + vFirst);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static UInt32 moved(Move move) {
      return (UInt32)move >> nPieceBit & vPieceMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static UInt32 captured(Move move) {
      return (UInt32)move >> nCaptiveBit & vPieceMask;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static UInt32 promoted(Move move) {
      return (UInt32)move >> nPromoteBit & vPieceMask;
    }
    #endregion

    #region Unpack Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void unpack1(Move move, out Int32 nFrom, out Int32 nTo,
                                 out UInt32 uPiece, out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      nFrom = from(move);
      nTo = to(move);

      uPiece = moved(move);
      bCapture = isCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void unpack2(Move move, out Int32 nFrom, out Int32 nTo,
                                 out UInt32 uPiece, out UInt32 uPromotion,
                                 out Boolean bCastles, out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      nFrom = from(move);
      nTo = to(move);

      uPiece = moved(move);
      uPromotion = promoted(move);

      bCastles = isCastles(move);
      bCapture = isCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackMove1(Move move, out sq sqFrom, out sq sqTo,
                                      out Piece piece, out Piece promotion, out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      sqFrom = (sq)from(move);
      sqTo = (sq)to(move);

      piece = (Piece)moved(move);
      promotion = (Piece)promoted(move);

      bCapture = isCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackMove2(Move move, out sq sqFrom, out sq sqTo,
                                      out Piece piece, out Piece promotion, out Piece capture,
                                      out Boolean bCastles, out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      sqFrom = (sq)from(move);
      sqTo = (sq)to(move);

      piece = (Piece)moved(move);
      promotion = (Piece)promoted(move);
      capture = (Piece)captured(move);

      bCastles = isCastles(move);
      bCapture = isCapture(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void unpackShort(Move move, out Int32 nFrom, out Int32 nTo,
                                     out UInt32 uPromotion, out Boolean bCastles) {
      nFrom = from(move);
      nTo = to(move);

      uPromotion = promoted(move);
      bCastles = isCastles(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void unpackHistory(Move move, out Int32 nFrom, out Int32 nTo) {
#if HistoryFromTo
      nFrom = from(move);
#else
      nFrom = pieceIndex((Byte)moved(move));
#endif
      nTo = to(move);
    }
    #endregion
    #endregion

    #region Flag Methods
    public Boolean WTM() {
      return (FlagsLo & LoFlags.WTM) != 0;
    }

    protected void setLegal(Boolean bLegal) {
      if (bLegal)
        FlagsLo &= ~LoFlags.Illegal;
      else
        FlagsLo |= LoFlags.Illegal;
    }

    protected void setInCheck(Boolean bInCheck) {
      if (bInCheck)
        FlagsLo |= LoFlags.InCheck;
      else
        FlagsLo &= ~LoFlags.InCheck;
    }

    protected void setFinal() {
      FlagsLo |= LoFlags.Final;
    }

    public Boolean IsFinal() {
      return (FlagsLo & LoFlags.Final) != 0;
    }

    public Boolean InCheck() {
      return (FlagsLo & LoFlags.InCheck) != 0;
    }

    public Boolean IsPassed() {
      return (FlagsLo & LoFlags.Passed) != 0;
    }

    public Boolean IsDraw() {
      return (FlagsDraw & DrawFlags.DrawMask) != 0;
    }

    public Boolean IsDraw2() {
      return (FlagsDraw & DrawFlags.Draw2) != 0;
    }

    public Boolean IsDraw50() {
      return (FlagsDraw & DrawFlags.Draw50) != 0;
    }

    public Boolean IsInsufficient() {
      return (FlagsDraw & DrawFlags.DrawIM) != 0;
    }

    public Boolean IsStalemate() {
      return IsFinal() && !InCheck();
    }

    //
    // Recognize Draw by Insufficient Material:
    //
    protected void setInsufficient() {
      FlagsDraw &= ~DrawFlags.DrawIM;   //[Safe]

      // No Pawn, Rook or Queen:
      if (Pawn == 0 && RectPiece == 0) {
        //
        // If either a single Knight or multiple Bishops covering squares
        // of only one color remain, then even a helpmate is not possible.
        //
        if (DiagPiece == 0) {           // Test for KK[N]:
          if (OneBitOrNone(Knight))
            FlagsDraw |= DrawFlags.DrawIM;
        }
        else if (Knight == 0) {         // Test for KB*KB* covering only one color, or KK:
          if ((DiagPiece & LiteSquare) == 0 ||
              (DiagPiece & DarkSquare) == 0)
            FlagsDraw |= DrawFlags.DrawIM;
        }
      }
    }

    protected void clrRepetition() {
      FlagsDraw &= ~(DrawFlags.Draw3 | DrawFlags.Draw2);
    }

    protected void setRepetition(Boolean bDraw3) {
      FlagsDraw |= bDraw3 ? DrawFlags.Draw3 : DrawFlags.Draw2;
    }

    protected DrawFlags fdr() {
      return FlagsDraw & (DrawFlags.Draw3 | DrawFlags.Draw2);
    }

    public Boolean IsFence() {
      return (FlagsDraw & DrawFlags.Fence) != 0;
    }

    protected void clrFence() {
      FlagsDraw &= ~DrawFlags.Fence;
    }

    protected void setFence() {
      FlagsDraw |= DrawFlags.Fence;
    }

    protected void setDraw50() {
      if (HalfMoveClock < HalfMoveClockMax)
        FlagsDraw &= ~DrawFlags.Draw50;
      else                              // 50 Move Rule
        FlagsDraw |= DrawFlags.Draw50;
    }

    protected Boolean IsNullMade() {
      return (FlagsMode & ModeFlags.NullMade) != 0;
    }

    private void clrNullMade() {
      FlagsMode &= ~ModeFlags.NullMade;
    }

    private void setNullMade() {
      FlagsMode |= ModeFlags.NullMade;
    }

    protected bool IsTrace() {
      return (FlagsMode & ModeFlags.Trace) != 0;
    }

    protected void clrTrace() {
      FlagsMode &= ~ModeFlags.Trace;
    }

    protected void setTrace(params Hashcode[] qHashcodes) {
      if (qHashcodes.Any(qHashcode => qHashcode == Hash))
        FlagsMode |= ModeFlags.Trace;
    }
    #endregion

    #region EPD Operation Methods
    protected void addOperation(
      Dictionary<String, List<String>> operations, String sKey, params String[] sValues) {
      if (operations is not null) {
        var values = new List<String>(sValues);
        operations.Add(sKey, values);
      }
    }

    private void newOperations() {
      Operations = new Dictionary<String, List<String>>(4);
      var nMove = GamePly / 2 + 1;
      addOperation(Operations, "fmvn", nMove.ToString());
      addOperation(Operations, "hmvc", HalfMoveClock.ToString());

      if (!IsNullOrEmpty(Name)) {
        var sValue = StringToVerbatimLiteral(Name);
        addOperation(Operations, "id", sValue);
      }
    }
    #endregion
  }
}
