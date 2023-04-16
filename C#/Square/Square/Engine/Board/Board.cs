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

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

using static System.String;
using static System.StringComparison;

namespace Engine;

using static Command.Parser;
using static Logging.Logger;
using static Position;
using static Position.PositionSide;

//
// Type Aliases:
//
using Plane = UInt64;
using Ply = UInt16;

partial class Board : IEquatable<Board> {
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

  private const UInt32 uSquareMask = (1 << 6) - 1;

  private const Int32 nA8H1 = 7;
  private const Int32 nA1H8 = 9;

  protected const String sOrthodoxStartEPD = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - hmvc 0; fmvn 1;";
  protected const String sOrthodoxStartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

  public const int OperationsCapacity = 4;
  #endregion                            // Constants

  #region Constructors
  static Board() {
#if DebugInit
    LogLine("Initializing Board...");
#endif
    #region Assertions
    // Assumed by both CaptiveMask and PromoteMask
    Trace.Assert((UInt32)Piece.None == 0,
                 "Undefined Piece must be Zero");
    // Assumed by eval() and appendPiece()
    Trace.Assert(vK6 == nPieces - 1,
                 "King assumed to be final Piece");
    Trace.Assert((UInt32)SideFlags.CanOOO << 1 == 1 << nBishopPairBit,
                 "Bishop Mask in unexpected position");

    Trace.Assert(Move.ShortMask == (Move)UInt16.MaxValue,
                 "ShortMask is not equal to UInt16.MaxValue");
    Trace.Assert(Move.EmptyMove == (Move.EmptyMove & Move.EqualMask),
                 "EqualMask does not preserve Empty Move");
    Trace.Assert(Move.NullMove == (Move.NullMove & Move.EqualMask),
                 "EqualMask does not preserve Null Move");
    Trace.Assert(Move.Undefined == (Move.Undefined & Move.EqualMask),
                 "EqualMask does not preserve Undefined Move");
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

  protected Board(GameState state) {
    State = state;
    Side = new PositionSide[nSides];
    initSides();
    newAtxToCount();                    //[Conditional]
  }

  #region Methods
  #region Initialization Methods
  [MemberNotNull(
    nameof(Friend),
    nameof(Foe))]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void initSides() {
    //[Note]Friend and Foe must always correspond to the value of WTM()
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
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void newAtxToCount() {
    AtxToCount = new SByte[nSquares];
  }

  [Conditional("BuildAtxToCount")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void ensureAtxToCount() {
    if (AtxToCount == null)
      newAtxToCount();
  }

  // Virtual method called by Position.Clear()
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public virtual void Clear() {
    //
    //[Note]Flags are reset when resetMove() calls Position.Copy(),
    // which calls Board.Copy() which also calls Board.CopyFlags().
    //
    FlagsTurn = default;
    FlagsEval = default;
    FlagsDraw = default;
    FlagsMode = default;

    HalfMoveClock = 0;
    EPTarget = default;

    RankPiece = Pawn = King = Knight = DiagPiece = OrthPiece = 0UL;
#if !Magic
    A1H8Piece = A8H1Piece = FilePiece = 0UL;
#endif
    HashPawn = Hash = 0UL;

    AttackedSum =
      BlackControlled =
      WhiteControlled = 0UL;

    Name = default;

    foreach (var side in Side)
      side.Clear();
  }

  #region Ensure EPD Operations
  private void addOperation(
    Dictionary<String, List<String>?> operations, String sKey, params String[] sValues) {
    if (operations != null) {
      var values = new List<String>(sValues);
      operations.Add(sKey, values);
    }
  }

  //
  // Ensure presence of Full Move Number and Half-Move Clock
  //
  private void ensureOperations() {
    if (Operations != null) return;

    Operations = new Dictionary<String, List<String>?>(OperationsCapacity);
    var wMove = MoveNumber(GamePly);
    addOperation(Operations, "fmvn", wMove.ToString());
    addOperation(Operations, "hmvc", HalfMoveClock.ToString());

    if (!IsNullOrEmpty(Name)) {
      var sValue = StringToVerbatimLiteral(Name);
      addOperation(Operations, "id", sValue);
    }
  }
  #endregion                            // Ensure EPD Operations
  #endregion                            // Initialization Methods

  #region Static Initialization
  public static void SetPieceSymbols(String? sLanguage) {
    if (sLanguage == null)
      PieceSymbols = default;
    else {
      var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

      var found = Locales.FirstOrDefault(
        locale => sLanguage.Equals(locale.Language, InvariantCultureIgnoreCase));
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

  [MemberNotNull(
    nameof(AtxRank),
    nameof(AtxFile),
    nameof(AtxA1H8),
    nameof(AtxA8H1),
    nameof(AtxKing),
    nameof(AtxKnight),
    nameof(BitRank),
    nameof(BitFile),
    nameof(BitA1H8),
    nameof(BitA8H1),
    nameof(RankOffset),
    nameof(OffsetDiag),
    nameof(OffsetOrth),
    nameof(OffsetA1H8),
    nameof(OffsetA8H1),
    nameof(zobristRandom),
    nameof(zobristBuffer),
    nameof(zobristTurn),
    nameof(zobristDraw),
    nameof(zobristFile),
    nameof(zobristRightsBlack),
    nameof(zobristRightsWhite),
    nameof(zobristBlack),
    nameof(zobristWhite),
    nameof(zobristExcludedFrom),
    nameof(zobristExcludedTo),
    nameof(zobristExcludedCastles),
    nameof(zobristExcludedPromotion)
    )]
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
    loadOrthAtx();                      // Each of the following loads takes around 0.333 ms
    loadDiagAtx();

    newZobrist();
    loadZobrist();
#if InitDeBruijn
#if ByteDeBruijn
    deBruijnByte = newDeBruijn(3);      // 8 == 1 << 3
    loadDeBruijn(deBruijnByte, 3, vDeBruijn);
#endif
#if DeBruijn
#if FullData
    deBruijnFull = newDeBruijn(6);      // 64 == 1 << 6
    loadDeBruijn(deBruijnFull, 6, qDeBruijn);
#else                                   //!FullData
    deBruijnHalf = newDeBruijn(5);      // 32 == 1 << 5
    loadDeBruijn(deBruijnHalf, 5, uDeBruijn);
#endif                                  // FullData
#endif                                  // DeBruijn
#endif                                  // InitDeBruijn
    colorSquares();
  }
  #endregion                            // Static Initialization

  #region Copy Methods
  //
  // Deep Copy:
  //
  [Conditional("BuildAtxToCount")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void copyAtxToCount(Board board) {
    Array.Copy(board.AtxToCount, AtxToCount, board.AtxToCount.Length);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public void CopyFlags(Board board) {
    //[C#]Avoid ambiguity with TurnFlags.Copy
    FlagsTurn = board.FlagsTurn & TurnFlags.Copy;
    FlagsEval = board.FlagsEval & EvalFlags.Copy;
    FlagsDraw = board.FlagsDraw & DrawFlags.Copy;
    FlagsMode = board.FlagsMode & ModeFlags.Copy;
  }

  #region BoardSide
  [MemberNotNull(
    nameof(Friend),
    nameof(Foe)
    )]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void copySides(Board board) {
    for (var nSide = 0; nSide < Side.Length; nSide++)
      Side[nSide].Copy(board.Side[nSide]);

    //[Note]Friend and Foe must always correspond to the value of WTM()
    (Friend, Foe) = GetSides(WTM());
  }
  #endregion                            // BoardSide

  //
  // The Board base class represents the state of the board, including Ply counts, 8 Planes (a.k.a, bit-boards), three Rotations,
  // Flags (e.g., Castle Rights and EnPassant) a 100-ply HalfMoveClock to detect 50-move draws, Piece Counters and two Hashcodes.
  // WTM() determines White To Move, and is true iff GamePly is even.
  // ~168 Bytes for simple Rotation [24 Bytes less for Magic]
  //
  [MemberNotNull(
    nameof(Friend),
    nameof(Foe),
    nameof(State))]
  public void Copy(Board board) {
    State = board.State;
    CopyFlags(board);                   // 4-bytes for Board Flags

    GamePly = board.GamePly;            // 2-bytes
    EPTarget = board.EPTarget;          // 1-byte (nullable)
    HalfMoveClock = board.HalfMoveClock;// 1-byte

    Hash = board.Hash;                  // 8-bytes
    HashPawn = board.HashPawn;          // 8-bytes

    Pawn = board.Pawn;                  // 8-bytes
    King = board.King;                  // 8-bytes
    Knight = board.Knight;              // 8-bytes
    DiagPiece = board.DiagPiece;        // 8-bytes
    OrthPiece = board.OrthPiece;        // 8-bytes

    RankPiece = board.RankPiece;        // 8-bytes
#if !Magic
    FilePiece = board.FilePiece;        // 8-bytes
    A1H8Piece = board.A1H8Piece;        // 8-bytes
    A8H1Piece = board.A8H1Piece;        // 8-bytes
#endif
    copySides(board);                   // 2 x 36 = 72-bytes

    ensureAtxToCount();                 //[Conditional]
    copyAtxToCount(board);              //[Conditional] 64-bytes
  }
  #endregion                            // Copy Methods
  #endregion                            // Constructors

  #region IEquatable Interface Methods
  public override Int32 GetHashCode() {
    var uHi = (UInt32)(Hash >> 32);
    var uLo = (UInt32)Hash;
    return (Int32)(uHi ^ uLo);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Boolean equalEPLegal(Board board) {
    var fTurnDelta = FlagsTurn ^ board.FlagsTurn;
    return !fTurnDelta.Has(TurnFlags.EPLegal);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Boolean equalEPTarget(Board board) {
    return EPTarget == board.EPTarget;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Boolean equalSides(Board board) {
    for (var nSide = 0; nSide < Side.Length; nSide++) {
      if (!Side[nSide].Equals(board.Side[nSide]))
        return false;
    }
    return true;
  }

  public Boolean Equals(Board? board) {
    if (board is null)
      return false;
#if SafeEquals
    if (Hash != board.Hash)             //[Shortcut]
      return false;

    var bEqual = equalSides(board) &&
      Pawn == board.Pawn &&
      King == board.King &&
      Knight == board.Knight &&
      DiagPiece == board.DiagPiece &&
      OrthPiece == board.OrthPiece &&
      equalEPLegal(board) &&
      (equalEPTarget(board) || !IsEPLegal());

    Trace.Assert(bEqual, "Hashcode Collision Detected");
    return bEqual;
#else                                   // SafeEquals
      return Hash == board.Hash;
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
  #endregion                            // IEquatable Interface Methods

  #region Ply Methods
  protected static UInt16 MoveDelta(Ply wPly) {
    return (UInt16)((wPly + 1) / 2);
  }

  // Inverse of ply()
  public static UInt16 MoveNumber(Ply wPly) {
    return (UInt16)(wPly / 2 + 1);
  }

  // Inverse of MoveNumber()
  private static Ply ply(Ply wMoveNumber, Boolean bWTM) {
    // Zero is sometimes used when the initial MoveNumber is unknown
    var nMoveNumber = wMoveNumber > 0 ? wMoveNumber : 1;
    var nPly = (nMoveNumber - 1) * 2;
    return (Ply)(bWTM ? nPly : nPly + 1);
  }
  #endregion                            // Ply Methods

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
  internal static Boolean IsShowRank(Move move) {
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
    return (move & Move.EqualMask) != Move.Undefined;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected static Boolean IsEmptyMove(Move move) {
    return (move & Move.EqualMask) == Move.EmptyMove;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean IsNullMove(Move move) {
    return (move & Move.EqualMask) == Move.NullMove;
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
    if ((move1 & Move.EqualMask) != (move2 & Move.EqualMask))
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
  #endregion                            // Unpack Methods
  #endregion                            // Move Methods
  #endregion                            // Methods
}
