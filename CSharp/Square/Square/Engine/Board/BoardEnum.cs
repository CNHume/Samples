//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2017-05-04 CNHume]Created File for the Board Enum Types and Methods
//
// Conditionals:
//
//#define DebugMoveColor
#define TestDraw3
#define RecursiveNullMade
#define HistoryFromTo

namespace Engine {
  using static Command.Parser;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;
  using System.Runtime.CompilerServices;
  using static System.String;

  //
  // Type Aliases:
  //
  using Hashcode = System.UInt64;
  using Ply = System.UInt16;
  using Plane = System.UInt64;

  partial class Board {
    #region SideName Enum
    public enum SideName : int { Black, White }
    #endregion

    #region LoFlags Enum
    public const Int32 nWTMBit = 3;     // pbl of nFiles

    [Flags]
    public enum LoFlags : byte {
      None = 0,
      WTM = 1 << nWTMBit,               // Bit 3
      EPFile = WTM - 1,                 // EPFile Flags
      Passed = WTM << 1,                // Bit 4
      Illegal = Passed << 1,            // Bit 5 Check Flags
      InCheck = Illegal << 1,           // Bit 6
      Final = InCheck << 1,             // Bit 7

      //
      //[Note]Final, InCheck and Illegal are omitted from Hash to
      // prevent repeating calls to IsLegal() for a Transposition:
      //
      Copy = Passed | WTM | EPFile      // EPFlags
    }
    #endregion

    #region HiFlags Enum
    public const Int32 nBishopPairBit = 2;

    [Flags]
    public enum HiFlags : byte {
      None = 0,
      CanOO = 1,                        // Bit 0 Castle Rights
      CanOOO = CanOO << 1,              // Bit 1
      CanCastleMask = CanOOO | CanOO,

      Lite = 1 << nBishopPairBit,       // Bit 2 Bishop Pair Flags
      Dark = Lite << 1,                 // Bit 3
      Pair = Dark | Lite,

      Copy = Pair | CanCastleMask
    }
    #endregion

    #region EGFlags Enum
    [Flags]
    public enum EGFlags : byte {
      None = 0,
      WhiteAlone = 1,                   // Bit 0 End Game Flags
      BlackAlone = WhiteAlone << 1,     // Bit 1
      OutsideSquare = BlackAlone << 1,  // Bit 2
      KBNvK = OutsideSquare << 1,       // Bit 3
      KQvKP = KBNvK << 1,               // Bit 4
      KingAlone = BlackAlone | WhiteAlone,
      EndGame = KQvKP | KBNvK | OutsideSquare | KingAlone,

      Copy = EndGame
    }
    #endregion

    #region DrawFlags Enum
    [Flags]
    public enum DrawFlags : byte {
      None = 0,
      //
      // Fence marks a position where the previous move caused an irreversible change
      // such that prior positions can never be repeated.  This includes Captures and
      // Pawn moves [i.e., moves which reset the 100-Ply Rule Clock]; but it includes
      // Castling Rights changes and simple piece moves made in lieu of an En Passant.
      //
      // Fence positions begin a Transposition Group that consists of positions up to 
      // the next Fence Position.  Draws by Repetition occur in a Transposition Group.
      //
      Fence = 1,                        // Bit 0 Draw Flags
      Draw2 = Fence << 1,               // Bit 1
      Draw3 = Draw2 << 1,               // Bit 2
      Draw50 = Draw3 << 1,              // Bit 3
      DrawIM = Draw50 << 1,             // Bit 4

      //
      //[Note]Omitting Fence, Draw2, Draw3, Draw50 and DrawIM from
      // the Hash prevents a call to IsLegal() for a Transposition
      //
      DrawMask = DrawIM | Draw50 | Draw3,
      Copy = DrawMask | Draw2 | Fence   // Avoid indefinite recursion in PVLookup()

      //
      // From the article CA1008: Enums should have zero value
      // at http://msdn.microsoft.com/en-us/library/ms182149.aspx
      //
      //"Note that if multiple members that have the value zero occur in a flags-attributed
      // enumeration, Enum.ToString() returns incorrect results for members that are not zero."
      //
      //[Symptom]This limitation appeared in FormatFlags() overloads where names of "redundant"
      // zero values, followed by a comma, would be included in the enumeration of Flags.
    }
    #endregion

    #region ModeFlags Enum
    [Flags]
    public enum ModeFlags : byte {
      None = 0,
      NullMade = 1,                     // Bit 0 Mode Flags
      ZWS = NullMade << 1,              // Bit 1
      Reduced = ZWS << 1,               // Bit 2
      Trace = Reduced << 1,             // Bit 3

      Copy = Trace | Reduced | ZWS
#if !RecursiveNullMade
             | NullMade
#endif
    }
    #endregion

    #region Locales
    public class Locale {
      public String Symbols;
      public String Language;
    }

    //
    // Czech,Danish,Dutch,English,Estonian,Finnish,French,German,Hungarian,Icelandic,Italian,Norwegian,Polish,Portuguese,Romanian,Spanish,Swedish
    //
    // The last two Symbols are single character abbreviations for the colors Black and White except for Hungarian, where both colors begin with
    // the letter "F"; so Green and Red are used.
    //
    public static readonly Locale[] Locales = {
      new Locale { Symbols = "PJSVDKCB", Language = "Czech" },
      new Locale { Symbols = "BSLTDKSH", Language = "Danish" },
      new Locale { Symbols = "OPLTDKZW", Language = "Dutch" },
      new Locale { Symbols = "PNBRQKBW", Language = "English" },
      new Locale { Symbols = "PROVLKMV", Language = "Estonian" },
      new Locale { Symbols = "PRLTDKMV", Language = "Finnish" },
      new Locale { Symbols = "PCFTDRNB", Language = "French" },
      new Locale { Symbols = "BSLTDKSW", Language = "German" },
      new Locale { Symbols = "GHFBVKZP", Language = "Hungarian" },
      new Locale { Symbols = "PRBHDKSH", Language = "Icelandic" },
      new Locale { Symbols = "PCATDRNB", Language = "Italian" },
      new Locale { Symbols = "BSLTDKSH", Language = "Norwegian" },
      new Locale { Symbols = "PSGWHKCB", Language = "Polish" },
      new Locale { Symbols = "PCBTDRPB", Language = "Portuguese" },
      new Locale { Symbols = "PCNTDRNA", Language = "Romanian" },
      new Locale { Symbols = "PCATDRNB", Language = "Spanish" },
      new Locale { Symbols = "BSLTDKSV", Language = "Swedish" } };
    #endregion

    #region Piece Enum
    //
    //[Note]PieceWeight[] values must be arranged in the Piece enumeration order given here.
    //
    // The order is otherwise free; but there are also subtle hash collision effects due to
    // different Zobrist values being chosen for a given Piece x Square pair.
    //
    // Piece._ indicates non-captures and non-promotions, in addition to empty squares
    //
    // Piece.Capture is an alias for Piece.K which indicates the captive piece
    // has yet to be identified.
    //
    public enum Piece : byte { _, P, N, B, R, Q, K, EP, Capture = K }

    protected static readonly Piece[] Promotions = { Piece.Q, Piece.R, Piece.B, Piece.N };

    public const Int32 nColors = 2;
    public const Int32 nPieces = 6;
    public const Int32 nSymbols = nPieces + nColors;
    protected const Int32 nPieceCounterBits = nPieces * nPerNibble;
    //protected const UInt64 qPieceCounterBit = 1UL << nPieceCounterBits;

    protected const Byte vCompositionOffset = vP6 + 1;
    internal const Int32 nCompositionOffsetBit = (vCompositionOffset - vP6) * nPerNibble;
    //protected const Int32 nCompositionBits = (vK6 - vCompositionOffset) * nPerNibble;
    //protected const UInt32 uCompositionBit = 1U << nCompositionBits;
    //protected const UInt32 uCompositionMask = uCompositionBit - 1 << nCompositionOffsetBit;
    internal const Int32 nHashPieceBits = (vK6 - vHF) * nPerTwoBits;

    protected const Byte vPieceNull = 1 << 3;
    public const Byte vPieceMask = vPieceNull - 1;

    internal const Byte vFirst = (Byte)(Piece.P);
    internal const Byte vP6 = (Byte)(Piece.P - vFirst);
    internal const Byte vHF = vP6;      // Used for HiFlags within HashPiece
    internal const Byte vN6 = (Byte)(Piece.N - vFirst);
    internal const Byte vB6 = (Byte)(Piece.B - vFirst);
    internal const Byte vR6 = (Byte)(Piece.R - vFirst);
    internal const Byte vQ6 = (Byte)(Piece.Q - vFirst);
    internal const Byte vK6 = (Byte)(Piece.K - vFirst);
    internal const Byte vEP6 = (Byte)(Piece.EP - vFirst);

    internal const Byte vBlack = (Byte)(vK6 + 1);
    internal const Byte vWhite = (Byte)(vBlack + 1);
    #endregion

    #region Square Enum
    public enum sq : byte {
      a1, b1, c1, d1, e1, f1, g1, h1,
      a2, b2, c2, d2, e2, f2, g2, h2,
      a3, b3, c3, d3, e3, f3, g3, h3,
      a4, b4, c4, d4, e4, f4, g4, h4,
      a5, b5, c5, d5, e5, f5, g5, h5,
      a6, b6, c6, d6, e6, f6, g6, h6,
      a7, b7, c7, d7, e7, f7, g7, h7,
      a8, b8, c8, d8, e8, f8, g8, h8
    }

    //
    // Plane:
    //
    public const Plane BIT0 = 1UL;
    protected const Plane BITHI = BIT0 << nSquares - 1; // For loadPawnWins() and sqHi()
    protected const Plane MASK64 = UInt64.MaxValue;

    // The following are used by resetWhite|BlackPawnAtx()
    public const Plane qpFileA = 0x0101010101010101UL;
    public const Plane qpFileB = qpFileA << 1;
    public const Plane qpFileC = qpFileB << 1;
    public const Plane qpFileD = qpFileC << 1;
    public const Plane qpFileE = qpFileD << 1;
    public const Plane qpFileF = qpFileE << 1;
    public const Plane qpFileG = qpFileF << 1;
    public const Plane qpFileH = qpFileG << 1;

    // The following are used by Position.white|blackCanPromote()
    public const Plane qpRank1 = 0xFFUL;
    public const Plane qpRank2 = qpRank1 << nFiles;
    public const Plane qpRank3 = qpRank2 << nFiles;
    public const Plane qpRank4 = qpRank3 << nFiles;
    public const Plane qpRank5 = qpRank4 << nFiles;
    public const Plane qpRank6 = qpRank5 << nFiles;
    public const Plane qpRank7 = qpRank6 << nFiles;
    public const Plane qpRank8 = qpRank7 << nFiles;
    #endregion

    #region Move Enum
    //
    // Move Fields:
    //
#if TestFromTo
    public const Int32 nToBit = 0;                      // Bit 0
    public const Int32 nFromBit = nToBit + 6;           // Bit 6
    public const Int32 nPromoteBit = nFromBit + 6;      // Bit 12
#else
    public const Int32 nFromBit = 0;                    // Bit 0
    public const Int32 nToBit = nFromBit + 6;           // Bit 6
    public const Int32 nPromoteBit = nToBit + 6;        // Bit 12 4-bits for promotion and castling
#endif
    public const Int32 nCastlesBit = nPromoteBit + 3;   // Bit 15[Chess 960]
    public const Int32 nPieceBit = nCastlesBit + 1;     // Bit 16 4-bits for piece and unused hi-bit
    public const Int32 nPieceHiBit = nPieceBit + 3;     // Bit 19 unused
    public const Int32 nCaptiveBit = nPieceHiBit + 1;   // Bit 20 4-bits for captures and unused hi-bit
    public const Int32 nCaptiveHiBit = nCaptiveBit + 3; // Bit 23 unused
    public const Int32 nOnlyFileBit = 28;

    //[Flags]
    public enum Move : uint {
      None = 0,
      Castles = 1 << nCastlesBit,       // Bit 15 Lo 16-bits suffice for PACN
      //PieceHi = 1 << nPieceHiBit,     // Bit 19 unused
      CaptiveHi = 1 << nCaptiveHiBit,   // Bit 23 unused
      NoteFinal = CaptiveHi << 1,       // Bit 24 4-bits for annotations
      NoteCheck = NoteFinal << 1,       // Bit 25
#if TestDraw3
      NoteDraw = NoteCheck << 1,        // Bit 26
      NoteDraw2 = NoteDraw << 1,        // Bit 27
#endif
      OnlyFile = 1U << nOnlyFileBit,    // Bit 28 4-bits for abbreviation and debug
      StoreMask = OnlyFile - 1,         // Mask28 Hi 4-bits masked from [Trans|Quiet]Position.BestMove
      OnlyRank = OnlyFile << 1,         // Bit 29
      Qxnt = OnlyRank << 1,             // Bit 30 Used by abbreviateRefresh()
#if DebugMoveColor
      WTM = Qxnt << 1,                  // Bit 31 for debugging
#endif
      FromToMask = (uSquareMask << nToBit) | (uSquareMask << nFromBit),
      PromoteMask = vPieceMask << nPromoteBit,
      ShortMask = Castles | PromoteMask | FromToMask,
      LimitMask = (vPieceMask << nPieceBit) | ShortMask, // Include moving piece; but omit captures
      CaptiveMask = vPieceMask << nCaptiveBit,
      Material = CaptiveMask | PromoteMask, // To distinguish quiet maneuvers

      //
      // NormalMask preserves CaptiveMask; but masks any annotation
      //
#if DebugMoveColor
      NormalMask = WTM | LimitMask | CaptiveMask,
#else
      NormalMask = LimitMask | CaptiveMask,
#endif

      //
      // The three Limit Values do not make use of CaptiveMask:
      //
      NullMove = Piece.P << nPromoteBit,
      Undefined = Piece.K << nPromoteBit,
      EmptyMove = Castles | Undefined   // Denotes Final Position in Transposition
    }
    #endregion

    #region PositionType Enum
    public enum PositionType : byte { Prefix, FEN, EPD }
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

    internal static Boolean equalMoves(Move move1, Move move2) {
      if ((move1 & Move.LimitMask) != (move2 & Move.LimitMask))
        return false;

      var bCapture1 = (move1 & Move.CaptiveMask) != 0;
      var bCapture2 = (move2 & Move.CaptiveMask) != 0;
      return bCapture1 == bCapture2;
    }

    internal static Boolean isAbove(Int32 nTo, Boolean bWTM) {
      return bWTM ? nTo >= (Int32)sq.a5 : nTo < (Int32)sq.a5;
    }

    protected Piece captive(Move move) {
      var uCapture = (UInt32)move >> nCaptiveBit & vPieceMask;
      return (Piece)uCapture;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void unpack1(Move move, out Int32 nFrom, out Int32 nTo, out UInt32 uPiece, out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
      nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
      uPiece = (UInt32)move >> nPieceBit & vPieceMask;
      bCapture = (move & Move.CaptiveMask) != 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpack2(Move move, out Int32 nFrom, out Int32 nTo, out UInt32 uPiece, out UInt32 uPromotion,
                                  out Boolean bCastles, out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
      nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
      uPiece = (UInt32)move >> nPieceBit & vPieceMask;
      uPromotion = (UInt32)move >> nPromoteBit & vPieceMask;
      bCastles = isCastles(move);
      bCapture = (move & Move.CaptiveMask) != 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackMove1(Move move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion,
                                      out Boolean bCapture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      var nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
      var nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
      sqFrom = (sq)nFrom;
      sqTo = (sq)nTo;
      promotion = (Piece)((UInt32)move >> nPromoteBit & vPieceMask);
      piece = (Piece)((UInt32)move >> nPieceBit & vPieceMask);
      bCapture = (move & Move.CaptiveMask) != 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void unpackMove2(Move move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion,
                                      out Boolean bCastles, out Boolean bCapture, out Piece capture) {
      Debug.Assert(isDefinite(move), "Indefinite Move");
      var nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
      var nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
      sqFrom = (sq)nFrom;
      sqTo = (sq)nTo;
      promotion = (Piece)((UInt32)move >> nPromoteBit & vPieceMask);
      piece = (Piece)((UInt32)move >> nPieceBit & vPieceMask);
      capture = (Piece)((UInt32)move >> nCaptiveBit & vPieceMask);
      bCapture = capture != Piece._;
      bCastles = isCastles(move);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void unpackHistory(Move move, out Int32 nFrom, out Int32 nTo) {
      nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
#if HistoryFromTo
      nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
#else
      nFrom = ((Int32)move >> nPieceBit & vPieceMask) - vFirst;
#endif
    }
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
          if ((Knight - 1 & Knight) == 0)
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
