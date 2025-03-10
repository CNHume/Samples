﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2017-05-04 CNHume]Created File for Board Enum Types and Constants
//
// Conditionals:
//
#define DebugSideToMove
#define RecursiveNullMade

namespace Engine;
//
// Type Aliases:
//
using Plane = UInt64;

partial class Board {
  #region Enumerations
  #region SideName Enum
  internal static readonly Int32 nSides = Enum.GetNames(typeof(SideName)).Length;

  internal const Int32 Black = (Int32)SideName.Black;
  internal const Int32 White = (Int32)SideName.White;

  public enum SideName : byte { Black, White }
  #endregion                            // SideName Enum

  #region SideFlags Enum
  //
  // SideFlags  0:6
  // --------------
  //  0:1 CanOO
  //  1:1 CanOOO
  //  2:1 Dark
  //  3:1 Lite
  //  4:1 Alone
  //  5:1 Insufficient
  //
  private const Int32 nBishopPairBit = 2;
  private const Int32 nAloneBit = 4;

  [Flags]
  public enum SideFlags : byte {
    None = 0,
    CanOO = 1,                          // Bit 0 Castle Rights
    CanOOO = CanOO << 1,                // Bit 1
    CanCastle = CanOOO | CanOO,

    Dark = 1 << nBishopPairBit,         // Bit 2 Bishop Pair Flags
    Lite = Dark << 1,                   // Bit 3
    Pair = Lite | Dark,

    Alone = 1 << nAloneBit,             // Bit 4 Lone King
    Insufficient = Alone << 1,          // Bit 5 Insufficient Material to Force Mate
    AloneOrInsufficient = Insufficient | Alone,

    // Flags used by weighPieces()
    Weight = AloneOrInsufficient | Pair,
    Copy = Weight | CanCastle
  }
  #endregion                            // SideFlags Enum

  #region TurnFlags Enum
  //
  // TurnFlags  0:4
  // --------------
  //  0:1 EPLegal
  //  1:1 Illegal
  //  2:1 InCheck
  //  3:1 Final
  //

  [Flags]
  public enum TurnFlags : byte {
    None = 0,
    EPLegal = 1,                        // Bit 0
    Illegal = EPLegal << 1,             // Bit 1 Check Flags
    InCheck = Illegal << 1,             // Bit 2
    Final = InCheck << 1,               // Bit 3

    //
    //[Note]Final, InCheck and Illegal are omitted from Hash to
    // prevent repeating calls to IsLegal() for a Transposition:
    //
    //[C#]Copy = EPLegal causes EPLegal to be serialzed as Copy.
    //Copy = EPLegal                    // EPLegal
  }
  #endregion                            // TurnFlags Enum

  #region DrawFlags Enum
  //
  // DrawFlags  0:5
  // --------------
  //  0:1 Draw0
  //  1:1 Draw2
  //  2:1 Draw3
  //  3:1 Draw50
  //  4:1 DrawIM
  //
  // Draw0 denotes the beginning of a Repetition Cycle, which includes the current
  // position and any subsequent position up to (but not including) the next Draw0
  // Position.
  //
  // Draw0 marks a position where the previous move caused an irreversible change
  // such that prior positions can never be repeated.  This includes Captures and
  // Pawn moves [i.e., moves which reset the 100-Ply Rule Clock]; but it includes
  // Castling Rights changes and simple piece moves made in lieu of an En Passant.
  //
  // Draw3 marks a position where Draw by 3-fold Repetition can be claimed, which
  // can only occur within a Repetition Cycle.
  //
  // Draw50 marks positions where a 50-Move [or 100-Ply] Rule Draw can be claimed.
  //
  [Flags]
  public enum DrawFlags : byte {
    None = 0,
    Draw0 = 1,                          // Bit 0 Draw Flags
    Draw2 = Draw0 << 1,                 // Bit 1
    Draw3 = Draw2 << 1,                 // Bit 2
    Draw50 = Draw3 << 1,                // Bit 3
    DrawIM = Draw50 << 1,               // Bit 4

    //
    //[Note]Omitting Draw0, Draw2, Draw3, Draw50 and DrawIM from
    // the Hash prevents a call to IsLegal() for a Transposition
    //
    DrawMask = DrawIM | Draw3,
    // Avoid indefinite recursion in lookupPV()
    Copy = DrawMask | Draw50 | Draw2 | Draw0

    //
    // From the article CA1008: Enums should have zero value
    // at http://msdn.microsoft.com/en-us/library/ms182149.aspx
    //
    //"Note that if multiple members that have the value zero occur in a flags-attributed
    // enumeration, Enum.ToString() returns incorrect results for members that are not zero."
    //
    //[Symptom]This limitation appeared in formatFlags() overloads where names of "redundant"
    // zero values, followed by a comma, would be included in the enumeration of Flags.
  }
  #endregion                            // DrawFlags Enum

  #region EvalFlags Enum
  //
  // EvalFlags  0:3
  // --------------
  //  0:1 OutsideSquare
  //  1:1 KBNvK
  //  2:1 KQvKP
  //
  [Flags]
  public enum EvalFlags : byte {
    None = 0,
    OutsideSquare = 1,                  // Bit 0 Eval Flags
    KBNvK = OutsideSquare << 1,         // Bit 1
    KQvKP = KBNvK << 1,                 // Bit 2
    EndGame = KQvKP | KBNvK | OutsideSquare,

    Copy = EndGame
  }
  #endregion                            // EvalFlags Enum

  #region ModeFlags Enum
  //
  // ModeFlags  0:4
  // --------------
  //  0:1 NullMade
  //  1:1 ZWS
  //  2:1 Reduced
  //  3:1 Trace
  //
  [Flags]
  public enum ModeFlags : byte {
    None = 0,
    NullMade = 1,                       // Bit 0 Mode Flags
    ZWS = NullMade << 1,                // Bit 1
    Reduced = ZWS << 1,                 // Bit 2
    Trace = Reduced << 1,               // Bit 3

    Copy = Trace | Reduced | ZWS
#if !RecursiveNullMade
            | NullMade
#endif
  }
  #endregion                            // ModeFlags Enum

  #region Piece Enum
  //
  //[Note]PieceWeight[] values must be arranged in the Piece enumeration order given here.
  //
  // The order is otherwise free; but there are also subtle hash collision effects due to
  // different Zobrist values being chosen for a given Piece x Games pair.
  //
  // Piece.None is used to indicate non-captures, non-promotions and empty squares.
  //
  // Piece.Capture is an alias for Piece.K to indicate that the captive piece has not yet
  // been identified.
  //
  public enum Piece : byte { None, P, N, B, R, Q, K, EP, Capture = K }

  protected static readonly Piece[] Promotions = { Piece.Q, Piece.N, Piece.B, Piece.R };

  private const Int32 nColors = 2;
  internal const Int32 nPieces = 6;
  private const Int32 nSymbols = nPieces + nColors;
  private const Int32 nPieceCounterBits = nPieces * nPerNibble;
  //private const UInt64 qPieceCounterBit = 1UL << nPieceCounterBits;

  protected const Byte vCompositionOffset = vP6 + 1;
  protected const Int32 nCompositionOffsetBit = (vCompositionOffset - vP6) * nPerNibble;
  //protected const Int32 nCompositionBits = (vK6 - vCompositionOffset) * nPerNibble;
  //protected const UInt32 uCompositionBit = 1U << nCompositionBits;
  //protected const UInt32 uCompositionMask = uCompositionBit - 1 << nCompositionOffsetBit;
  internal const Int32 nHashPieceBits = (vK6 - vHF) * nPerTwoBits;

  protected const Byte vPieceNull = 1 << 3;
  protected const Byte vPieceMask = vPieceNull - 1;

  protected const Byte vFirst = (Byte)(Piece.P);
  internal const Byte vP6 = (Byte)(Piece.P - vFirst);
  internal const Byte vHF = vP6;                        // For SideFlags within HashPiece
  internal const Byte vN6 = (Byte)(Piece.N - vFirst);
  internal const Byte vB6 = (Byte)(Piece.B - vFirst);
  internal const Byte vR6 = (Byte)(Piece.R - vFirst);
  internal const Byte vQ6 = (Byte)(Piece.Q - vFirst);
  internal const Byte vK6 = (Byte)(Piece.K - vFirst);
  internal const Byte vEP6 = (Byte)(Piece.EP - vFirst);

  protected const Byte vBlack = (Byte)(vK6 + 1);        // For PieceSymbols[]
  protected const Byte vWhite = (Byte)(vBlack + 1);
  #endregion

  #region Square Enum
  public enum Sq : byte {
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8
  }
  #endregion                            // Games Enum

  #region Move Enum
  //
  // Move Bits  0:16
  // ---------------
  //  0:3 FileFrom
  //  3:3 RankFrom
  //  6:3 FileTo
  //  9:3 RankTo
  // 12:3 Promotion [or Limit Move]
  // 15:1 Castles
  //
  // Move Bits 16:16
  // ---------------
  // 16:3 Piece
  // 19:1 Spare1
  // 20:3 Captive
  // 23:1 Spare2
  // 24:4 Annotation [Final, Check, Draw, Draw2]
  // 28:2 Abbreviate [File, Rank]
  // 30:1 Quiescent
  // 31:1 Debug WTM
  //
#if TestFromTo
  protected const Int32 nToBit = 0;                     // Bit 0
  protected const Int32 nFromBit = nToBit + 6;          // Bit 6
  protected const Int32 nPromoteBit = nFromBit + 6;     // Bit 12
#else
  protected const Int32 nFromBit = 0;                   // Bit 0
  protected const Int32 nToBit = nFromBit + 6;          // Bit 6
  protected const Int32 nPromoteBit = nToBit + 6;       // Bit 12 4-bits for promotion and castling
#endif
  protected const Int32 nCastlesBit = nPromoteBit + 3;  // Bit 15 [Chess960]
  protected const Int32 nPieceBit = nCastlesBit + 1;    // Bit 16 4-bits for piece and unused hi-bit
  protected const Int32 nPieceHiBit = nPieceBit + 3;    // Bit 19 unused
  protected const Int32 nCaptiveBit = nPieceHiBit + 1;  // Bit 20 4-bits for captures and unused hi-bit
  protected const Int32 nCaptiveHiBit = nCaptiveBit + 3;// Bit 23 unused
  protected const Int32 nNoteDraw2Bit = 26;
  internal const Int32 nHideFileBit = 28;

  #region Move Masks
  protected const Move MovePawn = (Move)((Byte)Piece.P << nPieceBit);
  protected const Move MoveKnight = (Move)((Byte)Piece.N << nPieceBit);
  protected const Move MoveBishop = (Move)((Byte)Piece.B << nPieceBit);
  protected const Move MoveRook = (Move)((Byte)Piece.R << nPieceBit);
  protected const Move MoveQueen = (Move)((Byte)Piece.Q << nPieceBit);
  protected const Move MoveKing = (Move)((Byte)Piece.K << nPieceBit);

  protected const Move MoveCapture = (Move)((UInt32)Piece.Capture << nCaptiveBit);
  #endregion                            // Move Masks

  //
  // Move Mask Use Cases
  //
  // Bits Mask        Uses
  // ---- ----        ----
  //  16  ShortMask   Keep Castles Bit for Chess960 PACN
  //  19  EqualMask   Keep PieceMask to distinguish NullMove, Undefined, EmptyMove
  //  23  StoreMask   Keep CaptiveMask for position-independent Killer Moves
  //  26  CheckMask   Keep NoteCheck | NoteFinal; Omit NoteDraw | NoteDraw2 in QXP, XP, XPM
  //  32  Move        Used to Search Lines
  //

  //[Flags]
  public enum Move : uint {
    None = 0,
    Castles = 1 << nCastlesBit,         // Bit 15 Lo 16-bits suffice for PACN
    //PieceHi = 1 << nPieceHiBit,       // Bit 19 unused
    CaptiveHi = 1 << nCaptiveHiBit,     // Bit 23 unused
    NoteFinal = CaptiveHi << 1,         // Bit 24 4-bits for annotations
    NoteCheck = NoteFinal << 1,         // Bit 25
    NoteDraw2 = 1U << nNoteDraw2Bit,    // Bit 26
    CheckMask = NoteDraw2 - 1,          // Mask26 Hi 6-bits omitted from [Trans|Quiet]Position.BestMove
    NoteDraw = NoteDraw2 << 1,          // Bit 27
    HideFile = 1U << nHideFileBit,      // Bit 28 Hi 4-bits are for abbreviation and debug
    HideRank = HideFile << 1,           // Bit 29
    Qxnt = HideRank << 1,               // Bit 30 Used by AbbreviateRefresh()
    WTM = Qxnt << 1,                    // Bit 31 Used to test WTM

    HideFrom = HideRank | HideFile,     // Used to abbreviate From square
    FromMask = uSquareMask << nFromBit,
    ToMask = uSquareMask << nToBit,
    FromToMask = ToMask | FromMask,
    PromoteMask = vPieceMask << nPromoteBit,
    PieceMask = vPieceMask << nPieceBit,
    CaptiveMask = vPieceMask << nCaptiveBit,
    ShortMask = Castles | PromoteMask | FromToMask,
    Material = CaptiveMask | PromoteMask,   // To distinguish quiet maneuvers

    //
    // EqualMask includes moving piece; but omit captures
    //
#if DebugSideToMove
    EqualMask = WTM | PieceMask | ShortMask,
#else
    EqualMask = PieceMask | ShortMask,
#endif
    //
    // StoreMask includes CaptiveMask; but omits annotations
    //
    StoreMask = CaptiveMask | EqualMask,

    //
    // EqualMask distinguishes the three Limit Moves:
    //
    NullMove = Piece.P << nPromoteBit,  // Null Move Heuristic
    Undefined = Piece.K << nPromoteBit,
    EmptyMove = Castles | Undefined     // Denotes Final Position in Transposition
  }
  #endregion                            // Move Enum
  #endregion                            // Enumerations

  #region Locales
  public class Locale {
    public String? Symbols;
    public String? Language;
  }

  //
  // Czech, Danish, Dutch, English, Estonian, Finnish, French, German, Hungarian,
  // Icelandic, Italian, Norwegian, Polish, Portuguese, Romanian, Spanish, Swedish
  //
  // The last two Symbols are single character abbreviations for the Black and White colors.
  // The colors Black and White both begin with "F" in Hungarian, so Green and Red are used instead.
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
  #endregion                            // Locales

  #region Plane
  internal const Int32 FilePos = 0, RankPos = 1;

  protected const Plane BIT0 = 1UL;
  protected const Plane BIT7 = BIT0 << nFiles - 1;      // For findEmptyFile()
  internal const Plane BIT32 = BIT0 << 32;
  protected const Plane MASK64 = UInt64.MaxValue;

  private const Byte MASK8 = Byte.MaxValue;

  // The following are used by resetWhite|BlackPawnAtx()
  protected const Plane qpFileA = 0x0101010101010101UL;
  protected const Plane qpFileB = qpFileA << 1;
  protected const Plane qpFileC = qpFileB << 1;
  protected const Plane qpFileD = qpFileC << 1;
  protected const Plane qpFileE = qpFileD << 1;
  protected const Plane qpFileF = qpFileE << 1;
  protected const Plane qpFileG = qpFileF << 1;
  protected const Plane qpFileH = qpFileG << 1;

  // The following are used by Position.white|blackCanPromote()
  protected const Plane qpRank1 = 0xFFUL;
  protected const Plane qpRank2 = qpRank1 << nFiles;
  protected const Plane qpRank3 = qpRank2 << nFiles;
  protected const Plane qpRank4 = qpRank3 << nFiles;
  protected const Plane qpRank5 = qpRank4 << nFiles;
  protected const Plane qpRank6 = qpRank5 << nFiles;
  protected const Plane qpRank7 = qpRank6 << nFiles;
  protected const Plane qpRank8 = qpRank7 << nFiles;
  #endregion                            // Plane
}
