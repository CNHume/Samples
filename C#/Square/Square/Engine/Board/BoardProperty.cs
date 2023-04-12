//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define BuildAtxTo
//#define Magic
//#define TestMagic
//#define InitDeBruijn
//#define DeBruijn                        // DeBruijn vs Mask
//#define FullData                        // Full vs Half
//#define TestZobrist

namespace Engine;

using static Position;
using static Position.PositionSide;

//
// Type Aliases:
//
using Hashcode = UInt64;
using Plane = UInt64;
using Ply = UInt16;

partial class Board {
  protected static String? PieceSymbols;

  #region Constant Fields
  protected static readonly PositionParameter[] Parameter;

  protected static Plane SquareLite;
  protected static Plane SquareDark;

  protected static Plane[] AtxKing;
  protected static Plane[] AtxKnight;

  protected static Plane[][] AtxRank;
  protected static Plane[][] AtxFile;
  protected static Plane[][] AtxA1H8;
  protected static Plane[][] AtxA8H1;

  protected static Byte[] RankOffset;
#if Magic
    protected static Int32[] LoA1H8;
    protected static Int32[] LoA8H1;
    protected static Byte[] MagicA1H8;
    protected static Byte[] MagicA8H1;
    protected static Byte[] MagicFile;
#if TestMagic
    protected static Byte[] StateFile;
    protected static Byte[] StateA1H8;
    protected static Byte[] StateA8H1;
#endif
#else
  protected static Byte[] OffsetOrth;
  protected static Byte[] OffsetDiag;   // Helps build OffsetA1H8 and OffsetA8H1
  protected static Byte[] OffsetA1H8;
  protected static Byte[] OffsetA8H1;

  protected static Plane[] BitRank;
  protected static Plane[] BitFile;
  protected static Plane[] BitA1H8;
  protected static Plane[] BitA8H1;
#endif
#if InitDeBruijn
    protected static Byte[] deBruijnByte;
#if DeBruijn
#if FullData
    protected static Byte[] deBruijnFull;
#else                                   //!FullData
    protected static Byte[] deBruijnHalf;
#endif                                  // FullData
#endif                                  // DeBruijn
#endif                                  // InitDeBruijn
  #region Zobrist Hashing
  private static Random zobristRandom;
  private static Byte[] zobristBuffer;
  private static Hashcode zobristTurn;
  private static Hashcode[] zobristDraw;
  private static Hashcode[] zobristFile;
  private static Hashcode[] zobristRightsBlack;
  private static Hashcode[] zobristRightsWhite;
  private static Hashcode[][] zobristBlack;
  private static Hashcode[][] zobristWhite;
  private static Hashcode[] zobristExcludedFrom;
  private static Hashcode[] zobristExcludedTo;
  private static Hashcode[] zobristExcludedPromotion;
  private static Hashcode zobristExcludedCastles;
#if TestZobrist
    private static List<Hashcode> zobrists;
#endif
  #endregion                          // Zobrist Hashing
  #endregion                            // Constant Fields

  #region Fields
  public TurnFlags FlagsTurn;           //[fturn]Final | InCheck | Illegal | EPLegal
  public DrawFlags FlagsDraw;           //[fdraw]DrawMask | Draw0
  public EvalFlags FlagsEval;           //[feval]OutsideSquare | KBN | KingAlone
  public ModeFlags FlagsMode;           //[fmode]Trace | NullMade | Reduced

  public Ply GamePly;                   // WTM iff IsEven(GamePly)
  public Byte HalfMoveClock;            // 100-Ply Clock for 50-Move Rule
  public Byte? EPTarget;                //[Nullable]

  public Hashcode Hash;                 // Transposition and QuietPosition Hash
  public Hashcode HashPawn;             // PawnPosition Hash

  public Plane Pawn;                    // Piece Types
  public Plane King;
  public Plane Knight;
  public Plane DiagPiece;
  public Plane OrthPiece;
  public Plane RankPiece;               // Piece Union over both sides
#if !Magic
  public Plane FilePiece;               // Piece Rotations
  public Plane A1H8Piece;
  public Plane A8H1Piece;
#endif
  #region Attacks and Control Fields
#if BuildAtxTo
    protected Plane[] AtxTo;
#endif
  protected SByte[] AtxToCount;
  protected Plane AttackedSum;
  protected Plane WhiteControlled;
  protected Plane BlackControlled;
  #endregion                            // Attacks and Control Fields

  #region BoardSide Fields
  public readonly PositionSide[] Side;

  protected PositionSide Friend;        // Friend and Foe as determined by WTM()
  protected PositionSide Foe;
  #endregion                            // BoardSide Fields
  #endregion                            // Fields

  #region Properties
  //
  // State may well be made static.  This minimal overhead leaves
  // open the possibility of analyzing multiple games in parallel.
  //
  internal GameState State { get; set; }

  public Ply SearchPly {
    //
    // toggleWTM() advances GamePly, and therefore also SearchPly
    //
    get { return (Ply)(GamePly - State.MovePly); }
  }

  public Plane Bishop { get { return DiagPiece & ~OrthPiece; } }
  public Plane Queen { get { return DiagPiece & OrthPiece; } }
  public Plane Rook { get { return OrthPiece & ~DiagPiece; } }

  public String? Name { get; set; }
  public Dictionary<String, List<String>?>? Operations { get; set; }
  #endregion
}
