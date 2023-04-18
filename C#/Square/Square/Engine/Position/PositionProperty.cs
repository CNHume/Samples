//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
#define UseMoveSort
#define LazyMoveSort
//#define TestPawnFeatures
//#define TestInitFree
//#define TestInitHelp

namespace Engine;

using HeapSort;                         // For Heap

using MoveOrder;

//
// Type Aliases:
//
using Draft = UInt16;
using Eval = Int16;
using ExtensionCounter = UInt16;
using MoveTypeOrdering = UInt64;
using Plane = UInt64;

partial class Position : Board {
  #region Static Fields
  private static readonly MoveType[] defaultMoveTypes;
  private static readonly MoveTypeOrdering defaultMoveTypeOrdering;

  private static Byte[] importance;

  private static readonly Draft wReducedDraftMin;
  private static readonly Draft wLateDrafthMin;
  private static readonly Draft wLerpDraftMax;
  #endregion                            // Static Fields

  #region Pawn Feature Fields
  public static readonly PawnFeature[] PawnFeatures;
  public static readonly Int32 nFeatureBits;
#if TestInitFree || TestInitHelp
  private static readonly Sq[] testSquares = {
    Sq.a1, Sq.a8, Sq.c2, Sq.c5, Sq.d6, Sq.e4, Sq.f1, Sq.g7, Sq.h8 };
#endif
  #endregion                            // Pawn Feature Fields

  #region Fields
  public Position? Parent;

  private Eval staticDelta;
  private Eval staticTotal;             // For isEndgame()
  private ExtensionCounter extensionCounts;
#if TestPawnFeatures
  public Plane[] FeatureOrth;
#endif
  private Plane[] restricted;
  private Plane pinnedPiece;

  public Move CurrentMove;              // Set by [null|try]Move() prior to calling Board.PlayMove()
  public List<Move> BestMoves;          // This is a line, not a set of alternative moves

  //
  // 16 members of the MoveType Enum, in their order of precedence:
  //
  private MoveTypeOrdering moveTypeOrdering;
  private MoveType[] moveTypes;

  //
  // The following Pawn Captures will precede PseudoPawnAboveCapture:
  //
  public List<Move> PseudoQueenPromotionCapture;
  public List<Move> PseudoUnderPromotionCapture;
  public List<Move> PseudoEPCapture;

  public List<Move> PseudoPawnAboveCapture;
  public List<Move> PseudoPawnBelowCapture;
  public List<Move> PseudoKnightCapture;
  public List<Move> PseudoKingCapture;

  public List<Move> PseudoDiagAboveCapture;
  public List<Move> PseudoDiagBelowCapture;
  public List<Move> PseudoOrthAboveCapture;
  public List<Move> PseudoOrthBelowCapture;

  //
  // The following Pawn Moves will precede PseudoPawnAboveMove:
  //
  public List<Move> PseudoQueenPromotion;
  public List<Move> PseudoUnderPromotion;

  public List<Move> PseudoPawnAboveMove;
  public List<Move> PseudoPawnBelowMove;
  public List<Move> PseudoKnightMove;
  public List<Move> PseudoCastles;      // Castles precede PseudoKingMove
  public List<Move> PseudoKingMove;

  public List<Move> PseudoDiagAboveMove;
  public List<Move> PseudoDiagBelowMove;
  public List<Move> PseudoOrthAboveMove;
  public List<Move> PseudoOrthBelowMove;

  //
  // Swap Move Lists:
  //
  public List<Move> PseudoGoodCaptures;
  public List<Move> PseudoBadCaptures;
  public List<Move> PseudoCaptures;

  //
  // Complete Move List Outputs
  //
  public List<Move> PseudoMoves;
  public List<Move>? SearchMoves;
#if UseMoveSort
  public SortMove[] SortMoves;
#if LazyMoveSort
  public Heap<SortMove> PriorityMove;
#endif
#else
  public List<Move> SiftedMoves;
#endif
  #endregion                            // Fields

  #region Properties
  private Boolean isMovePosition {
    // MovePosition assumed by startTask() and heartbeat()
    get { return ReferenceEquals(this, State.MovePosition); }
  }

  private String debugString {
    get { return ToString(); }
  }
  #endregion                            // Properties
}
