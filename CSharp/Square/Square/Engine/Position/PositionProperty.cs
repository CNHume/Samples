//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define BuildAtxTo
#define UseMoveSort
#define LazyMoveSort
//#define TestPawnFeatures
//#define TestInitFree
//#define TestInitHelp

namespace Engine {
  using HeapSort;                           // for Heap

  using MoveOrder;

  using System;
  using System.Collections.Generic;

  using static MoveOrder.TypedMove;

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
    protected static Byte[] Importance;

    protected static readonly Draft wReducedDraftMin;
    protected static readonly Draft wLateDrafthMin;
    protected static readonly Draft wLerpDraftMax;
    #endregion

    #region Pawn Feature Fields
    public static readonly PawnFeature[] PawnFeatures;
    public static readonly Int32 nFeatureBits;
#if TestInitFree || TestInitHelp
    private static readonly sq[] testSquares = { sq.a1, sq.a8, sq.c2, sq.c5, sq.d6, sq.e4, sq.f1, sq.g7, sq.h8 };
#endif
    #endregion

    #region Virtual Fields
    public Position? Parent;
    protected Eval StaticDelta;
    protected Eval StaticTotal;         // For isEndgame()
    protected ExtensionCounter ExtensionCounts;
#if TestPawnFeatures
    public Plane[] FeatureRect;
#endif
#if BuildAtxTo
    protected Plane[] AtxTo;
#endif
    protected SByte[] ControlTo;
    protected Plane AttackedSum;
    protected Plane WhiteControlled;
    protected Plane BlackControlled;

    protected Plane[] Restricted;
    protected Plane PinnedPiece;

    public Move CurrentMove;            // Set by [null|try]Move() prior to calling Board.playMove()
    public List<Move> BestMoves;        // This is a line, not a set of alternative moves

    //
    // 16 members of the MoveType Enum, in their order of precedence:
    //
    MoveTypeOrdering MoveTypeOrdering;
    public MoveType[] MoveTypes;

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
    public List<Move> PseudoRectAboveCapture;
    public List<Move> PseudoRectBelowCapture;

    //
    // The following Pawn Moves will precede PseudoPawnAboveMove:
    //
    public List<Move> PseudoQueenPromotion;
    public List<Move> PseudoUnderPromotion;

    public List<Move> PseudoPawnAboveMove;
    public List<Move> PseudoPawnBelowMove;
    public List<Move> PseudoKnightMove;
    public List<Move> PseudoCastles;    // Castles precede PseudoKingMove
    public List<Move> PseudoKingMove;

    public List<Move> PseudoDiagAboveMove;
    public List<Move> PseudoDiagBelowMove;
    public List<Move> PseudoRectAboveMove;
    public List<Move> PseudoRectBelowMove;

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
    public List<Move> SearchMoves;
#if UseMoveSort
    public SortMove[] SortMoves;
#if LazyMoveSort
    public Heap<SortMove> PriorityMove;
#endif
#else
    public List<Move> SiftedMoves;
#endif
    #endregion

    #region Properties
    protected Boolean IsMovePosition {
      // MovePosition assumed by startTask() and heartbeat()
      get { return ReferenceEquals(this, State.MovePosition); }
    }

    private String DebugString {
      get { return ToString(); }
    }
    #endregion
  }
}
