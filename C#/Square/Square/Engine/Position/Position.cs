﻿//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2010-07-17 CNHume]Created Class
//
// Notes:
//
// The following features seem most relevant to performance:
//
// Alpha-Beta
// PV Search
// Quiescence
// Null Move Pruning
// Futility & Delta Pruning
// *XP and ID
// Killer Heuristic
// generators & pinnedPiece
// C++ vs C# around 2.5 times faster
// Rotated BB not Magic
//
// Conditionals:
//
#define InheritMoveTypes
//#define LinkedTranspositions
//#define TimeAtxFromUpdates
#define UseMoveSort                     // 12.24% Faster without UseMoveSort
#define LazyMoveSort
//#define ShowCornerCP
//#define TestCorner
#define DebugInit
//#define DebugStand
//#define TestBest
//#define TestImportance
//#define TestPawnFeatures
#define InitFree                        //[Default]
//#define InitHelp                        //[Test]
//#define TestInitFree
//#define TestInitHelp

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Engine;

using HeapSort;                         // For Heap

using MoveOrder;

using static Logging.Logger;

//
// Type Aliases:
//
using Bval = Int16;
using Depth = UInt16;
using Plane = UInt64;
using PlyDepth = Byte;

[DebuggerDisplay("{debugString}")]
partial class Position : Board {
  #region Constants
  protected const PlyDepth vStartDepthDefault = 6;
  protected const Depth wSwapDepthMax = 1;
  // Carlsen v Topalov 2015-06-16 13-ply 1 PVSMin searched 0.25 Gnode in 4:24 vs 2 PVSMin searched 1.56 Gnode in 25:38
  internal const Depth wNullPlyMin = 1;         // 1 Added for Mavo Nice Mate1 #6, 3 added for Xiong v Nakamura
  internal const Depth wPVSDepthMin = 1;        // 1
  internal const Depth wSmartPlyMax = 4;        // 4
  internal const Depth wSmartDepthMin = 6;      // 6
  internal const Depth wVerifyDepthMin = 12;    // 12
  internal const Depth wLerpDepthMax = 14;      // 11
  internal const Depth wReducedDepthMin = 1;    // 1
  internal const Depth wOccamDepthMax = 1;      // 2
  internal const Depth wThreatDepthMin = 3;     // 3
  internal const Depth wSingularDepthMin = 4;   // 6
  internal const Depth wLateDepthMin = 2;
  protected const Int32 nFirstCapacity = 6;     // Transposition moveFound from prior Depth plus 2 Killers

  private const Boolean bWhiteMovesFirst = true;

  private static readonly Byte[] quartance = {
    8,  4,  6,  8,                      // a8 b8 c8 d8
    8,  9,  8,  9,                      // a7 b7 c7 d7
    4,  6,  9, 10,                      // a6 b6 c6 d6
    6,  8, 12, 15 };                    // a5 b5 c5 d5
  #endregion                            // Constants

  #region Constructors
  static Position() {
#if DebugInit
    LogLine("Initializing Position...");
#endif
    #region Assertions
    Trace.Assert(EvalUndefined == Bval.MinValue, "EvalUndefined != Bval.MinValue");

    Trace.Assert(PieceWeight[vP6] == mPawnWeight, "PieceWeight[vP6] != PawnWeight");
    Trace.Assert(PieceWeight[vN6] == mKnightWeight, "PieceWeight[vN6] != KnightWeight");
    Trace.Assert(PieceWeight[vB6] == mBishopWeight, "PieceWeight[vB6] != BishopWeight");
    Trace.Assert(PieceWeight[vR6] == mRookWeight, "PieceWeight[vR6] != RookWeight");
    Trace.Assert(PieceWeight[vQ6] == mQueenWeight, "PieceWeight[vQ6] != QueenWeight");
    #endregion

    #region Read Only Assignments
    wReducedDraftMin = draft(wReducedDepthMin);
    wLateDrafthMin = draft(wLateDepthMin);
    wLerpDraftMax = draft(wLerpDepthMax);

    PawnFeatures = (PawnFeature[])Enum.GetValues(typeof(PawnFeature));
    nFeatureBits = PawnFeatures.Length * nPerNibble;
    #endregion                          // Read Only Assignments

    initParameters();
#if TestCorner
    printMapping("edgeDistance()", edgeDistance);
    printMapping("liteCornerDistance()", liteCornerDistance);
    printMapping("darkCornerDistance()", darkCornerDistance);
    printMapping("liteCornerDefence()", liteCornerDefence);
    printMapping("darkCornerDefence()", darkCornerDefence);
#endif
#if ShowCornerCP
    printMapping("liteCornerCP()", liteCornerCP);
    printMapping("darkCornerCP()", darkCornerCP);
#endif

    //
    // Initialize Default MoveType Ordering:
    //
    defaultMoveTypes = (MoveType[])Enum.GetValues(typeof(MoveType));
    defaultMoveTypeOrdering = compressMoveTypes(defaultMoveTypes);
  }

  public Position(GameState state) : base(state) {
    ensureSides();
#if TestPawnFeatures
    newFeatures();
#endif
    newBestMoves(GameState.wDepthMax);
    newRestricted();
    newMoveTypes();
    newPseudoMoves();
  }

  private void ensureSides() {
    foreach (var parameter in Parameter) {
      var nSide = (Int32)parameter.SideName;
      if (Side[nSide] == null)
        Side[nSide] = new PositionSide(this, parameter);
    }
  }

  //
  // Used by resetMove()
  //
  public void Copy(Position position) {
    //[Safe]ensureSides();
    base.Copy(position);
    staticDelta = position.staticDelta;
    staticTotal = position.staticTotal;
    extensionCounts = position.extensionCounts;
#if InheritMoveTypes
    moveTypeOrdering = position.moveTypeOrdering;
#endif
  }

  //
  // Deep Copy:
  //
  public void Clone(Position position) {
    Copy(position);
    CurrentMove = position.CurrentMove;
    Name = position.Name;
  }
  #endregion                            // Constructors

  #region Static Intialization
  //
  //[Note]Apart from allocation time, static initialization for the
  // Position class, and its Board sub-class, require less than one
  // ms to complete.
  //
  [MemberNotNull(nameof(importance))]
  private static void initParameters() {
    loadOutsideSquare();
    loadFreeHelp();

    newSquareImportance();
    loadSquareImportance();
  }

  [MemberNotNull(nameof(importance))]
  private static void newSquareImportance() {
    importance = new Byte[nSquares];
  }

  private static void loadSquareImportance() {
    var n4 = 0;                         // quartance[] defined over 16 squares of a quarter-board
    for (var y = 0; y < nRanks / 2; y++) {
      var yInverse = InvertRank(y);
      for (var x = 0; x < nFiles / 2; x++, n4++) {
        var xInverse = InvertFile(x);

        importance[sqr(x, y)] = quartance[n4];
        importance[sqr(xInverse, y)] = quartance[n4];
        importance[sqr(x, yInverse)] = quartance[n4];
        importance[sqr(xInverse, yInverse)] = quartance[n4];
      }
    }
#if TestImportance
    printSquares("Importance", Importance);
#endif
  }
  #endregion                            // Static Initialization

  #region Instance Intialization
#if TestPawnFeatures
  [MemberNotNull(nameof(FeatureOrth))]
  private void newFeatures() {
    FeatureOrth = new Plane[PawnFeatures.Length * 2];
  }
#endif
  [MemberNotNull(nameof(BestMoves))]
  private void newBestMoves(Int32 nCapacity) {
#if TestBest
    BestMoves = new List<BestMove>(nCapacity);
#else
    BestMoves = new List<Move>(nCapacity);
#endif
  }

  [MemberNotNull(nameof(restricted))]
  private void newRestricted() {
    restricted = new Plane[nSquares];
  }
  #endregion                            // Instance Intialization

  #region Move List Initialization
  [MemberNotNull(nameof(moveTypes))]
  private void newMoveTypes() {
    moveTypes = new MoveType[defaultMoveTypes.Length];
  }

  [MemberNotNull(
#if UseMoveSort
    nameof(SortMoves),
#if LazyMoveSort
    nameof(PriorityMove),
#endif
#else
    nameof(SiftedMoves),
#endif
    nameof(PseudoQueenPromotionCapture),
    nameof(PseudoUnderPromotionCapture),
    nameof(PseudoEPCapture),
    nameof(PseudoPawnAboveCapture),
    nameof(PseudoPawnBelowCapture),
    nameof(PseudoKnightCapture),
    nameof(PseudoKingCapture),
    nameof(PseudoDiagAboveCapture),
    nameof(PseudoDiagBelowCapture),
    nameof(PseudoOrthAboveCapture),
    nameof(PseudoOrthBelowCapture),
    nameof(PseudoQueenPromotion),
    nameof(PseudoUnderPromotion),
    nameof(PseudoPawnAboveMove),
    nameof(PseudoPawnBelowMove),
    nameof(PseudoKnightMove),
    nameof(PseudoCastles),
    nameof(PseudoKingMove),
    nameof(PseudoDiagAboveMove),
    nameof(PseudoDiagBelowMove),
    nameof(PseudoOrthAboveMove),
    nameof(PseudoOrthBelowMove),
    nameof(PseudoGoodCaptures),
    nameof(PseudoBadCaptures),
    nameof(PseudoCaptures),
    nameof(PseudoMoves)
    )]
  private void newPseudoMoves() {
#if NoCapacity
    PseudoQueenPromotion = new List<Move>();
    PseudoUnderPromotion = new List<Move>();
    PseudoQueenPromotionCapture = new List<Move>();
    PseudoUnderPromotionCapture = new List<Move>();
    PseudoEPCapture = new List<Move>();
    PseudoCastles = new List<Move>();

    PseudoPawnAboveMove = new List<Move>();
    PseudoPawnBelowMove = new List<Move>();
    PseudoKingMove = new List<Move>();
    PseudoKnightMove = new List<Move>();
    PseudoDiagAboveMove = new List<Move>();
    PseudoDiagBelowMove = new List<Move>();
    PseudoOrthAboveMove = new List<Move>();
    PseudoOrthBelowMove = new List<Move>();

    PseudoPawnAboveCapture = new List<Move>();
    PseudoPawnBelowCapture = new List<Move>();
    PseudoKingCapture = new List<Move>();
    PseudoKnightCapture = new List<Move>();
    PseudoDiagAboveCapture = new List<Move>();
    PseudoDiagBelowCapture = new List<Move>();
    PseudoOrthAboveCapture = new List<Move>();
    PseudoOrthBelowCapture = new List<Move>();

    PseudoGoodCaptures = new List<Move>();
    PseudoBadCaptures = new List<Move>();
    PseudoCaptures = new List<Move>();
    PseudoMoves = new List<Move>();
#else
    PseudoQueenPromotion = new List<Move>(2);
    PseudoUnderPromotion = new List<Move>(2 * 3);
    PseudoQueenPromotionCapture = new List<Move>(2);
    PseudoUnderPromotionCapture = new List<Move>(2 * 3);
    PseudoEPCapture = new List<Move>(2);
    PseudoCastles = new List<Move>(2);

    PseudoPawnAboveMove = new List<Move>(8 * 3);
    PseudoPawnBelowMove = new List<Move>(8 * 3);
    PseudoKingMove = new List<Move>(8);
    PseudoKnightMove = new List<Move>(2 * 8);
    PseudoDiagAboveMove = new List<Move>(2 * 13);
    PseudoDiagBelowMove = new List<Move>(2 * 13);
    PseudoOrthAboveMove = new List<Move>(2 * 14);
    PseudoOrthBelowMove = new List<Move>(2 * 14);

    PseudoPawnAboveCapture = new List<Move>(8 * 2);
    PseudoPawnBelowCapture = new List<Move>(8 * 2);
    PseudoKingCapture = new List<Move>(6);
    PseudoKnightCapture = new List<Move>(2 * 6);
    PseudoDiagAboveCapture = new List<Move>(2 * 4);
    PseudoDiagBelowCapture = new List<Move>(2 * 4);
    PseudoOrthAboveCapture = new List<Move>(2 * 4);
    PseudoOrthBelowCapture = new List<Move>(2 * 4);

    PseudoGoodCaptures = new List<Move>(32);
    PseudoBadCaptures = new List<Move>(32);
    PseudoCaptures = new List<Move>(48);
    PseudoMoves = new List<Move>(128);
#endif
#if UseMoveSort
    SortMoves = new SortMove[512];
#if LazyMoveSort
    PriorityMove = new Heap<SortMove>(SortMoves, 0);
#endif
#else
    SiftedMoves = new List<Move>(128);
#endif
  }

  internal List<Move> newSearchMoves() {
    return SearchMoves = new List<Move>(4);
  }
  #endregion                            // Move List Initialization
}
