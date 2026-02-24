//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
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
//#define DebugDraw2
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

using static System.String;

namespace Engine;

using System.Runtime.CompilerServices;

using MoveOrder;

using Sorters;                         // For Heap

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
        Side[nSide] = new(this, parameter);
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
    PseudoQueenPromotion = [];
    PseudoUnderPromotion = [];
    PseudoQueenPromotionCapture = [];
    PseudoUnderPromotionCapture = [];
    PseudoEPCapture = [];
    PseudoCastles = [];

    PseudoPawnAboveMove = [];
    PseudoPawnBelowMove = [];
    PseudoKingMove = [];
    PseudoKnightMove = [];
    PseudoDiagAboveMove = [];
    PseudoDiagBelowMove = [];
    PseudoOrthAboveMove = [];
    PseudoOrthBelowMove = [];

    PseudoPawnAboveCapture = [];
    PseudoPawnBelowCapture = [];
    PseudoKingCapture = [];
    PseudoKnightCapture = [];
    PseudoDiagAboveCapture = [];
    PseudoDiagBelowCapture = [];
    PseudoOrthAboveCapture = [];
    PseudoOrthBelowCapture = [];

    PseudoGoodCaptures = [];
    PseudoBadCaptures = [];
    PseudoCaptures = [];
    PseudoMoves = [];
#else
    PseudoQueenPromotion = new(2);
    PseudoUnderPromotion = new(2 * 3);
    PseudoQueenPromotionCapture = new(2);
    PseudoUnderPromotionCapture = new(2 * 3);
    PseudoEPCapture = new(2);
    PseudoCastles = new(2);

    PseudoPawnAboveMove = new(8 * 3);
    PseudoPawnBelowMove = new(8 * 3);
    PseudoKingMove = new(8);
    PseudoKnightMove = new(2 * 8);
    PseudoDiagAboveMove = new(2 * 13);
    PseudoDiagBelowMove = new(2 * 13);
    PseudoOrthAboveMove = new(2 * 14);
    PseudoOrthBelowMove = new(2 * 14);

    PseudoPawnAboveCapture = new(8 * 2);
    PseudoPawnBelowCapture = new(8 * 2);
    PseudoKingCapture = new(6);
    PseudoKnightCapture = new(2 * 6);
    PseudoDiagAboveCapture = new(2 * 4);
    PseudoDiagBelowCapture = new(2 * 4);
    PseudoOrthAboveCapture = new(2 * 4);
    PseudoOrthBelowCapture = new(2 * 4);

    PseudoGoodCaptures = new(32);
    PseudoBadCaptures = new(32);
    PseudoCaptures = new(48);
    PseudoMoves = new(128);
#endif
#if UseMoveSort
    SortMoves = new SortMove[512];
#if LazyMoveSort
    PriorityMove = new(SortMoves, 0);
#endif
#else
    SiftedMoves = new(128);
#endif
  }

  internal List<Move> newSearchMoves() {
    return SearchMoves = new(4);
  }
  #endregion                            // Move List Initialization

  #region Find Methods
  public Position? FirstNamedParent(Position? parent) {
    var position = this;
    while (position is not null &&
           !ReferenceEquals(position, parent) &&
           IsNullOrEmpty(position.Name))
      position = position.Parent;

    return position;
  }
  #endregion                            // Find Methods

  #region Draw By Repetition
#if DebugDraw2
  private Boolean validateDraw2() {
    var bDraw2 = FlagsDraw.Has(DrawFlags.Draw3 | DrawFlags.Draw2);
    if (bDraw2) {
      var nCount = 1;

      for (var position = Parent;
           position is not null;
           position = position.Parent) {
        if (Equals(position)) nCount++;
      }

      var bValid = nCount > 1 ? bDraw2 : !bDraw2;
      if (!bValid)
        DisplayCurrent(nameof(validateDraw2));

      return bValid;
    }
    else
      return true;
  }
#endif                                  // DebugDraw2
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Position? findRepetition1(Boolean bLookupCycle = false) {
    if (IsDraw0()) return default;

    if (bLookupCycle)
      GameState.AtomicIncrement(ref State.LookupCycleSearches);
    else
      GameState.AtomicIncrement(ref State.RepetitionSearches);

    //
    // Each Parent position is examined back to the initial position,
    // not just to State.MovePosition where the current search began.
    //
    for (var position = Parent;
         position is not null;
         position = position.Parent) {
      if (bLookupCycle)
        GameState.AtomicIncrement(ref State.LookupCyclePlies);
      else
        GameState.AtomicIncrement(ref State.RepetitionPlies);

      //
      //[Note]IsDraw0() must be checked for both sides.
      //
      if (Equals(position))
        return position;
      else if (position.IsDraw0())
        break;                          // End of Repetition Cycle
    }

    return default;
  }

  //
  // Identify Draw by 3-Fold Repetition
  //
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void findRepetition() {
    ClrRepetition();
    var position = findRepetition1();
    if (position is not null) {
      if (IsNullMade())
        //
        // Null Moves do not count as repetition of the position; but the
        // Draw3 and Draw2 flags are copied to expedite subsequent search:
        //
        FlagsDraw |= position.fdraw();
      else
        SetRepetition(position.fdraw() != 0);
#if DebugDraw2
      validateDraw2();
#endif
    }
  }
  #endregion                            // Draw By Repetition
}
