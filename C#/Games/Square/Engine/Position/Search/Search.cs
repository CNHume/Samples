﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2010-07-17 CNHume]Created Class
//
// Conditionals:
//
#define Quiescence
//#define DebugMove
#define AddBestMoves
//#define DebugMoveIsLegal
#define DebugSideToMove
#define DebugSearchMoves
//#define DebugPseudoMoves
//#define TraceVal
//#define DebugCandidateMoves
//#define DebugGoodMoves
//#define DebugNextMove
#define LazyMoveSort
//#define LateMoveReduction
#define MateThreat
#define SingularExtension
//#define DebugSingular
//#define OccamReduced
//#define SwapOn
#define CountPVDoubles
//#define DeepSingular
//#define DeepThreat
//#define DeepNull
//#define TestLerp
#define GetSmart
//#define VerifyUpper
//#define DebugBest
//#define TestBest

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;                      // For StringBuilder

using static System.Math;

namespace Engine;

using MoveOrder;

using static GameState;
using static Logging.Logger;
using static MoveOrder.BestMoveEnum;

//
// Type Aliases:
//
using Depth = UInt16;
using Draft = UInt16;
using Eval = Int16;

partial class Position : Board {
  #region Constants
  protected const Boolean Swaps = true;

  //
  // Futility Prining is one type of Forward Pruning.
  // A second weight applies to "Pre-Frontier Nodes".
  //
  // See https://www.chessprogramming.org/Futility_Pruning
  //
  internal static Eval[] FutilityMargin = {
    mBishopWeight, mRookWeight };
  #endregion                            // Constants

  #region Search Methods
  private Eval search(Draft wDraft, Eval mAlpha, Eval mBeta,
                      Move moveExcluded = Move.Undefined) {
    const String methodName = nameof(search);
    BestMoves.Clear();                  //[Required]per iteration
    var moveBest = Move.Undefined;      //[Init]

    #region Test for Draw
    //[Note]IsDraw50() will be called after tryMove() below,
    // to detect Checkmate.
    if (IsDraw()) {
      State.IncEvalType(EvalType.Exact);
      return eval();
    }
    #endregion

    #region Test for entry into Quiet Search
    var bInCheck = InCheck();
    if (bInCheck) {                   // Check Extension
      if (extended(ref wDraft, SearchExtensions.Check))
        AtomicIncrement(ref State.CheckExtCount);
    }

    //
    // Depth is tested here, rather than in the PVS recursion case,
    // because search also recurses from the heuristic cases below.
    //
    var wDepth = depth(wDraft);         // InCheck Adjusted Depth
    if (wDepth < 1) {
#if Quiescence
      var mQuietValue = quiet(mAlpha, mBeta);
#else
      var mQuietValue = boundValue(eval(), mAlpha, mBeta);
#endif
#if DebugBest
      // BestMoves should not be empty here
      var emptyMessage1 = $"BestMoves.Count = {BestMoves.Count} Empty1 at wDepth = {wDepth} [{methodName}]";
      Debug.Assert(BestMoves.Count > 0, emptyMessage1);
#endif
      return mQuietValue;
    }
    #endregion                          // Test for entry into Quiet Search

    #region Transposition Table Lookup
#if TraceVal
    if (IsTrace())                      //[Note]CurrentMove Undefined
      Display($"{methodName}(Depth = {wDepth})");
#endif
    Debug.Assert(mAlpha < mBeta, "Alpha must be less than Beta");

    List<GoodMove> goodMoves = new(nFirstCapacity);
    var bFoundValue = probeXP(
      wDepth, mAlpha, mBeta, moveExcluded, goodMoves,
      out Move moveFound, out Eval mValueFound, out EvalType etFound);
    // Variations updated iff bFoundValue
    if (bFoundValue) {
      // moveFound not always defined for EvalType.Upper [Fail Low]
      if (IsDefinite(moveFound)) {      //[Safe]Also prevent unexpected EmptyMove
#if DebugMove
        unpackMove1(
          moveFound, out Sq sqFrom, out Sq sqTo,
          out Piece piece, out Piece promotion, out Boolean bCapture);
        //unpackMove2(
        //  moveFound, out Sq sqFrom, out Sq sqTo,
        //  out Piece piece, out Piece promotion, out Piece capture,
        //  out Boolean bCastles, out Boolean bCapture);
#endif
#if DebugBest
        // BestMoves will be empty here
        var notEmptyMessage = $"BestMoves.Count = {BestMoves.Count} Not Empty at wDepth = {wDepth} [{methodName}]";
        Debug.Assert(BestMoves.Count == 0, notEmptyMessage);
#endif
        if (isMovePosition()) {
#if TestBest
          addPV(mAlpha, mValueFound, moveFound, BestMoves, wDepth);
#else
          addPV(mAlpha, mValueFound, moveFound, BestMoves);
#endif
        }
        else {
          // Safe to update BestMoves now
          //[Conditional]
          verifyMoveIsLegal(moveFound, methodName);
#if AddBestMoves
          //[Bug]Also cf. quiet()
          addBest(moveFound, SearchProbe);
#endif                                  // AddBestMoves
        }
      }
#if DebugBest
      // BestMoves should not be empty here
      var emptyMessage2 = $"BestMoves.Count = {BestMoves.Count} Empty2 at wDepth = {wDepth} [{methodName}]";
      Debug.Assert(BestMoves.Count > 0, emptyMessage2);
#endif
      return mValueFound;
    }
    #endregion                          // Transposition Table Lookup

    #region Heuristic Tests
    var bReduced = FlagsMode.Has(ModeFlags.Reduced);
    if (bReduced)
      AtomicIncrement(ref State.ReducedTotal);

    var bTestSingular = false;
    var bPruneQuiet = false;
    var bZWS = FlagsMode.Has(ModeFlags.ZWS);
    var bMoveExcluded = IsDefined(moveExcluded);
    var wReducedDraft = reduceShallow(wDraft);  // Draft for Heuristic Searches

    if (!bInCheck) {
      var mStand = standpatval(mValueFound, etFound);

      if (bZWS) {
        var bPrune = prune(
          wDraft, wDepth, mAlpha, mBeta, mValueFound, etFound, bMoveExcluded,
          out Eval mPrunedValue);

        if (bPrune)
          return mPrunedValue;
      }

      if (!bReduced) {
        //[Conditional]
        threat(ref wDraft, ref wReducedDraft, ref wDepth);
      }

      // Determine whether Futility Pruning should be performed at Frontier Nodes:
      if (State.IsFutility && 0 < wDepth) {
        var bNonMateWindow = -MateMin < mAlpha && mBeta < MateMin;
        if (bNonMateWindow && !FlagsEval.Has(EvalFlags.EndGame)) {
          var nMargin = wDepth - 1;
          if (nMargin < FutilityMargin.Length)
            bPruneQuiet = mStand + FutilityMargin[nMargin] <= mAlpha;
        }
      }                                 // Futility Pruning
#if SingularExtension
      bTestSingular = wSingularDepthMin <= wDepth &&
                      EvalUndefined < mValueFound &&
                      //[Test]More inclusion performs better than less!
                      Abs(mValueFound) < mQueenWeight &&
                      IsDefined(moveFound) && !(bReduced || bMoveExcluded) &&
                      canExtend(vSingular);     //[Ergo]child.canExtend(vSingular) below
#endif
    }                                   //!bInCheck
    #endregion                          // Heuristic Tests

    #region Generate Moves
    var moves = PseudoMoves;
    //
    // The "go searchmoves" UCI command sets SearchMoves
    // to restrict the set of candidate moves at the Root:
    //
    if (SearchMoves != null && SearchMoves.Count > 0) {
      moves.Clear();
      moves.AddRange(SearchMoves);
#if DebugSearchMoves
      var sb = new StringBuilder("SearchMoves:")
        .MapMoves(Extension.AppendPACN, moves, Side, State.IsChess960);
      LogLine(sb.ToString());
#endif
    }
    else {
#if SwapOn
      var bSwap = wDepth < wSwapDepthMax;
      generate(moves, bSwap);
#else
      generate(moves, !Swaps);
#endif
      //[Timer]timeGenerate(moves, !Swaps);
#if DebugPseudoMoves
      DisplayCurrent($"{methodName}(Depth = {wDepth})");
      var sb = new StringBuilder("PseudoMoves:")
        .MapMoves(Extensions.AppendPACN, moves, State.IsChess960);
        .FlushLine();
#endif                                  // DebugPseudoMoves
    }
    #endregion

    #region Move Loop Initializaton
    var bTrace = IsTrace();
    var et = EvalType.Upper;            //[Init]Fail Low is the default assumption
    var mBest = EvalUndefined;          // Value for the strongest variation
                                        //[Test]mBest = MinusInfinity;
    var mBest2 = mBest;                 // Value from the least best MultiPV
    var mValue = EvalUndefined;
    #endregion

    var child = Push();                 // Push Position to make the moves
    try {
      var bestLine = child.BestMoves;
      #region Move Sort
      //
      // Try to consider the best moves first!
      //
      addGoodMoves(goodMoves, wDepth, mAlpha, mBeta, moveExcluded);
      var nEarly = sortMoves(moves, goodMoves, wDepth);
#if DebugGoodMoves
      if (goodMoves.Count > 0) {
        var sb = new StringBuilder("goodMoves:")
          .MapMoves(Extensions.AppendPACN, goodMoves, State.IsChess960);
          .FlushLine();
      }
#endif
#if DebugCandidateMoves
      //
      // Take care not to perturb the moves enumeration:
      //
#if LazyMoveSort
      var pm2 = (Heap<SortMove>)PriorityMove.Clone();
#else
      List<Move> pm2 = new(moves);
#endif                                  // LazyMoveSort
      var sb2 = new StringBuilder("Candidate Moves:");
#if LazyMoveSort
      sb2.MapMoves(Extensions.AppendPACN, from sm2 in pm2 select sm2.Move, State.IsChess960);
#else
      sb2.MapMoves(Extensions.AppendPACN, from move2 in pm2 select move2, State.IsChess960);
#endif
      sb2.FlushLine();
#endif                                  // DebugCandidateMoves
      #endregion                        // Move Sort

      #region Move Loop
      var nTried = 0;
      var uLegalMoves = 0U;
      var bTryZWS = false;              //[Note]Full PVS requires Raised Alpha
      var uRaisedAlpha = 0U;
#if LazyMoveSort
      // The Heap Enumerator uses Remove() to obtain the "minimum" SortMove,
      // i.e., the one with the best Depth then best Score then least Index:
      foreach (var sm in PriorityMove) {
        var move = sm.Move;
#else                                  //!LazyMoveSort
      for (var nMoveIndex = 0; nMoveIndex < moves.Count; nMoveIndex++) {
        var move = SortMoves[nMoveIndex].Move;
#endif                                  // LazyMoveSort
        #region Make Move
#if DebugMove
        unpackMove1(
          move, out Sq sqFrom, out Sq sqTo,
          out Piece piece, out Piece promotion, out Boolean bCapture);
        //unpackMove2(
        //  move, out Sq sqFrom, out Sq sqTo,
        //  out Piece piece, out Piece promotion, out Piece capture,
        //  out Boolean bCastles, out Boolean bCapture);
#endif
        verifySideToMove(move, methodName);     //[Conditional]
#if DebugNextMove
        var sb = new StringBuilder("Next Move =")
          .AppendPACN(move, State.IsChess960);
        LogLine(sb.ToString());
#endif
        var wDraft1 = nextDraft(wDraft);
        var bEarly = nTried++ < nEarly;

        if (EqualMoves(move, moveExcluded) || !child.tryMove(ref move))
          continue;                     //[Note]Excluding moves may result in a Game Leaf

        uLegalMoves++;
        #endregion                      // Make Move

        #region Test for 50-Move Rule
        if (child.IsDraw50()) {
          mBest = contempt();           //[ToDo]Referencing child.IsDraw50()

          if (mAlpha < mBest) {
            mAlpha = mBest;
            et = EvalType.Exact;
          }

          addBest(move, SearchDraw, bestLine);
          goto exit;                    // Draw50 Dynamic Game Leaf
        }
        #endregion                      // Test for 50-Move Rule

        mValue = updateBest(
          child, wDepth, wDraft, ref wDraft1, wReducedDraft,
          uRaisedAlpha, mAlpha, mBeta, ref mBest, ref mBest2,
          move, moveFound, mValueFound,
          bPruneQuiet, bTestSingular, bEarly, bTryZWS);

        #region Update Best Move
        //
        //[Note]Annotation is made from the child position resulting from each move.
        //
        var moveNoted = child.annotateFinal(move);

        if (mBest < mValue) {
          mBest = mValue;
          if (mAlpha < mBest) {
            moveBest = moveNoted;
            //[Old]
            addBest(moveBest, SearchUpdate, bestLine);
          }
        }
        #endregion                      // Update Best Move

        if (!isMovePosition())
          mBest2 = mBest;
        else if (mAlpha < mValue) {
          //[<=]See Johannessen v Fischer #8 w Aspiration
          //
          //[Note]mAlpha may be less than mBest here: to admit weaker MultiPV lines.
          //
#if TestBest
          mBest2 = addPV(mAlpha, mValue, moveNoted, bestLine, wDepth);
#else
          mBest2 = addPV(mAlpha, mValue, moveNoted, bestLine);
#endif
        }

        #region Test for Cutoff
        if (mAlpha < mBest2) {
          traceVal("Raised Alpha", mBest2);     //[Conditional]
          uRaisedAlpha++;
          mAlpha = mBest2;

          if (mBeta <= mAlpha) {
#if TraceVal
            if (bTrace)
              LogLine("Trace: Failed High");
#endif
            et = EvalType.Lower;        // Cutoff Reached: Ignore further moves and Fail High
            rewardMove(move, wDepth, mValue, et, moveExcluded);
            goto exit;
          }

          et = EvalType.Exact;

          if (wPVSDepthMin <= wDepth && !bZWS)
            bTryZWS = true;             // Raised Alpha
        }
        #endregion
      }                                 //[Next]Pseudo Move
      #endregion

      if (uLegalMoves == 0) {           // No Move Found
        SetFinal();                     // Mark Game Leaf
        mBest = finalValue();
      }

      traceVal("Failed Low", mBest);    //[Conditional]
    }
    finally {
      Pop(ref child);                   // Pop Position used for this Ply
    }

  exit:
#if VerifyUpper
    var bUpper = et == EvalType.Upper;
    var bIndefinite = IsIndefinite(moveBest);
    if (bUpper != bIndefinite) {
      Trace.Assert(bUpper == bIndefinite, "bUpper != bIndefinite");
    }
#endif
#if DebugBest
    // BestMoves should not be empty here
    var emptyMessage3 = $"BestMoves.Count = {BestMoves.Count} Empty3 at wDepth = {wDepth} [{methodName}]";
    Debug.Assert(BestMoves.Count > 0, emptyMessage3);
#endif
    return storeXP(wDepth, mBest, et, moveBest, moveExcluded);
  }

  [Conditional("AddBestMoves")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
#if TestBest
  private void addBest(Move move, BestMoveEnum be = default, List<BestMove>? bestLine = default) {
#else
  private void addBest(Move move, BestMoveEnum be = default, List<Move>? bestLine = default) {
#endif
    const String methodName = nameof(addBest);
    BestMoves.Clear();
    if (IsIndefinite(move)) {
      var message = $"Indefinite {move} move [{methodName}]";
      Debug.Assert(IsDefinite(move), message);
    }
    else {
#if TestBest
      BestMove bestMove = new(move, be, this);
      BestMoves.Add(bestMove);
#else
      BestMoves.Add(move);
#endif
      if (bestLine is not null)
        BestMoves.AddRange(bestLine);
    }
  }

  private Eval updateBest(
    Position child, Depth wDepth, Draft wDraft, ref Draft wDraft1, Draft wReducedDraft,
    UInt32 uRaisedAlpha, Eval mAlpha, Eval mBeta, ref Eval mBest, ref Eval mBest2,
    Move move, Move moveFound, Eval mValueFound,
    Boolean bPruneQuiet, Boolean bTestSingular, Boolean bEarly, Boolean bTryZWS) {
    var mValue = EvalUndefined;

    #region Futility Pruning and LMR
    var bNonChecking = !child.InCheck();
    var bNonMaterial = !move.Has(Move.Material);
    var bQuietMove = bNonChecking && bNonMaterial;

    if (EvalUndefined < mBest && bQuietMove) {
      //
      if (bPruneQuiet) {
        traceVal("Futility Prune", mBest);      //[Conditional]
        AtomicIncrement(ref State.FutilePruneTotal);
        mValue = (Eval)(-child.eval());
        //[Test](Eval)(-child.quiet((Eval)(-mBeta), (Eval)(-mAlpha)));
        goto updateBest;
      }
#if LateMoveReduction
      //
      // Late Move Reduction [LMR]:
      //
      if (!bEarly && wLateDrafthMin <= wDraft) {
        var wLMRDraft = (Draft)(wDraft1 - extensionDraft(vLate));
        var safeMode = child.FlagsMode;
        child.FlagsMode |= ModeFlags.Reduced;
        mValue = (Eval)(-child.search(wLMRDraft, (Eval)(-mBeta), (Eval)(-mAlpha)));
        child.FlagsMode = safeMode;
        if (mValue <= mAlpha)
          goto updateBest;
      }
#endif
    }
    #endregion                          // Futility Pruning and LMR

    #region Singular Extension
#if SingularExtension
    if (bTestSingular && EqualMoves(move, moveFound)) {
#if DeepSingular
      var wSingularDraft = reduceDeep(wDraft);
#else
      var wSingularDraft = wReducedDraft;
#endif
      if (singular(wSingularDraft, mValueFound, move)) {
#if DebugSingular
        child.DisplayCurrent($"Singular Extension at Depth = {wDepth}");
#endif
        //
        //[Note]The Singular Extension should apply to this child only:
        // So, wDraft1 is restored at the beginning of every iteration.
        //
        child.incExtension(ref wDraft1, vSingular);
        AtomicIncrement(ref State.SingularExtCount);
      }
    }
#endif
    #endregion                          // Singular Extension

#if GetSmart
    //var bCastles = IsCastles(move);
    var bReduce = bQuietMove && !bEarly && (Piece)Moved(move) != Piece.K;

    //
    // Houdart suggested depth reduction for Smart Fail High.
    // Hyatt objected that this would return prior XP values.
    //
    var bSmart =
      bReduce &&
      uRaisedAlpha > 1 &&
      wSmartDepthMin <= wDepth &&
      SearchPly < wSmartPlyMax;

    var wReduced = bSmart ? nextDraft(wDraft1) : wDraft1;
#else                                   //!GetSmart
    var wReduced = wDraft1;
#endif                                  // GetSmart
    mValue = child.pvs(wDraft1, wReduced, mBest2, mAlpha, mBeta, bTryZWS);

  updateBest:
    return mValue;
  }

  #region PVS Method
  //
  // PVS can reduce the # of moves searched by one third.
  //
  //[Note]Principal Variation Search (PVS) aka NegaScout aka Zero/Null/Minimal Window Search
  // was developed by multiple researchers.  Robert Hyatt explored the idea as early as 1978.
  // Judea Pearl described an enhancement to Alpha-Beta, known as Scout, in 1980.  This gave
  // rise to more complete development by Fishburn, Finkel, Marsland, Campbell and Reinefeld.
  //
  // See https://www.chessprogramming.org/Principal_Variation_Search
  //
  // The PVS implementation below was gleaned from various articles; and is quite similar to
  // that shown in Figure 5: Minimal Window Principal Variation Search of
  // "A Review of Game-Tree Pruning" by Tony A. Marsland, University of Alberta, Edmonton CA
  // at http://www.cs.unm.edu/~aaron/downloads/qianreview.pdf
  // Prepared for the ICCA Journal June 22, 2001.
  //
  // It is also quite similar to Figure 2.7: Pseudocode for The NegaScout Algorithm
  // taken from http://www.cs.unm.edu/~aaron/downloads/qian_search.pdf
  //
  // In both cases: score==mBest
  // In the latter case: n==mAlpha1, cur==mValue
  //
  // Node Type Nomenclature is surveyed at
  // https://www.chessprogramming.org/Node_Types
  //
  // Knuth+ Marsland+ Eval    Fail    Search
  // Moore  Popowich  Type    Type    Window
  // -----  --------  -----   ----    ------
  //   1      PV      Exact           Alpha < Value < Beta
  //   2      Cut     Lower   High    Beta <= Value
  //   3      All     Upper   Low     Value <= Alpha
  //
  // See "The DTS high-performance parallel tree search algorithm" at
  // http://www.craftychess.com/hyatt/search.html
  //
  // All Nodes [the successors of a Cut Node] are the best candidates for Split Points
  // while a Cut Node [the successor of a PV or an  All Node] should be avoided.
  //
  // Cut-node becomes All-node once the first and all candidate cutoff moves are searched [Pradhu Kannan]
  // First child of a Cut-node, and other candidate cutoff moves (nullmove, killers, captures, checks, ...) is All-node
  // Children of All-nodes are Cut-nodes
  //
  // PV Nodes may be satisfactory once their first child [which must also be a PV Node]
  // has been searched.
  //
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Eval pvs(
    Draft wDraft, Draft wReducedDraft, Eval mBest2, Eval mAlpha, Eval mBeta, Boolean bTryZWS) {
    var mValue = EvalUndefined;
    var mAlpha1 = (Eval)(mAlpha + 1);

    //
    // Primary Search
    //
    if (bTryZWS) {                      // PVS starts with a ZWS (after Raised Alpha)
      var moveFlags = FlagsMode;        //[Save]
      FlagsMode |= ModeFlags.ZWS;       //[Note]This allows prune()
      mValue = (Eval)(-search(wReducedDraft, (Eval)(-mAlpha1), (Eval)(-mAlpha)));
      FlagsMode = moveFlags;            //[Restore]
    }
    else
      mValue = (Eval)(-search(wDraft, (Eval)(-mBeta), (Eval)(-mAlpha)));

    //
    // Increment appropriate PVS Node Count:
    //
    if (FlagsMode.Has(ModeFlags.ZWS))                         // True ZWS [>200x PVSimple] is most frequent
      AtomicIncrement(ref State.ZWSimpleTotal);
    else if (!bTryZWS)                                        // Primary Search was a FWS
      AtomicIncrement(ref State.PVSimpleTotal);
    else if (mAlpha1 == mBeta && wDraft <= wReducedDraft)     // Primary Zero Window was the Full Window
      AtomicIncrement(ref State.PVSingleTotal);               // Rare, traditionally counted as PVSingle
    else if (mValue <= mBest2 || mBeta <= mValue)             //[Note]mBest2 vs mAlpha used for MultiPV > 1
      AtomicIncrement(ref State.PVSingleTotal);               // Skip second search [occurs >20x more than PVDouble]
    else {                                                    // PVDouble Search is >10000x more rare than the other cases
      AtomicIncrement(ref State.PVDoubleTotal);               // Second search required
      State.IncPVDoubleCount(SearchPly);                      // Update PVDouble Histogram

      //
      // Perform Secondary Full Window Search (FWS) if necessary (PVDouble)
      //
      // Not necessary to clear Draw50, if it was set by the Initial Search
      //[Safe]FlagsDraw &= ~DrawFlags.Draw50;
      //
      //[Warning]Passing in mValue vs mAlpha resulted in incremental score creep here:
      //
      mValue = (Eval)(-search(wDraft, (Eval)(-mBeta), (Eval)(-mAlpha)));
    }

    return mValue;
  }
  #endregion                            // PVS Method

  #region Forward Pruning Heuristics
  private Boolean prune(
    Draft wDraft, Depth wDepth, Eval mAlpha, Eval mBeta,
    Eval mValueFound, EvalType etFound, Boolean bMoveExcluded, out Eval mPrunedValue) {
    mPrunedValue = EvalUndefined;
    var bDepthLimit = State.Bound.Plies <= wDepth;
    var bMateSearch = State.Bound.MovesToMate.HasValue;
    var bNonMateWindow = -MateMin < mAlpha && mBeta < MateMin;
    var bReduced = FlagsMode.Has(ModeFlags.Reduced);
    var wShallow = reduceShallow(wDraft);

    //
    // Razoring is a risky Forward Pruning idea which
    // may prune 25% of the nodes in some positions.
    // See https://www.chessprogramming.org/Razoring
    //
    if (State.IsOccam && wDepth <= wOccamDepthMax) {
      if (bNonMateWindow && !(bReduced || bMateSearch || CanPromote())) {
        var mAlpha2 = (Eval)(mAlpha - occamDelta(wDepth));  // mAlpha vs mBeta

        if (mPrunedValue <= mAlpha2) {//[<]Search
          var mBeta2 = (Eval)(mAlpha2 + 1);
#if OccamReduced
          FlagsMode |= ModeFlags.Reduced;
          var mValue2 = Search(wShallow, mAlpha2, mBeta2));
#else
          var mValue2 = quiet(mAlpha2, mBeta2);
#endif
          if (mValue2 < mBeta2) {       // Prune
            traceVal("Occam Prune", mValue2);       //[Conditional]
            AtomicIncrement(ref State.OccamPruneTotal);
            mPrunedValue = mValue2;
            return true;                // No moves made here - omit storeXP()
          }                             // Prune
          else
            traceVal("Occam Non-Prune", mValue2);   //[Conditional]
        }                               // Search
      }                                 // Qualify
    }                                   // IsOccam

    //
    // Prevent Null Moves from being made twice in succession!
    //
    // isEndgame() is called to protect against ignoring Zugzwang.
    // [Null Moves might be used to help detect Zugzwang.]
    //
    if (State.IsNullPrune && SearchPly > wNullPlyMin &&
        !(IsNullMade() || bMateSearch || bMoveExcluded || isEndgame())) {
      //
      // A significant material advantage may suggest that the opponent should prefer
      // more promising alternatives.  Null Moves "skip a turn" in such cases, to see
      // whether this makes the current position any more tenable.  Assume the search
      // can be safely pruned otherwise.
      //
      // See https://www.chessprogramming.org/Null_Move_Pruning for a detailed
      // discussion of "Null Move Pruning".  An overview of the "Null-move heuristic"
      // can be found at http://en.wikipedia.org/wiki/Null-move_heuristic.
      //
      // It is worth testing whether a Null Move allows the opponent to improve their
      // position in any case.  However, this can obscure more accurate evaluation of
      // the position by pruning when the advantage to the side to move is increasing.
      // So, there is a tradeoff between pruning the search and obtaining an accurate
      // evaluation - including mate detection.
      //
      mPrunedValue = pruneval(wShallow, mAlpha, mBeta, mValueFound, etFound);

      // Null Move Pruning Test Cases:
      //-#8 [12-ply] 8/2N5/5R1p/2pP4/1pP3pP/1P2k3/r3n3/7K b - - 0 55
      //#10 [13-ply] 4Q3/6rk/5K2/8/8/8/8/8 w - - 0 1
      // #6 [10-ply] 1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PPK/8 w - - 0 4
      //
      if (mBeta <= mPrunedValue) {
        // Push Position for the search
        var child2 = Push();
        try {
          child2.nullMove();
          var wDeep = reduceDeep(wDraft);
#if DeepNull
          var wNullDraft = wDeep;
#else
          var wNullDraft = wShallow;
#endif
#if TestLerp
          //
          // Prune aggressively at lower depth; more conservatively at higher depth.
          //
          var mAbs = Abs(mBeta);
          var mBeta2 = MateMin <= mAbs ?
            mBeta : (Eval)Interpolate(wNullDraft, wReducedDraftMin, mBeta, wLerpDraftMax, mPrunedValue);
#else
          var mBeta2 = mBeta;
#endif
          var mAlpha2 = (Eval)(mBeta2 - 1);
          child2.FlagsMode |= ModeFlags.Reduced;
          // Limit wNullDraft = wDraft - R
          var mValue2 = wReducedDraftMin <= wNullDraft ?
            (Eval)(-child2.search(wNullDraft, (Eval)(-mBeta2), (Eval)(-mAlpha2))) :
            (Eval)(-child2.quiet((Eval)(-mBeta2), (Eval)(-mAlpha2)));
          if (mBeta2 <= mValue2) {      // Null Move did not improve mValue: Prune
            traceVal("Null Prune", mValue2);    //[Conditional]
            AtomicIncrement(ref State.PrunedNullMoves);
            if (wDepth < wVerifyDepthMin)
              mPrunedValue = mValue2;
            else {                      //[ToDo]Improve verification
              FlagsMode |= ModeFlags.Reduced;
              mPrunedValue = search(wDeep, mAlpha, mBeta);
            }
            return true;                // No moves made here - omit storeXP()
          }

          traceVal("Null Non-Prune", mValue2);//[Conditional]
        }
        finally {                       // Pop Position prior to return
          Pop(ref child2);              // Pop Position used for the search
        }
      }
    }

    return false;
  }
  #endregion                            // Forward Pruning Heuristics

  #region Extension Heuristics
  [Conditional("MateThreat")]
  private void threat(ref Draft wDraft, ref Draft wShallow, ref Depth wDepth) {
    //
    // Prevent Null Moves from being made twice in succession!
    //
    if (IsNullMade() || wThreatDepthMin > wDepth || isEndgame() || !canExtend(vThreat))
      return;

    var child2 = Push();                // Push Position for the search
    try {
      child2.nullMove();
      //[EvalRange]
      //var mAlpha2 = (Eval)(mAlpha - mThreatWeight); //[Test]
      const Eval mAlpha2 = -MateMin;
      var mBeta2 = (Eval)(mAlpha2 + 1); // vs -EvalMax
      child2.FlagsMode |= ModeFlags.Reduced;
#if DeepThreat
      var wThreatDraft = reduceDeep(wDraft);
#else
      var wThreatDraft = wShallow;
#endif
      var mThreat =
        (Eval)(-child2.search(wThreatDraft, (Eval)(-mBeta2), (Eval)(-mAlpha2)));
      if (EvalUndefined < mThreat && mThreat < mBeta2) {
        traceVal("Mate Threat", mThreat);       //[Conditional]

        incExtension(ref wDraft, vThreat);
        wDepth = depth(wDraft);                 // Threat Adjusted Depth
        wShallow = reduceShallow(wDraft);
        AtomicIncrement(ref State.ThreatExtCount);
      }
      else
        traceVal("Non-Mate Threat", mThreat);   //[Conditional]
    }
    finally {
      Pop(ref child2);                  // Pop Position used for the search
    }
  }

  private Eval clonedSearch(Draft wDraft, Eval mAlpha, Eval mBeta,
                            Move moveExcluded = Move.Undefined) {
    var mValue = EvalUndefined;

    //
    // Clone the Position to avoid interfering with the current moves enumeration
    //
    var clone = State.Push(Parent);     //[Note]Parent may be null

    try {
      clone.Clone(this);                // Prepare for resetMove()
      clone.FlagsMode |= ModeFlags.Reduced;
      mValue = clone.search(wDraft, mAlpha, mBeta, moveExcluded);
    }
    finally {
      Pop(ref clone);                   // Pop Clone
    }

    return mValue;
  }

  private Boolean singular(Draft wSingularDraft, Eval mValueFound, Move move) {
    var mBeta = (Eval)(mValueFound - mSingularWeight);
    var mAlpha = (Eval)(mBeta - 1);
    var moveMasked = move & Move.StoreMask;
    var mValue = clonedSearch(wSingularDraft, mAlpha, mBeta, moveMasked);
    var bSingular = mValue < mBeta;
    var sTrace = bSingular ? "Singular Extension" : "Singular Non-Extension";
    traceVal(sTrace, mValue);           //[Conditional]
    return bSingular;
  }
  #endregion                            // Extension Heuristics
  #endregion                            // Search Methods
}
