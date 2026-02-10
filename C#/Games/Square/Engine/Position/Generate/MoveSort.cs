//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2012-03-27 CNHume]Created File
//
// Conditionals:
//
//#define DebugMoveOrder
#define CountEarlyMoves
#define UseKillers
#define BottleBothSides
#define UseHistory                      //[Debug]Adds Illegal Moves
#define LazyMoveSort                    // LazyMoveSort significantly faster than Array.Sort()
#define RewardMoveTypes
//#define TestGoodCapture
#define TestGoodValue
//#define TestRewardMove

using System.Diagnostics;
using System.Text;

namespace Engine;

using Exceptions;

using MoveOrder;

using static Logging.Logger;            // For DebugMoveOrder

//
// Type Aliases:
//
using Depth = UInt16;
using Eval = Int16;

partial class Position : Board {
  #region Move Order Heuristics
  private void rewardMoveType(Move move) {
    unpack1(move, out Int32 nFrom, out Int32 nTo,
            out UInt32 uPiece, out Boolean bCapture);

    //
    //[Note]IsAbove() is assessed from the perspective of Foe
    // because IncrementGamePly() was called by PlayMove().
    //
    var parameter = Foe.Parameter;
    var bAbove = parameter.IsAbove(nTo);
    var type = moveType(nFrom, nTo, uPiece, bCapture, bAbove);
#if TestRewardMove
    var sb = new StringBuilder()
      .AppendPACN(move, Side, State.IsChess960)
      .Append($" by {parameter.SideName} is {type}");
    LogLine(sb.ToString());
#endif
    var nIndex = Array.IndexOf(moveTypes, type);
    if (nIndex < 0)
      throw new PositionException($"Could not find the {type} MoveType");

    moveTypes.Rotate(0, nIndex);

    //
    // expandMoveTypes(moveTypeOrdering) is called by addPseudoMoves() to
    // provide preliminary ordering for PseudoMoves created by generate().
    //
    //[ToDo]Add moveTypeOrdering to the XPM payload in storeXPM() and probeXPM()
    // so that the new ordering will be recalled for subsequent searches at the
    // current Ply Depth.
    //
    // This moveTypeOrdering is currently inherited by children at subsequent Depth.
    //
    moveTypeOrdering = compressMoveTypes(moveTypes);
  }

  private void rewardMove(Move move, Depth wDepth, Eval mValue, EvalType et, Move moveExcluded) {
    //[Test]Debug.Assert(mValue > EvalUndefined, $"{nameof(rewardMove)}({nameof(EvalUndefined)})");
#if NoMaterial
    var bMaterial = move.Has(Move.Material);
    if (bMaterial) return;
#endif
    var moveMasked = move & Move.StoreMask;
    //[Conditional]
    storeKiller(moveMasked, wDepth, mValue, et);
    //[Conditional]
    storeXPM(wDepth, mValue, et, moveMasked, moveExcluded);
#if RewardMoveTypes
    rewardMoveType(move);
#endif
  }

  private void addGoodMoves(
    List<GoodMove> goodMoves, Depth wDepth, Eval mAlpha, Eval mBeta, Move moveExcluded) {
    //[Conditional]
    probeXPM(goodMoves, wDepth, mAlpha, mBeta, moveExcluded);
    //[Conditional]
    probeKiller(goodMoves, wDepth, mAlpha, mBeta);
  }

  private Int32 sortMoves(List<Move> moves, List<GoodMove> goodMoves, Depth wDepth) {
    //[Test]goodMoves.Sort();

    var nMoves = moves.Count;
    var mLateStart = Eval.MaxValue;
    var nEarly = 0;                     //[Init]
    foreach (var move in moves) {
      var nIndex = goodMoves.FindIndex(gm => EqualMoves(gm.Move, move));
      if (nIndex >= 0) {
        nEarly++;
        State.IncEarlyMoveCount(SearchPly);     // Update EarlyMove Histogram
        var good = goodMoves[nIndex];
        var mGoodValue = good.Value;
        if (EvalUndefined < mGoodValue && mGoodValue < mLateStart) {
          mLateStart = mGoodValue;
        }
      }
    }

    var bWTM = WTM();
    State.AddEarlyTotal(bWTM, nEarly);

    var mLateNext = mLateStart;
    var nGenerated = 0;
    foreach (var move in moves) {
      var nIndex = goodMoves.FindIndex(gm => EqualMoves(gm.Move, move));
#if TestGoodValue
      var mValue = EvalUndefined;

      if (nIndex < 0)                   // Move not found among goodMoves
        mValue = --mLateNext;           // Next value below Eval.MaxValue
      else {
        var good = goodMoves[nIndex];
        var mGoodValue = good.Value;    // goodMove Eval

        //mValue = EvalUndefined < mGoodValue ? mGoodValue : (Eval)(mLateStart - nIndex);
        if (EvalUndefined < mGoodValue)
          mValue = mGoodValue;
        else
          mValue = (Eval)(mLateStart - nIndex);
      }
#else                                  //!TestGoodValue
      var mValue = nIndex < 0 ? EvalUndefined : (Eval)(Eval.MaxValue - nIndex);
#endif                                  // TestGoodValue
      //
      // nGenerated index is included in SortMove so a Stable
      // Sort can be implemented on its IComparable interface
      //
      SortMoves[nGenerated++] = new(move, nGenerated, mValue, wDepth);
    }
#if LazyMoveSort
    PriorityMove.Truncate();            // Truncate Heap, preparing to rebuild.
    PriorityMove.IsAscending = true;
    //
    //[Note]SortMoves became referenced as the PriorityMove.Entries array when
    // the PriorityMove = new Heap<SortMove>(SortMoves) constructor was called.
    //
    // Perform a Lazy Sort by converting SortMoves[] into a Heap:
    //
    PriorityMove.Build(nGenerated);
#else
    Array.Sort<SortMove>(SortMoves, 0, nGenerated);
#endif
#if DebugMoveOrder
    if (IsTrace()) {
      DisplayCurrent(nameof(sortMoves));

      var sb = new StringBuilder();
#if LazyMoveSort
      var nMoveIndex = 0;
      foreach (var sm in PriorityMove) {
#else
      for (var nMoveIndex = 0; nMoveIndex < nGenerated; nMoveIndex++) {
        var sm = SortMoves[nMoveIndex];
#endif
        sb.Clear();
        sb.AppendAN(sm.Move, Side, State.IsChess960);
        LogLine($"{nMoveIndex}) {sb}: Depth = {sm.Depth}, Value = {sm.Value}, Index = {sm.Index}");
#if LazyMoveSort
        nMoveIndex++;
#endif
      }
#if LazyMoveSort
      Debug.Assert(!PriorityMove.IsAscending, "Heap Ascending after enumeration");
      if (!PriorityMove.IsAscending)
        PriorityMove.Reverse();
#endif
    }
#endif                                  // DebugMoveOrder
    if (nGenerated != nMoves) {
      Debug.Assert(nGenerated == nMoves, "nGenerated != nMoves");
    }

    //[Commented]Scale(bWTM, wPly, 0.75f);
    return nEarly;
  }
  #endregion                            // Move Order Heuristics
}
