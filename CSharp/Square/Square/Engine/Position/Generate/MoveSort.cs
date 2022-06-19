//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-03-27 CNHume]Created File
//
// Conditionals:
//
//#define DebugMoveOrder
#define DebugMove
//#define DebugMoveColor
#define CountEarlyMoves
#define UseKillers
#define BottleBothSides
#define UseHistory
#define UseMoveSort
#define LazyMoveSort                    // LazyMoveSort ~10% faster than a full Array.Sort()
#define RewardMoveTypes
#define TestGoodValue

namespace Engine {
  using Exceptions;
  using MoveOrder;
  using static MoveOrder.TypedMove;
  using static Logging.Logger;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;
  using System.Text;                    // for StringBuilder

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Eval = Int16;
  using MemoHashcode = UInt32;

  partial class Position : Board {
    #region Move Order Heuristics
    private void rewardMoveType(Move move) {
      var type = moveType(move, WTM());
      var nIndex = Array.IndexOf(MoveTypes, type);
      if (nIndex < 0)
        throw new PositionException($"Could not find the {type} MoveType");
      else if (nIndex > 0)
        MoveTypes.Place(0, nIndex);

      //
      // New ordering inherited by subsequent children:
      //
      MoveTypeOrdering = Compress(MoveTypes);
    }

    protected void rewardMove(Move move, Depth wDepth, Eval mValue, EvalType et, Move moveExcluded) {
      //[Test]Debug.Assert(mValue > EvalUndefined, "rewardMove(EvalUndefined)");
#if NoMaterial
      var bMaterial = (move & Move.Material) != 0;
      if (bMaterial) return;
#endif
      var moveMasked = move & Move.NormalMask;
#if UseKillers
      storeKiller(moveMasked, wDepth, mValue, et);      //[Conditional]
#endif                                  // UseKillers
#if UseHistory
      storeXPM(wDepth, mValue, et, moveMasked, moveExcluded);
#endif
#if RewardMoveTypes
      rewardMoveType(move);
#endif
    }

    private void addMoves(List<GoodMove> goodMoves, Depth wDepth, Eval mAlpha, Eval mBeta, Move moveExcluded) {
#if UseHistory
      var bFound = probeXPM(wDepth, mAlpha, mBeta, moveExcluded, goodMoves);
#endif
#if UseKillers
      probeKiller(goodMoves, wDepth, mAlpha, mBeta);    //[Conditional]
#endif                                  // UseKillers
    }

#if UseMoveSort
    protected Int32 sortMoves(List<Move> moves, List<GoodMove> goodMoves, Depth wDepth) {
      //[Test]goodMoves.Sort();

      var nMoves = moves.Count;
      var mLateStart = Eval.MaxValue;
      var nEarly = 0;                   //[Init]
      foreach (var move in moves) {
        var nIndex = goodMoves.FindIndex(gm => equalMoves(gm.Move, move));
        if (nIndex >= 0) {
          nEarly++;
          State.IncEarlyMoveCount(SearchPly);   // Update EarlyMove Histogram
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
        var nIndex = goodMoves.FindIndex(gm => equalMoves(gm.Move, move));
#if TestGoodValue
        var mValue = EvalUndefined;

        if (nIndex < 0)
          mValue = --mLateNext;
        else {
          var good = goodMoves[nIndex];
          var mGoodValue = good.Value;
          //mValue = EvalUndefined < mGoodValue ? mGoodValue : (Eval)(mLateStart - nIndex);
          if (EvalUndefined < mGoodValue)
            mValue = mGoodValue;
          else
            mValue = (Eval)(mLateStart - nIndex);
        }
#else
        var mValue = nIndex < 0 ? EvalUndefined : (Eval)(Eval.MaxValue - nIndex);
#endif
        //
        // nGenerated index is included in SortMove so a Stable
        // Sort can be implemented on its IComparable interface
        //
        SortMoves[nGenerated++] = new SortMove(move, nGenerated, mValue, wDepth);
      }
#if LazyMoveSort
      PriorityMove.Truncate();          // Truncate Heap, preparing to rebuild.
      PriorityMove.IsAscending = true;

      //
      // Convert SortMoves[] into a Heap:
      //
      PriorityMove.Build(nGenerated);
#else
      Array.Sort<SortMove>(SortMoves, 0, nGenerated);
#endif
#if DebugMoveOrder
      if (IsTrace()) {
        DisplayCurrent("sortMoves()");

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
        Trace.Assert(!PriorityMove.IsAscending, "Heap Ascending after enumeration");
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
#else                                   // UseMoveSort
    protected Int32 sortMoves(List<Move> moves, List<GoodMove> goodMoves, Depth wDepth) {
      var nStart = SiftedMoves.Count;
      Trace.Assert(nStart == 0, "nStart != 0");

      var nEarlyCapacity = goodMoves.Count;
      var earlyMoves = new List<Move>(nEarlyCapacity);

      //
      // goodMoves.Count is not subtracted from moves.Count because there
      // is no guarantee that any of the goodMoves will be found in moves:
      //
      var nMoves = moves.Count;
      var lateMoves = new List<Move>(nMoves);

      //
      //[Note]The following operations are O(M*N) where N is the number of goodMoves
      //
      // Sift up the elements of "moves" found in goodMoves:
      //
      foreach (var move in moves) {
        if (goodMoves.Exists(gm => equalMoves(gm.Move, move))) {
          earlyMoves.Add(move);
          State.IncEarlyMoveCount(SearchPly);   // Update EarlyMove Histogram
        }
        else
          lateMoves.Add(move);
      }

      var bWTM = WTM();
      foreach (var gm in goodMoves) {   // Maintain goodMove priority for earlyMoves
#if DebugMove
        unpackMove1(gm.Move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
        //unpackMove2(gm.Move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
#if DebugMoveColor && BottleBothSides
        var bWhiteMove = (gm.Move & Move.WTM) != 0;
        if (bWTM != bWhiteMove) {
          Debug.Assert(bWTM == bWhiteMove, "WTM != WhiteMove [sortMoves]");
          DisplayCurrent("sortMoves()");
        }
#endif
        //SiftedMoves was cleared in generate() via clearPseudoMoveLists()
        var nIndex = earlyMoves.FindIndex(em => equalMoves(em, gm.Move));
        if (nIndex >= 0) {
          var em = earlyMoves[nIndex];
          //[Note]goodMoves may contain dupilicates
          if (!SiftedMoves.Exists(sm => equalMoves(sm, em))) {
#if TestGoodCapture
            var good = gm.Move & Move.NormalMask;
            if (good != em) {
              var goodCaptive = captured(good);
              var emCaptive = captured(em);
              if (emCaptive != Piece.Capture) {
                var sb = new StringBuilder();
                sb.AppendAN(good, Side, false);
                if (goodCaptive != Piece.None)
                  sb.Append(goodCaptive);

                sb.Append(" != ");
                sb.AppendAN(em, Side, false);
                if (emCaptive != Piece.None)
                  sb.Append(emCaptive);

                sb.FlushLine();
              }
              else if (goodCaptive != Piece.Capture) {
                good &= ~Move.CaptiveMask;
                good |= (Move)((Byte)Piece.Capture << nCaptiveBit);
              }
            }
#endif
            //
            //[Warning]It is necessary to Add(em) rather than gm here.
            //
            // An em capture will specify either Piece.Capture or Piece.EP; but a Killer gm
            // can specify a capture from some other position and still match an em capture.
            //
            SiftedMoves.Add(em);
          }
        }
      }

      var nEarly = SiftedMoves.Count;
      State.AddEarlyTotal(bWTM, nEarly);

      SiftedMoves.AddRange(lateMoves);
      var nGenerated = SiftedMoves.Count;

      if (nGenerated != nMoves) {
        Debug.Assert(nGenerated == nMoves, "nGenerated != nMoves");
      }

      return nEarly;
    }
#endif                                  // UseMoveSort
    #endregion
  }
}
