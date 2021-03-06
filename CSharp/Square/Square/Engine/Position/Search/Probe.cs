﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2011-06-09 CNHume]Created File
//
// Conditionals:
//
//#define XPMCompositionHash
//#define KillerCompositionHash
#define BottleBothSides                 // Prevents odd extension from referencing opponent's Killer
//#define BottleGamePly
//#define DebugMoveColor
#define CountEvalTypes                  // for IncEvalType()
#define TransposeQuiet
#define QuiescentTryXP
//#define DedupeGoodMoves
//#define LoadMRU
//#define TraceVal                        // for traceVal()
//#define FailHard
//#define XPHash128                       // 128-bit Hashcodes take 9% more time to maintain;
//#define QXPHash128                      // and another 3% more time to compare
//#define XPMoveTypes

namespace Engine {
  using CacheValue;
  using MoveOrder;
  using static Logging.Logger;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Text;

  //
  // Type Aliases:
  //
  using Depth = System.UInt16;
  using Eval = System.Int16;
  using Ply = System.UInt16;
  using Hashcode = System.UInt64;

  partial class Position : Board {
    #region Helper Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Eval adjustValue(Eval mAlpha, Eval mBeta, Eval mValueFound, EvalType etFound, Ply wSearchPlies) {
      var mValue = EvalUndefined;

      if (EvalUndefined < mValueFound) {
        mValueFound = debitMate(mValueFound, wSearchPlies);
#if FailHard
        mValue = boundValue(mValue, mAlpha, mBeta);
#endif
        //
        // Lower Bound reliable when mBeta <= mValue
        // Upper Bound reliable when mValue <= mAlpha
        //
        if ((etFound == EvalType.Lower && mBeta <= mValueFound) ||
            (etFound == EvalType.Upper && mValueFound <= mAlpha) ||
            etFound == EvalType.Exact)
          mValue = mValueFound;
      }

      return mValue;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Move adjustEmptyMove(Move moveFound) {
      //
      // The Final Move Flag is set when the resulting child position has been
      // determined to be Final, i.e., that there are no Legal Moves available
      // for the child.
      //
      // Restore LoFlags.Final to annotate the parent's move; and to prevent a
      // redundant search.
      //
      if (isEmptyMove(moveFound)) {
        setFinal();
        moveFound = Move.Undefined;
      }
#if DebugMoveColor
      if (isDefined(moveFound) && WTM())
        moveFound |= Move.WTM;
#endif
      return moveFound;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public Eval addMove(Move moveFound, List<GoodMove> goodMoves,
                        Depth wDepth, Eval mValueFound, Eval mAlpha, Eval mBeta, EvalType etFound,
                        Boolean bFilterEvalUndefined = false) {
      var mValue = adjustValue(mAlpha, mBeta, mValueFound, etFound, SearchPly);
      // Note: Adjusted mValue may be EvalUndefined
      var bAllowValue = EvalUndefined < mValue || !bFilterEvalUndefined;
      if (bAllowValue && goodMoves is not null && isDefinite(moveFound)) {
#if DedupeGoodMoves
        //
        // Duplicates should be avoided; but will be filtered from SiftedMoves
        //
        if (goodMoves.Exists(gm => equalMoves(gm.Move, moveFound))) {
#if DEBUG
          var sb = new StringBuilder()
            .AppendFormat("Filtering ")
            .AppendAN(moveFound, State.Rule.IsChess960);
          LogLine(sb.ToString());
#endif
          return mValue;
        }
#endif                                  // DedupeKillers
        // moveFound may be either annotated or abbreviated
        var good = new GoodMove(moveFound, wDepth, mValue, etFound);
#if LoadMRU
        goodMoves.Insert(0, good);      // MRU order
#else
        goodMoves.Add(good);            // LRU works best
#endif
      }

      return mValue;
    }
    #endregion

    #region XPM Methods
    protected Eval storeXPM(Depth wDepth, Eval mValue, EvalType et,
                            Move moveBest = Move.Undefined,
                            Move moveExcluded = Move.Undefined) {       // 10 MHz
      Trace.Assert(EvalUndefined < mValue, "storeXPM(EvalUndefined)");
      traceVal("storeXPM", mValue, et); //[Conditional]
      State.IncEvalType(et);
#if XPMCompositionHash || DebugMoveColor
      var bWTM = WTM();
#endif
#if XPMCompositionHash
      UInt32 wPly = State.MovePly;
      var nSide = bWTM ? 0 : 1;
      var uMemoHash = compositionHash(bWTM);
      var qDynamic = (Hashcode)(uMemoHash * wPly + nSide);
#else
      var qDynamic = DynamicHash(moveExcluded);
#endif
      var mAdjusted = creditMate(mValue, SearchPly);

      if (IsFinal()) {
        Trace.Assert(!isDefined(moveBest), "moveBest defined in a Final position.");
        moveBest = Move.EmptyMove;
      }
#if DebugMoveColor
      if (isDefinite(moveBest)) {
        var bWhiteMove = (moveBest & Move.WTM) != 0;
        if (bWTM != bWhiteMove) {
          Debug.Assert(bWTM == bWhiteMove, "WTM != WhiteMove [storeXPM]");
          DisplayCurrent("storeXPM()");
        }
      }
#endif
#if XPHash128
      var store = new PositionMove(qDynamic, HashPawn, State.MovePly, wDepth,
                                   mAdjusted, et, moveBest);
#else
      var store = new PositionMove(qDynamic, State.MovePly, wDepth,
                                   mAdjusted, et, moveBest);
#endif
      State.XPMTank.Save(store);
      return mValue;
    }

    protected Boolean probeXPM(Depth wDepth, Eval mAlpha, Eval mBeta, Move moveExcluded, List<GoodMove> goodMoves) {
#if XPMCompositionHash
      var bWTM = WTM();
      UInt32 wPly = State.MovePly;
      var nSide = bWTM ? 0 : 1;
      var uMemoHash = compositionHash(bWTM);
      var qDynamic = (Hashcode)(uMemoHash * wPly + nSide);
#else
      var qDynamic = DynamicHash(moveExcluded);
#endif
#if XPHash128
      var match = new PositionMove(qDynamic, HashPawn, State.MovePly, wDepth);
#else
      var match = new PositionMove(qDynamic, State.MovePly, wDepth);
#endif
      var matches = new List<PositionMove>();
      State.XPMTank.Load(match, matches);
      var bFound = matches.Count > 0;

      foreach (var found in matches) {
        var moveFound = adjustEmptyMove(found.BestMove);
        var etFound = found.Type;
        var mValueFound = found.Value;
        traceVal("probeXPM", mValueFound, etFound);     //[Conditional]
        var mValue = addMove(moveFound, goodMoves, wDepth, mValueFound, mAlpha, mBeta, etFound);
      }

      return bFound;
    }
    #endregion

    #region XP Methods
    protected Eval storeXP(Depth wDepth, Eval mValue, EvalType et,
                           Move moveBest = Move.Undefined,
                           Move moveExcluded = Move.Undefined) {  // 10 MHz
      Trace.Assert(EvalUndefined < mValue, "storeXP(EvalUndefined)");
      traceVal("storeXP", mValue, et);  //[Conditional]
      State.IncEvalType(et);
      var qDynamic = DynamicHash(moveExcluded);
      var mAdjusted = creditMate(mValue, SearchPly);

      if (IsFinal()) {
        Trace.Assert(!isDefined(moveBest), "moveBest defined in a Final position.");
        moveBest = Move.EmptyMove;
      }
#if DebugMoveColor
      if (isDefinite(moveBest)) {
        var bWTM = WTM();
        var bWhiteMove = (moveBest & Move.WTM) != 0;
        if (bWTM != bWhiteMove) {
          Debug.Assert(bWTM == bWhiteMove, "WTM != WhiteMove [storeXP]");
          DisplayCurrent("storeXP()");
        }
      }
#endif
#if XPHash128
#if XPMoveTypes
      var store = new Transposition(qDynamic, HashPawn, MoveTypeOrdering, State.MovePly, wDepth,
                                    mAdjusted, et, moveBest);
#else
      var store = new Transposition(qDynamic, HashPawn, State.MovePly, wDepth,
                                    mAdjusted, et, moveBest);
#endif
#else                                   // XPHash128
#if XPMoveTypes
      var store = new Transposition(qDynamic, MoveTypeOrdering, State.MovePly, wDepth,
                                    mAdjusted, et, moveBest);
#else
      var store = new Transposition(qDynamic, State.MovePly, wDepth,
                                    mAdjusted, et, moveBest);
#endif
#endif
      State.XPTank.Save(store);
      return mValue;
    }

    protected Boolean probeXP(Depth wDepth, Eval mAlpha, Eval mBeta, Move moveExcluded, List<GoodMove> goodMoves,
                              out Move moveFound, out Eval mValue, out EvalType etFound) {
      var qDynamic = DynamicHash(moveExcluded);
#if XPHash128
#if XPMoveTypes
      var match = new Transposition(qDynamic, HashPawn, MoveTypeOrdering, State.MovePly, wDepth);
#else
      var match = new Transposition(qDynamic, HashPawn, State.MovePly, wDepth);
#endif
#else                                   // XPHash128
#if XPMoveTypes
      var match = new Transposition(qDynamic, MoveTypeOrdering, State.MovePly, wDepth);
#else
      var match = new Transposition(qDynamic, State.MovePly, wDepth);
#endif
#endif
      var bValid = State.XPTank.LoadFirst(ref match);
#if XPMoveTypes
      if (bValid) MoveTypeOrdering = match.MoveTypeOrdering;
#endif
      moveFound = adjustEmptyMove(match.BestMove);      //[out]3
      etFound = match.Type;                             //[out]2
      var mValueFound = match.Value;
      traceVal("probeXP", mValueFound, etFound);        //[Conditional]
      mValue = addMove(moveFound, goodMoves, wDepth, mValueFound, mAlpha, mBeta, etFound);  //[out]1
      var bValueDefined = EvalUndefined < mValue;
      return bValid && bValueDefined;
    }
    #endregion

    #region QXP Methods
    protected Eval storeQXP(Eval mValue, EvalType et,
                            Move moveBest = Move.Undefined) {
      Trace.Assert(EvalUndefined < mValue, "storeQXP(EvalUndefined)");
      traceVal("storeQXP", mValue, et); //[Conditional]
      State.IncEvalType(et);
      var mAdjusted = creditMate(mValue, SearchPly);

      if (IsFinal()) {
        Trace.Assert(!isDefined(moveBest), "moveBest defined in a Final position.");
        moveBest = Move.EmptyMove;
      }
#if DebugMoveColor
      if (isDefinite(moveBest)) {
        var bWTM = WTM();
        var bWhiteMove = (moveBest & Move.WTM) != 0;
        if (bWTM != bWhiteMove) {
          Debug.Assert(bWTM == bWhiteMove, "WTM != WhiteMove [storeQXP]");
          DisplayCurrent("storeQXP()");
        }
      }
#endif
#if QXPHash128
      var store = new QuietPosition(Hash, State.MovePly, HashPawn, mAdjusted, et, moveBest);
#else
      var store = new QuietPosition(Hash, State.MovePly, mAdjusted, et, moveBest);
#endif
      State.QXPTank.Save(store);
      return mValue;
    }

    protected Boolean probeQXP(Eval mAlpha, Eval mBeta,
                               out Move moveFound, out Eval mValue, out EvalType etFound) {
#if QXPHash128
      var match = new QuietPosition(Hash, State.MovePly, HashPawn);
#else
      var match = new QuietPosition(Hash, State.MovePly);
#endif
      var bValid = State.QXPTank.LoadFirst(ref match);
      var moveBest = adjustEmptyMove(match.BestMove);
      moveFound = isDefined(moveBest) ? moveBest |= Move.Qxnt : moveBest;   //[out]3
      etFound = match.Type;                             //[out]2
      //[Note]Mate values are suspect because quiet moves were not considered
      var mValueFound = match.Value;
      mValue = adjustValue(mAlpha, mBeta, mValueFound, etFound, SearchPly); //[out]1
      traceVal("probeQXP", mValue, etFound);            //[Conditional]
      var bValueDefined = EvalUndefined < mValue;
      return bValid && bValueDefined;
    }
    #endregion

    #region Combined XP and QXP Methods
    // For lookupPV()
    protected void probeMove(Eval mAlpha, Eval mBeta, out Move moveFound) {
      var bFoundValue = false;
      Debug.Assert(mAlpha < mBeta, "Alpha must be less than Beta");
#if TransposeQuiet || QuiescentTryXP
      moveFound = Move.Undefined;
#if QuiescentTryXP
      const Depth wDepth = 0;
      bFoundValue = probeXP(wDepth, mAlpha, mBeta, Move.Undefined, null, out moveFound, out Eval mValue, out EvalType etFound);
#endif
#if TransposeQuiet
      if (!isDefined(moveFound))
        bFoundValue = probeQXP(mAlpha, mBeta, out moveFound, out mValue, out etFound);
#endif
#endif                                  // TransposeQuiet || QuiescentTryXP
    }

    protected Boolean probeQxnt(Eval mAlpha, Eval mBeta, out Move moveFound, out Eval mValue, out EvalType etFound) {
      var bFoundValue = false;
      etFound = EvalType.Undefined;
      Debug.Assert(mAlpha < mBeta, "Alpha must be less than Beta");
#if TransposeQuiet || QuiescentTryXP
      moveFound = Move.Undefined;
      mValue = EvalUndefined;
#if QuiescentTryXP
      const Depth wDepth = 0;
      bFoundValue = probeXP(wDepth, mAlpha, mBeta, Move.Undefined, null, out moveFound, out mValue, out etFound);
      State.IncQxnt(bFoundValue);       //[Conditional]
#endif
#if TransposeQuiet
      if (!bFoundValue)                 //[C#]There is no Logical-OR assignment operator ||=
        bFoundValue = probeQXP(mAlpha, mBeta, out moveFound, out mValue, out etFound);
#endif
#endif                                  // TransposeQuiet || QuiescentTryXP
      return bFoundValue;
    }
    #endregion

    #region Killer Methods
    //
    //[ToDo]Killer updates are not thread safe.  See also the references, e.g., in sortMoves().
    //
    protected void storeKiller(Move uMaskedMove, Depth wDepth, Eval mValue, EvalType et) {
      var bWTM = WTM();
      Trace.Assert(EvalUndefined < mValue, "storeKiller(EvalUndefined)");
      traceVal("storeKiller", mValue, et);      //[Conditional]
      var mAdjusted = creditMate(mValue, SearchPly);
      var store = new GoodMove(uMaskedMove, wDepth, mAdjusted, et);
      UInt32 wPly = State.MovePly;
#if BottleGamePly
      wPly = GamePly;
#else
      wPly += wDepth;                   //[Note]wDepth value may not guarantee Ply/Color Parity
#endif
#if KillerCompositionHash
      var uMemoHash = compositionHash(true);
      wPly *= uMemoHash;
#endif
#if BottleBothSides
      var nSide = bWTM ? 0 : 1;
#else
      var nSide = 0;
#endif
      State.Bottle.Save(store, uMaskedMove, wPly, nSide);
    }

    protected Boolean probeKiller(List<GoodMove> goodMoves, Depth wDepth, Eval mAlpha, Eval mBeta) {
      var bWTM = WTM();
      const Boolean bFilterEvalUndefined = true;
      UInt32 wPly = State.MovePly;
#if BottleGamePly
      wPly += SearchPly;
#else
      wPly += wDepth;
#endif
#if KillerCompositionHash
      var uMemoHash = compositionHash(true);
      wPly *= uMemoHash;
#endif
#if BottleBothSides
      var nSide = bWTM ? 0 : 1;
#else
      var nSide = 0;
#endif
      var killers = State.Bottle.Load(wPly, nSide);
      var bFound = killers.Count > 0;

      foreach (var killer in killers) {
        var moveFound = killer.Move;
        var mValueFound = killer.Value;
        var etFound = killer.Type;
        traceVal("probeKiller", mValueFound, etFound);  //[Conditional]
        addMove(moveFound, goodMoves, wDepth, mValueFound, mAlpha, mBeta, etFound, bFilterEvalUndefined);
      }

      return bFound;
    }
    #endregion

    #region Traced Position Diagnostic
    [Conditional("TraceVal")]
    protected void traceVal(String sLabel, Eval? mValue, EvalType et = EvalType.Undefined) {
      if (IsTrace()) {
        var sb = new StringBuilder();
        sb.AppendFormat($"Trace #{State.NodeTotal}: {sLabel}");
        if (mValue.HasValue) {
          var mEval = reflectValue(WTM(), (Eval)mValue);
          sb.Append(" Eval");
          sb.AppendEvalTerm((Eval)mEval);
          if (mValue != EvalUndefined)
            sb.AppendFormat($" {et}");
        }

        if (State.NodeTotal == 57388) {
          LogLine("TraceFound");
        }
#if DebugStand
        if (Enum.TryParse<EvalType>(sLabel, out EvalType etLabel)) {
        }
#endif
        LogLine(sb.ToString());
      }
    }
    #endregion
  }
}
