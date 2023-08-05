//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
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
#define CountEvalTypes                  // For IncEvalType()
#define TransposeQuiet
#define QuiescentTryXP
//#define DedupeGoodMoves
//#define LoadMRU
//#define TraceVal                        // For traceVal()
//#define FailHard
//#define XPHash128                       // 128-bit Hashcodes take 9% more time to maintain;
//#define QXPHash128                      // and another 3% more time to compare
//#define XPMoveTypes

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;

namespace Engine;

using CacheValue;

using MoveOrder;

using static Logging.Logger;

//
// Type Aliases:
//
using Depth = UInt16;
using Eval = Int16;
using Ply = UInt16;

partial class Position : Board {
  #region Helper Methods
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private static Eval adjustValue(
    Eval mAlpha, Eval mBeta, Eval mValueFound, EvalType etFound, Ply wSearchPlies) {
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
  private Move adjustEmptyMove(Move moveFound) {
    //
    // The Final Move Flag is set when the resulting child position has been
    // determined to be Final, i.e., that there are no Legal Moves available
    // for the child.
    //
    // Restore TurnFlags.Final to annotate the parent's move; and to prevent
    // a redundant search.
    //
    if (IsEmptyMove(moveFound)) {
      SetFinal();
      moveFound = Move.Undefined;
    }
#if DebugMoveColor
    if (IsDefined(moveFound) && WTM())
      moveFound |= Move.WTM;
#endif
    return moveFound;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Eval addMove(Move moveFound, List<GoodMove>? goodMoves,
                       Depth wDepth, Eval mValueFound, Eval mAlpha, Eval mBeta, EvalType etFound,
                       Boolean bFilterEvalUndefined = false) {
    var mValue = adjustValue(mAlpha, mBeta, mValueFound, etFound, SearchPly);
    //[Note]Adjusted mValue may be EvalUndefined
    var bAllowValue = EvalUndefined < mValue || !bFilterEvalUndefined;
    if (bAllowValue && goodMoves != null && IsDefinite(moveFound)) {
#if DedupeGoodMoves
      //
      // Duplicates should be avoided; but will be filtered from SiftedMoves
      //
      if (goodMoves.Exists(gm => EqualMoves(gm.Move, moveFound))) {
#if DEBUG
        var sb = new StringBuilder()
          .AppendFormat("Filtering ")
          .AppendAN(moveFound, Side, State.IsChess960);
        LogLine(sb.ToString());
#endif
        return mValue;
      }
#endif                                  // DedupeGoodMoves
      // moveFound may be either annotated or abbreviated
      var good = new GoodMove(moveFound, wDepth, mValue, etFound);
#if LoadMRU
      goodMoves.Insert(0, good);        // MRU order
#else
      goodMoves.Add(good);              // LRU works best
#endif
    }

    return mValue;
  }
  #endregion                            // Helper Methods

  #region XPM Methods
  private Eval storeXPM(Depth wDepth, Eval mValue, EvalType et,
                        Move moveBest = Move.Undefined,
                        Move moveExcluded = Move.Undefined) { // 10 MHz
    const String methodName = nameof(storeXPM);
    Debug.Assert(EvalUndefined < mValue, $"{methodName}({nameof(EvalUndefined)})");
    traceVal(methodName, mValue, et);   //[Conditional]
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
      Trace.Assert(IsUndefined(moveBest), $"moveBest defined in a Final position [{methodName}].");
      moveBest = Move.EmptyMove;
    }
#if DebugMoveColor
    if (IsDefinite(moveBest)) {
      var bWhiteMove = moveBest.Has(Move.WTM);
      if (bWTM != bWhiteMove) {
        Debug.Assert(bWTM == bWhiteMove, $"WTM != WhiteMove [{methodName}]");
        DisplayCurrent(methodName);
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

  private Boolean probeXPM(Depth wDepth, Eval mAlpha, Eval mBeta,
                           Move moveExcluded, List<GoodMove> goodMoves) {
    const String methodName = nameof(probeXPM);
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
      traceVal(methodName, mValueFound, etFound);   //[Conditional]
      var mValue = addMove(moveFound, goodMoves, wDepth, mValueFound, mAlpha, mBeta, etFound);
    }

    return bFound;
  }
  #endregion                            // XPM Methods

  #region XP Methods
  private Eval storeXP(Depth wDepth, Eval mValue, EvalType et,
                       Move moveBest = Move.Undefined,
                       Move moveExcluded = Move.Undefined) {  // 10 MHz
    const String methodName = nameof(storeXP);
    Debug.Assert(EvalUndefined < mValue, $"{methodName}({nameof(EvalUndefined)})");
    traceVal(methodName, mValue, et);   //[Conditional]
    State.IncEvalType(et);
    var qDynamic = DynamicHash(moveExcluded);
    var mAdjusted = creditMate(mValue, SearchPly);

    if (IsFinal()) {
      Trace.Assert(IsUndefined(moveBest), $"moveBest defined in a Final position [{methodName}].");
      moveBest = Move.EmptyMove;
    }
#if DebugMoveColor
    if (IsDefinite(moveBest)) {
      var bWTM = WTM();
      var bWhiteMove = moveBest.Has(Move.WTM);
      if (bWTM != bWhiteMove) {
        Debug.Assert(bWTM == bWhiteMove, $"WTM != WhiteMove [{methodName}]");
        DisplayCurrent(methodName);
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

  private Boolean probeXP(Depth wDepth, Eval mAlpha, Eval mBeta,
                          Move moveExcluded, List<GoodMove>? goodMoves,
                          out Move moveFound, out Eval mValue, out EvalType etFound) {
    const String methodName = nameof(probeXP);
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
    moveFound = adjustEmptyMove(match.BestMove);        //[out]3
    etFound = match.Type;                               //[out]2
    var mValueFound = match.Value;
    traceVal(methodName, mValueFound, etFound);     //[Conditional]
    mValue = addMove(moveFound, goodMoves, wDepth, mValueFound, mAlpha, mBeta, etFound);  //[out]1
    var bValueDefined = EvalUndefined < mValue;
    return bValid && bValueDefined;
  }
  #endregion                            // XPM Methods

  #region QXP Methods
  private Eval storeQXP(Eval mValue, EvalType et,
                        Move moveBest = Move.Undefined) {
    const String methodName = nameof(storeQXP);
    Debug.Assert(EvalUndefined < mValue, $"{methodName}({nameof(EvalUndefined)})");
    traceVal(methodName, mValue, et);   //[Conditional]
    State.IncEvalType(et);
    var mAdjusted = creditMate(mValue, SearchPly);

    if (IsFinal()) {
      Trace.Assert(IsUndefined(moveBest), $"moveBest defined in a Final position [{methodName}].");
      moveBest = Move.EmptyMove;
    }
#if DebugMoveColor
    if (IsDefinite(moveBest)) {
      var bWTM = WTM();
      var bWhiteMove = moveBest.Has(Move.WTM);
      if (bWTM != bWhiteMove) {
        Debug.Assert(bWTM == bWhiteMove, $"WTM != WhiteMove [{methodName}]");
        DisplayCurrent(methodName);
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

  private Boolean probeQXP(Eval mAlpha, Eval mBeta,
                           out Move moveFound, out Eval mValue, out EvalType etFound) {
    const String methodName = nameof(probeQXP);
#if QXPHash128
    var match = new QuietPosition(Hash, State.MovePly, HashPawn);
#else
    var match = new QuietPosition(Hash, State.MovePly);
#endif
    var bValid = State.QXPTank.LoadFirst(ref match);
    var moveBest = adjustEmptyMove(match.BestMove);
    moveFound = IsUndefined(moveBest) ? moveBest : moveBest | Move.Qxnt;    //[out]3
    etFound = match.Type;                       //[out]2
                                                //[Note]Mate values are suspect because quiet moves were not considered
    var mValueFound = match.Value;
    mValue = adjustValue(mAlpha, mBeta, mValueFound, etFound, SearchPly);   //[out]1
    traceVal(methodName, mValue, etFound);      //[Conditional]
    var bValueDefined = EvalUndefined < mValue;
    return bValid && bValueDefined;
  }
  #endregion                            // QXP Methods

  #region Combined XP and QXP Methods
  // For lookupPV()
  private void probeMove(Eval mAlpha, Eval mBeta, out Move moveFound) {
    var bFoundValue = false;
    Debug.Assert(mAlpha < mBeta, "Alpha must be less than Beta");
#if TransposeQuiet || QuiescentTryXP
    moveFound = Move.Undefined;
#if QuiescentTryXP
    const Depth wDepth = 0;
    bFoundValue = probeXP(wDepth, mAlpha, mBeta, Move.Undefined, default, out moveFound, out Eval mValue, out EvalType etFound);
#endif
#if TransposeQuiet
    if (IsUndefined(moveFound))
      bFoundValue = probeQXP(mAlpha, mBeta, out moveFound, out mValue, out etFound);
#endif
#endif                                  // TransposeQuiet || QuiescentTryXP
  }

  private Boolean probeQxnt(Eval mAlpha, Eval mBeta, out Move moveFound, out Eval mValue, out EvalType etFound) {
    var bFoundValue = false;
    etFound = EvalType.Undefined;
    Debug.Assert(mAlpha < mBeta, "Alpha must be less than Beta");
#if TransposeQuiet || QuiescentTryXP
    moveFound = Move.Undefined;
    mValue = EvalUndefined;
#if QuiescentTryXP
    const Depth wDepth = 0;
    bFoundValue = probeXP(wDepth, mAlpha, mBeta, Move.Undefined, default, out moveFound, out mValue, out etFound);
    State.IncQxnt(bFoundValue);         //[Conditional]
#endif
#if TransposeQuiet
    if (!bFoundValue)                   //[C#]There is no Logical-OR assignment operator ||=
      bFoundValue = probeQXP(mAlpha, mBeta, out moveFound, out mValue, out etFound);
#endif
#endif                                  // TransposeQuiet || QuiescentTryXP
    return bFoundValue;
  }
  #endregion                            // Combined XP and QXP Methods

  #region Killer Methods
  //
  //[ToDo]Killer updates are not thread safe.  See also the references, e.g., in sortMoves().
  //
  private void storeKiller(Move uMaskedMove, Depth wDepth, Eval mValue, EvalType et) {
    const String methodName = nameof(storeKiller);
    Debug.Assert(EvalUndefined < mValue, $"{methodName}({nameof(EvalUndefined)})");
    traceVal(methodName, mValue, et);   //[Conditional]
    var mAdjusted = creditMate(mValue, SearchPly);
    var store = new GoodMove(uMaskedMove, wDepth, mAdjusted, et);
    UInt32 wPly = State.MovePly;
#if BottleGamePly
    wPly = GamePly;
#else
    wPly += wDepth;                     //[Note]wDepth value may not guarantee Ply/Color Parity
#endif
#if KillerCompositionHash
    var uMemoHash = compositionHash(true);
    wPly *= uMemoHash;
#endif
#if BottleBothSides
    var nSide = WTM() ? 0 : 1;
#else
    var nSide = 0;
#endif
    State.Bottle.Save(store, uMaskedMove, wPly, nSide);
  }

  private Boolean probeKiller(List<GoodMove> goodMoves, Depth wDepth, Eval mAlpha, Eval mBeta) {
    const String methodName = nameof(probeKiller);
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
      traceVal(methodName, mValueFound, etFound);   //[Conditional]
      addMove(moveFound, goodMoves, wDepth, mValueFound, mAlpha, mBeta, etFound, bFilterEvalUndefined);
    }

    return bFound;
  }
  #endregion                            // Killer Methods

  #region Traced Position Diagnostic
  [Conditional("TraceVal")]
  private void traceVal(String sLabel, Eval? mValue, EvalType et = EvalType.Undefined) {
    if (IsTrace()) {
      var sb = new StringBuilder();
      sb.AppendFormat($"Trace #{State.Nodes}: {sLabel}");
      if (mValue.HasValue) {
        var mEval = ReflectValue(WTM(), (Eval)mValue);
        sb.Append(" Eval");
        sb.AppendEvalTerm((Eval)mEval);
        if (mValue != EvalUndefined)
          sb.AppendFormat($" {et}");
      }
#if DebugStand
      if (Enum.TryParse<EvalType>(sLabel, out EvalType etLabel)) {
      }
#endif
      LogLine(sb.ToString());
    }
  }
  #endregion                            // Traced Position Diagnostic
}
