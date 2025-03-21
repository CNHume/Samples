﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2011-06-09 CNHume]Created File
//
// Conditionals:
//
//#define XPMCompositionHash
//#define KillerCompositionHash
#define BottleBothSides                 // Prevents odd extension from referencing opponent's Killer
//#define BottleGamePly
//#define DebugMoveIsLegal
#define DebugSideToMove
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
using BottleHash = UInt32;
using Depth = UInt16;
using Eval = Int16;
#if XPMCompositionHash
using Hashcode = UInt64;
#endif
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
  private Move adjustMoveFound(Move move, String callerName) {
    //
    // The Final Move Flag is set when the resulting child position has been
    // determined to be Final, i.e., that there are no Legal Moves available
    // for the child.
    //
    // Restore TurnFlags.Final to annotate the parent's move; and to prevent
    // a redundant search.
    //
    if (IsEmptyMove(move)) {
      SetFinal();
      move = Move.Undefined;
    }
    else
      verifySideToMove(move, callerName);       //[Conditional]

    return move;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Eval addGoodMove(
    List<GoodMove>? goodMoves, Move moveFound,
    Depth wDepth, Eval mValueFound, Eval mAlpha, Eval mBeta, EvalType etFound,
    Boolean bFilterEvalUndefined = false) {
    const String methodName = nameof(addGoodMove);
    var mValue = adjustValue(mAlpha, mBeta, mValueFound, etFound, SearchPly);
    traceVal(methodName, mValue, etFound);  //[Conditional]

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
      GoodMove good = new(moveFound, wDepth, mValue, etFound);
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
  [Conditional("UseHistory")]
  private void storeXPM(Depth wDepth, Eval mValue, EvalType et,
                        Move moveBest = Move.Undefined,
                        Move moveExcluded = Move.Undefined) { // 10 MHz
    const String methodName = nameof(storeXPM);
    Debug.Assert(EvalUndefined < mValue, $"{methodName}({nameof(EvalUndefined)})");
    traceVal(methodName, mValue, et);   //[Conditional]
    State.IncEvalType(et);
#if XPMCompositionHash || DebugSideToMove
    var bWTM = WTM();
#endif
#if XPMCompositionHash
    var wPly = State.MovePly;
    var nSide = bWTM ? 0 : 1;
    var uMemoHash = compositionHash(bWTM);
    var qDynamic = (Hashcode)uMemoHash * wPly + (Hashcode)nSide;
#else
    var qDynamic = DynamicHash(moveExcluded);
#endif
    var mAdjusted = creditMate(mValue, SearchPly);

    if (IsFinal()) {
      const String message = $"moveBest defined in a Final position [{methodName}].";
      Debug.Assert(IsIndefinite(moveBest), message);
      moveBest = Move.EmptyMove;
    }

    verifySideToMove(moveBest, methodName);     //[Conditional]
#if XPHash128
    PositionMove store = new(qDynamic, HashPawn, State.MovePly, wDepth,
                             mAdjusted, et, moveBest);
#else
    PositionMove store = new(qDynamic, State.MovePly, wDepth,
                             mAdjusted, et, moveBest);
#endif
    State.XPMTank.Save(store);
  }

  [Conditional("UseHistory")]
  private void probeXPM(List<GoodMove> goodMoves, Depth wDepth,
                        Eval mAlpha, Eval mBeta, Move moveExcluded) {
    const String methodName = nameof(probeXPM);
#if XPMCompositionHash
    var bWTM = WTM();
    var wPly = State.MovePly;
    var nSide = bWTM ? 0 : 1;
    var uMemoHash = compositionHash(bWTM);
    var qDynamic = (Hashcode)uMemoHash * wPly + (Hashcode)nSide;
#else
    var qDynamic = DynamicHash(moveExcluded);
#endif
#if XPHash128
    PositionMove match = new(qDynamic, HashPawn, State.MovePly, wDepth);
#else
    PositionMove match = new(qDynamic, State.MovePly, wDepth);
#endif
    List<PositionMove> matches = [];
    State.XPMTank.Load(match, matches);
    var bFound = matches.Count > 0;

    foreach (var found in matches) {
      var moveFound = adjustMoveFound(found.BestMove, methodName);
      var etFound = found.Type;
      var mValueFound = found.Value;
      traceVal(methodName, mValueFound, etFound);   //[Conditional]
      var mValue = addGoodMove(
        goodMoves, moveFound,
        wDepth, mValueFound, mAlpha, mBeta, etFound);
    }
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
      const String message = $"moveBest defined in a Final position [{methodName}].";
      Debug.Assert(IsIndefinite(moveBest), message);
      moveBest = Move.EmptyMove;
    }

    verifySideToMove(moveBest, methodName);     //[Conditional]
#if XPHash128
#if XPMoveTypes
    Transposition store = new(qDynamic, HashPawn, MoveTypeOrdering, State.MovePly, wDepth,
                              mAdjusted, et, moveBest);
#else
    Transposition store = new(qDynamic, HashPawn, State.MovePly, wDepth,
                              mAdjusted, et, moveBest);
#endif
#else                                   // XPHash128
#if XPMoveTypes
    Transposition store = new(qDynamic, MoveTypeOrdering, State.MovePly, wDepth,
                              mAdjusted, et, moveBest);
#else
    Transposition store = new(qDynamic, State.MovePly, wDepth,
                              mAdjusted, et, moveBest);
#endif
#endif
    State.XPTank.Save(store);
    return mValue;
  }

  private Boolean probeXP(Depth wDepth, Eval mAlpha, Eval mBeta,
                          Move moveExcluded, List<GoodMove>? goodMoves,
                          out Move moveFound, out Eval mValue, out EvalType etFound, Boolean bTest = false) {
    const String methodName = nameof(probeXP);
    var qDynamic = DynamicHash(moveExcluded);
#if XPHash128
#if XPMoveTypes
    Transposition match = new(qDynamic, HashPawn, MoveTypeOrdering, State.MovePly, wDepth);
#else
    Transposition match = new(qDynamic, HashPawn, State.MovePly, wDepth);
#endif
#else                                   // XPHash128
#if XPMoveTypes
    Transposition match = new(qDynamic, MoveTypeOrdering, State.MovePly, wDepth);
#else
    Transposition match = new(qDynamic, State.MovePly, wDepth);
#endif
#endif
    var bValid = State.XPTank.LoadFirst(ref match);
#if XPMoveTypes
    if (bValid) MoveTypeOrdering = match.MoveTypeOrdering;
#endif
    moveFound = adjustMoveFound(match.BestMove, methodName);  //[out]1
    etFound = match.Type;                                     //[out]3
    var mValueFound = match.Value;
    traceVal(methodName, mValueFound, etFound); //[Conditional]

    const Move testMove = (Move)0x00060DFF;
    //const Move testMove = (Move)0x00560FBF;
    if (bTest && State.Nodes == 15062440 && EqualMoves(testMove, moveFound)) {
      // #15062440
      // this == 6rk/5Qp1/5pNp/3P3P/2b5/2P5/5PPK/2R5 b - - 8 50
      // 0x403E9F3D36FFFE9F
      // Parent == 5Nrk/5Qp1/5p1p/3P3P/2b5/2P5/5PPK/2R5 w - - 7 50
      // 0xB0C61808739951E6
      //moveFound |= Move.WTM;            //[Debug]Problem Present Here
      //[Conditional]
      verifyMoveIsLegal(moveFound, methodName);
      DisplayCurrent(methodName);
    }

    mValue = addGoodMove(
      goodMoves, moveFound,
      wDepth, mValueFound, mAlpha, mBeta, etFound);     //[out]2

    var bValueDefined = EvalUndefined < mValue;
    var bFound = bValid && bValueDefined;
    if (bFound) {
      var message = $"Found {moveFound} move [{methodName}]";
      //Debug.Assert(IsDefined(moveFound), message);
    }
    return bFound;
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
      const String message = $"moveBest defined in a Final position [{methodName}].";
      Debug.Assert(IsIndefinite(moveBest), message);
      //[Note]Move.Empty should never be saved to QXPTank.
      moveBest = Move.Undefined;
    }

    verifySideToMove(moveBest, methodName);     //[Conditional]
#if QXPHash128
    QuietPosition store = new(Hash, State.MovePly, HashPawn, mAdjusted, et, moveBest);
#else
    QuietPosition store = new(Hash, State.MovePly, mAdjusted, et, moveBest);
#endif
    State.QXPTank.Save(store);
    return mValue;
  }

  private Boolean probeQXP(Eval mAlpha, Eval mBeta,
                           out Move moveFound, out Eval mValue, out EvalType etFound) {
    const String methodName = nameof(probeQXP);
#if QXPHash128
    QuietPosition match = new(Hash, State.MovePly, HashPawn);
#else
    QuietPosition match = new(Hash, State.MovePly);
#endif
    var bValid = State.QXPTank.LoadFirst(ref match);
    var moveBest = match.BestMove;
    verifySideToMove(moveBest, methodName);     //[Conditional]

    moveFound = IsUndefined(moveBest) ? moveBest : moveBest | Move.Qxnt;    //[out]1
    etFound = match.Type;                       //[out]3
                                                //[Note]Mate values are suspect because quiet moves were not considered
    var mValueFound = match.Value;
    mValue = adjustValue(mAlpha, mBeta, mValueFound, etFound, SearchPly);   //[out]2
    traceVal(methodName, mValue, etFound);      //[Conditional]
    var bValueDefined = EvalUndefined < mValue;
    var bFound = bValid && bValueDefined;
    if (bFound) {
      var message = $"Found {moveFound} move [{methodName}]";
      //Debug.Assert(IsDefined(moveFound), message);
    }
    return bFound;
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
    bFoundValue = probeXP(wDepth, mAlpha, mBeta,
                          Move.Undefined, default,
                          out moveFound, out Eval mValue, out EvalType etFound);
#endif
#if TransposeQuiet
    if (IsUndefined(moveFound))
      bFoundValue = probeQXP(mAlpha, mBeta, out moveFound, out mValue, out etFound);
#endif
#endif                                  // TransposeQuiet || QuiescentTryXP
  }

  private Boolean probeQxnt(Eval mAlpha, Eval mBeta,
                            out Move moveFound, out Eval mValue, out EvalType etFound) {
    var bFoundValue = false;
    etFound = EvalType.Undefined;
    Debug.Assert(mAlpha < mBeta, "Alpha must be less than Beta");
#if TransposeQuiet || QuiescentTryXP
    moveFound = Move.Undefined;
    mValue = EvalUndefined;
#if QuiescentTryXP
    const Depth wDepth = 0;
    bFoundValue = probeXP(wDepth, mAlpha, mBeta,
                          Move.Undefined, default,
                          out moveFound, out mValue, out etFound);
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
  [Conditional("UseKillers")]
  private void storeKiller(Move move, Depth wDepth, Eval mValue, EvalType et) {
    const String methodName = nameof(storeKiller);
    Debug.Assert(EvalUndefined < mValue, $"{methodName}({nameof(EvalUndefined)})");
    traceVal(methodName, mValue, et);   //[Conditional]
    var mAdjusted = creditMate(mValue, SearchPly);
    GoodMove store = new(move, wDepth, mAdjusted, et);
#if BottleGamePly
    var wPly = GamePly;
#else
    var wPly = State.MovePly + wDepth;  //[Note]wDepth value may not guarantee Ply/Color Parity
#endif
#if KillerCompositionHash
    var uBottleHash = (BottleHash)wPly * compositionHash(true);
#else
    var uBottleHash = (BottleHash)wPly;
#endif
#if BottleBothSides
    var nSide = WTM() ? 0 : 1;
#else
    var nSide = 0;
#endif
    State.Bottle.Save(store, move, uBottleHash, nSide);
  }

  [Conditional("UseKillers")]
  private void probeKiller(List<GoodMove> goodMoves, Depth wDepth, Eval mAlpha, Eval mBeta) {
    const String methodName = nameof(probeKiller);
    const Boolean bFilterEvalUndefined = true;
#if BottleGamePly
    var wPly = GamePly;
#else
    var wPly = State.MovePly + wDepth;  //[Note]wDepth value may not guarantee Ply/Color Parity
#endif
#if KillerCompositionHash
    var uBottleHash = (BottleHash)wPly * compositionHash(true);
#else
    var uBottleHash = (BottleHash)wPly;
#endif
#if BottleBothSides
    var nSide = WTM() ? 0 : 1;
#else
    var nSide = 0;
#endif
    var killers = State.Bottle.Load(uBottleHash, nSide);
    var bFound = killers.Count > 0;

    foreach (var killer in killers) {
      var moveFound = killer.Move;
      var mValueFound = killer.Value;
      var etFound = killer.Type;
      traceVal(methodName, mValueFound, etFound);   //[Conditional]
      addGoodMove(
        goodMoves, moveFound,
        wDepth, mValueFound, mAlpha, mBeta, etFound,
        bFilterEvalUndefined);
    }
  }
  #endregion                            // Killer Methods

  #region Traced Position Diagnostic
  [Conditional("TraceVal")]
  private void traceVal(String sLabel, Eval? mValue, EvalType et = EvalType.Undefined) {
    if (IsTrace()) {
      var sb = new StringBuilder();
      sb.AppendNodeNumber(State.Nodes);
      if (mValue.HasValue) {
        var mEval = ReflectValue(WTM(), (Eval)mValue);
        sb.Append(" Eval")
          .AppendEvalTerm((Eval)mEval);
        if (mValue != EvalUndefined)
          sb.AppendFormat($" {et}");
      }
#if DebugStand
      if (Enum.TryParse(sLabel, out EvalType etLabel)) {
      }
#endif
      LogLine(sb.ToString());
    }
  }
  #endregion                            // Traced Position Diagnostic
}
