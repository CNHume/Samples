﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define DebugMove
#define AddBestMoves
//#define DebugMoveIsLegal
#define DebugSideToMove
#define DebugSearchMoves
//#define DebugPseudoMoves
//#define TraceVal
#define TransposeQuiet
//#define QuietCheck
//#define QuietMate                       //[Experimental]
//#define SwapOn
//#define VerifyUpper
//#define DebugBest
//#define TestBest

using System.Diagnostics;
using System.Text;

namespace Engine;

using MoveOrder;

using static GameState;
using static Logging.Logger;
using static MoveOrder.BestMoveEnum;


//
// Type Aliases:
//
using Eval = Int16;

partial class Position : Board {
  #region Constants
  private const Boolean Qxnt = true;
  //private const Boolean NotQxnt = !Qxnt;
  #endregion

  #region Search Methods
  private Eval quiet(Eval mAlpha, Eval mBeta) {
    const String methodName = nameof(quiet);
    BestMoves.Clear();                  //[Required]per iteration
    var moveBest = Move.Undefined;      //[Init]

    #region Test for Draw
    if (IsDraw() || IsDraw50()) {
      State.IncEvalType(EvalType.Exact);
#if TransposeQuiet
      return eval();
#else
      return boundValue(eval(), mAlpha, mBeta);
#endif
    }
    #endregion                          // Test for Draw

    #region Transposition Table Lookup
#if TraceVal
    if (IsTrace())                      //[Note]CurrentMove Undefined
      Display($"{methodName}()");
#endif
    // BestMoves updated iff bFoundValue
    if (probeQxnt(mAlpha, mBeta, out Move moveFound, out Eval mValueFound, out EvalType etFound)) {
      // moveFound not always defined for EvalType.Upper [Fail Low]
      if (IsDefinite(moveFound)) {      //[Safe]Also prevent unexpected EmptyMove
#if DebugMove
        unpackMove1(
          moveFound, out Sq sqFrom, out Sq sqTo,
          out Piece piece, out Piece promotion, out Boolean bCapture);
        //unpackMove2(
        //  moveFound, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion,
        //  out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
        if (!isMovePosition()) {
          //[Conditional]
          verifyMoveIsLegal(moveFound, methodName);
#if AddBestMoves
          //[Bug]Potential cf. search()
          addBest(moveFound, QuietProbe);
#endif                                  // AddBestMoves
        }
      }
#if DebugBest
      // BestMoves should not be empty here
      var emptyMessage1 = $"BestMoves.Count = {BestMoves.Count} Empty1 [{methodName}]";
      Debug.Assert(BestMoves.Count > 0, emptyMessage1);
#endif
      return mValueFound;
    }
    #endregion                          // Transposition Table Lookup

    #region Move Loop Initializaton
    var bTrace = IsTrace();
#if QuietCheck || QuietMate
    var bInCheck = InCheck();
#endif
    var et = EvalType.Upper;            //[Init]Fail Low is the default assumption
                                        // Stand Pat (tests Draw Flags)
    var mStand = standpatval(mValueFound, etFound);

    var mValue = EvalUndefined;
    var mBest = EvalUndefined;

    var bStandPat = mAlpha/* + mStandPatWeight*/ < mStand;
    if (bStandPat) {
      mBest = mStand;

      if (mAlpha < mBest)
        mAlpha = mBest;
    }
    #endregion                          // Move Loop Initializaton

    //if (IsFinal()) {
    //}

    if (mBeta <= mAlpha) {
      //[Test]return boundValue(mBest, mAlpha, mBeta);
    }
    else {
      #region Generate Moves
      var moves = PseudoMoves;
      if (SearchMoves != null && SearchMoves.Count > 0) {
        moves.Clear();
        moves.AddRange(SearchMoves);
#if DebugSearchMoves
        var sb = new StringBuilder("SearchMoves:")
          .MapMoves(Extension.AppendPACN, moves, Side, State.IsChess960)
          .FlushLine();
#endif
      }
      else {
#if SwapOn
        generate(moves, Swaps);
#elif QuietCheck || QuietMate
        if (bInCheck)
          generate(moves, !Swaps);
        else
          generateMaterialMoves(moves);
#else
        generateMaterialMoves(moves);
#endif
#if DebugPseudoMoves
        DisplayCurrent(methodName);
        var sb = new StringBuilder("PseudoMoves:")
          .MapMoves(Extensions.AppendPACN, moves, State.IsChess960)
          .FlushLine();
#endif
      }
      #endregion                        // Generate Moves

      var child = Push();               // Push Position to make the moves
      try {
        var bestLine = child.BestMoves;
        #region Move Loop
        var uLegalMoves = 0U;
        foreach (var mov in moves) {
          var move = mov;               // Allow tryMove(ref move) below
          #region Delta Prune or Make Move
#if DebugMove
          unpackMove1(
            move, out Sq sqFrom, out Sq sqTo,
            out Piece piece, out Piece promotion, out Boolean bCapture);
          //unpackMove2(
          //  move, out Sq sqFrom, out Sq sqTo,
          //  out Piece piece, out Piece promotion, out Piece capture,
          //  out Boolean bCastles, out Boolean bCapture);
#endif
          verifySideToMove(moveBest, methodName);     //[Conditional]

          var bNonMaterial = !move.Has(Move.Material);
#if QuietMate
          if (uLegalMoves > 0) {
#else
          if (true) {
#endif
#if QuietCheck
            if (bNonMaterial && !bInCheck) {
#else
            if (bNonMaterial) {
#endif
              AtomicIncrement(ref State.QuietSkipTotal);
              continue;
            }

            if (isDeltaPruned(ref move, mAlpha, mStand)) {
              AtomicIncrement(ref State.DeltaPruneTotal);
              continue;
            }
          }

          //
          //[Note]DrawFlags.Draw3 would only be set on entry, because the point of quiet() is to
          // continue searching only so long as moves which alter the material balance are found.
          //
          if (!child.tryMove(ref move, NotFindRepetition, Qxnt))
            continue;

          uLegalMoves++;
          #endregion

          //
          //[Note]IsDraw50() isn't called because quiet() searches
          // for moves which alter the material balance.
          //
#if QuietMate && !QuietCheck
          if (bNonMaterial) {           // Skip search of any Check Evasion.
            State.QuietSkipTotal++;
            continue;
          }
#endif
          mValue = (Eval)(-child.quiet((Eval)(-mBeta), (Eval)(-mAlpha)));

          #region Update Best Move
          if (mBest < mValue) {
            mBest = mValue;

            if (mAlpha < mBest) {
              //
              //[Note]Annotation is made from the child position resulting from each move.
              //
              moveBest = child.annotateFinal(move);
              //[Old]
              addBest(moveBest, QuietUpdate, bestLine);

              traceVal("Quiet Raised Alpha", mBest);  //[Conditional]
              mAlpha = mBest;
              if (mBeta <= mAlpha) {
#if TraceVal
                if (bTrace)
                  LogLine("Trace: Quiet Failed High");
#endif
                et = EvalType.Lower;    // Cutoff Reached: Ignore further moves and Fail High
                goto exit;
              }

              et = EvalType.Exact;
            }
          }
          #endregion                    // Update Best Move
        }                               //[Next]Pseudo Move
        #endregion
        if (uLegalMoves == 0) {
#if QuietMate
          if (isLeaf()) {
            SetFinal();                 // Mark Game Leaf
            mBest = finalValue();
          }
#endif
        }
        traceVal("Quiet Failed Low", mBest);    //[Conditional]
      }
      finally {
        Pop(ref child);                 // Pop Position used for this Ply
      }
    }

  exit:
#if VerifyUpper
    var bUpper = et == EvalType.Upper;
    var bIndefinite = IsIndefinite(moveBest);
    if (bUpper != bIndefinite) {
      Trace.Assert(bUpper == bIndefinite, "bUpper != bIndefinite");
    }
#endif
    if (mBest == EvalUndefined)
      mBest = mStand;
#if DebugBest
    // BestMoves should not be empty here
    var emptyMessage2 = $"BestMoves.Count = {BestMoves.Count} Empty2 [{methodName}]";
    Debug.Assert(BestMoves.Count > 0, emptyMessage2);
#endif
#if TransposeQuiet
    return storeQXP(mBest, et, moveBest);
#else
    return boundValue(mBest, mAlpha, mBeta);
#endif
  }

  protected Boolean isLeaf() {
    var moves = PseudoMoves;
    generate(moves, !Swaps);
    var child = Push();                 // Push Position to find a legal move
    try {
      foreach (var mov in moves) {
        var move = mov;
        if (child.tryMove(ref move, NotFindRepetition))
          return false;
      }

      return true;
    }
    finally {
      Pop(ref child);                   // Pop Position used for this test
    }
  }

  #region Delta Pruning Method
  //
  // Delta Pruning Heuristic: Skip insufficient captures and promotions,
  // unless an endgame has arisen.  The evaluation is estimated and the
  // move made only if the cutoff is met.  Check is not considered here.
  //
  private Boolean isDeltaPruned(ref Move move, Eval mAlpha, Eval mStand) {
    var bPrune = false;
    Eval mPromotion = 0;
    Eval mCapture = 0;

    var uPromotion = Promoted(move);
    if (uPromotion > 0) {
      // Only Pawns Promote:
      mPromotion = weightP(PieceIndex(uPromotion));
    }

    if (IsCapture(move)) {
      var nTo = To(move);
#if DebugMove
      var sqTo = (Sq)nTo;
#endif
      var vCapture = CaptureIndex(nTo, ref move, out Boolean bEnPassant);
      mCapture = weight(vCapture);
    }

    //
    // Test isEndgame() based on the Total of material remaining for both sides:
    //
    var mPositionDelta = (Eval)(mPromotion - mCapture);
    var mPositionTotal = (Eval)(staticTotal + mPositionDelta);

    var bEndGame = isEndgame(mPositionTotal);
    if (!bEndGame) {
      var mMoveDelta = (Eval)(mPromotion + mCapture);
      var mDelta = (Eval)(mDeltaBaseWeight + mMoveDelta);
      var mValue = (Eval)(mStand + mDelta);
      bPrune = mValue <= mAlpha;
      if (bPrune)                       //[Conditional]
        traceVal("Quiet Delta Pruned", mValue);
    }

    return bPrune;
  }
  #endregion                            // Delta Pruning Method
  #endregion                            // Search Methods
}
