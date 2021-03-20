//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
#define DebugMove
//#define DebugMoveColor
#define DebugSearchMoves
#define AddBestMoves
#define AddRangeBestMoves               //[Debug]
//#define DebugPseudoMoves
//#define TraceVal
#define TransposeQuiet
//#define QuietCheck
//#define QuietMate                       //[Experimental]quiet() may overlook the best defence
//#define SwapOn
//#define VerifyUpper

namespace Engine {
  using static Logging.Logger;
  using static GameState;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Text;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;

  partial class Position : Board {
    #region Constants
    private const Boolean Qxnt = true;
    //private const Boolean NotQxnt = !Qxnt;
    private const Boolean FindRepetition = true;
    private const Boolean NotFindRepetition = !FindRepetition;
    #endregion

    #region Search Methods
    protected Eval quiet(Eval mAlpha, Eval mBeta) {
      var moves = PseudoMoves;
      BestMoves.Clear();                //[Required]

      #region Test for Draw
      if (IsDraw()) {                   //[Note]setDraw50() will not be called below
        State.IncEvalType(EvalType.Exact);
#if TransposeQuiet
        return eval();
#else
        return boundValue(eval(), mAlpha, mBeta);
#endif
      }
      #endregion

      #region Transposition Table Lookup
#if TraceVal
      var bTrace = IsTrace();
      if (bTrace) {
        const String sLabel = "quiet()";
        Display(sLabel);                //[Note]Undefined CurrentMove
      }
#endif
      // BestMoves updated iff bFoundValue
      if (probeQxnt(mAlpha, mBeta, out Move moveFound, out Eval mValueFound, out EvalType etFound)) {
        // moveFound not always defined for EvalType.Upper [Fail Low]
        if (isDefinite(moveFound)) {    //[Safe]Also prevent unexpected EmptyMove
#if DebugMove
          unpackMove1(moveFound, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
          //unpackMove2(moveFound, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Boolean bCastles, out Boolean bCapture, out Piece capture);
#endif
#if AddBestMoves
          BestMoves.Add(moveFound);
#endif
        }

        return mValueFound;
      }
      #endregion

      #region Move Loop Initializaton
#if QuietCheck || QuietMate
      var bInCheck = InCheck();
#endif
      var et = EvalType.Upper;          //[Init]Fail Low is the default assumption
                                        // Stand Pat (tests Draw Flags)
      var mStand = standpatval(mValueFound, etFound);

      var moveBest = Move.Undefined;    //[Init]
      var mValue = EvalUndefined;
      var mBest = EvalUndefined;

      var bStandPat = mAlpha/*+ mStandPatWeight*/ < mStand;
      if (bStandPat) {
        mBest = mStand;

        if (mAlpha < mBest)
          mAlpha = mBest;
      }
      #endregion

      if (mBeta <= mAlpha) {
        //[Test]return boundValue(mBest, mAlpha, mBeta);
      }
      else {
        #region Generate Moves
        if (SearchMoves is not null && SearchMoves.Count > 0) {
          moves.Clear();
          moves.AddRange(SearchMoves);
#if DebugSearchMoves
          var sb = new StringBuilder("SearchMoves:");
          sb.MapMoves(Extension.AppendPACN, moves, State.Rule);
          sb.FlushLine();
#endif
        }
        else {
#if SwapOn
          generate(moves, Swaps);
#elif QuietCheck || QuietMate
          if (bInCheck)
            generate(moves, NoSwaps);
          else
            generateMaterialMoves(moves);
#else
          generateMaterialMoves(moves);
#endif
#if DebugPseudoMoves
          DisplayCurrent("quiet()";
          var sb = new StringBuilder("PseudoMoves:");
          sb.mapMoves(Extensions.AppendPACN, moves, State.Rule.IsChess960);
          sb.FlushLine();
#endif
        }
        #endregion

        var child = Push();             // Push Position to make the moves
        try {
          #region Move Loop
#if DebugMoveColor
          var bDebugWTM = WTM();
#endif
          var uLegalMoves = 0U;
          foreach (var mov in moves) {
            var move = mov;
            #region Delta Prune or Make Move
#if DebugMove
            unpackMove1(move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
            //unpackMove2(move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Boolean bCastles, out Boolean bCapture, out Piece capture);
#endif
#if DebugMoveColor
            var bWhiteMove = (move & Move.WTM) != 0;
            if (bDebugWTM != bWhiteMove) {
              Debug.Assert(bDebugWTM == bWhiteMove, "WTM != WhiteMove [search]");
            }
#endif
            var bNonMaterial = (move & Move.Material) == 0;
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
            //[Note]setDraw50() is not called because the point of quiet() is to continue
            // searching only so long as moves which alter the material balance are found.
            //
#if QuietMate && !QuietCheck
            if (bNonMaterial) {         // Skip search of any Check Evasion.
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
                moveBest =
                  child.CurrentMove = child.annotateFinal(move);
#if AddBestMoves
                BestMoves.Clear();
                BestMoves.Add(moveBest);
#if AddRangeBestMoves
                BestMoves.AddRange(child.BestMoves);
#endif
#endif
                traceVal("Quiet Raised Alpha", mBest);  //[Conditional]
                mAlpha = mBest;
                if (mBeta <= mAlpha) {
#if TraceVal
                  if (bTrace)
                    LogLine("Trace: Quiet Failed High");
#endif
                  et = EvalType.Lower;  // Cutoff Reached: Ignore further moves and Fail High
                  goto exit;
                }

                et = EvalType.Exact;
              }
            }
            #endregion
          }                             //[Next]Pseudo Move
          #endregion
#if QuietMate
          if (uLegalMoves == 0) {       // No Move Found
            setFinal();                 // May not be a true Game Leaf
            mBest = final();
          }
#endif
          traceVal("Quiet Failed Low", mBest);  //[Conditional]
        }
        finally {
          Pop(ref child);               // Pop Position used for this Ply
        }
      }

    exit:
#if VerifyUpper
      var bUpper = et == EvalType.Upper;
      var bUndefined = moveBest == Move.Undefined;
      if (bUpper != bUndefined) {
        Trace.Assert(bUpper == bUndefined, "bUpper != bUndefined");
      }
#endif
      if (mBest == EvalUndefined)
        mBest = mStand;

#if TransposeQuiet
      return storeQXP(mBest, et, moveBest);
#else
      return boundValue(mBest, mAlpha, mBeta);
#endif
    }

    #region Delta Pruning Method
    //
    // Delta Pruning Heuristic: Skip insufficient captures and promotions,
    // unless an endgame has arisen.  The evaluation is estimated and the
    // move made only if the cutoff is met.  Check is not considered here.
    //
    private Boolean isDeltaPruned(ref Move move, Eval mAlpha, Eval mStand) {
      var bPrune = false;
      var bCapture = (move & Move.CaptiveMask) != 0;
      var promotion = (Piece)((UInt32)move >> nPromoteBit & vPieceMask);
      var bPromotion = promotion != Piece._;

      var mCapture = (Eval)0;
      var mPromotion = (Eval)0;

      if (bCapture) {
        var nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
#if DebugMove
        var sqTo = (sq)nTo;
#endif
        var vCapture = captured(nTo, ref move, out Boolean bEnPassant);

        if (vK6 <= vCapture) {
          Debug.Assert(vCapture != vK6, "Unknown Captive",
                       "No captive found for {0}.", (sq)nTo);
        }

        mCapture = weight(vCapture);
      }

      if (bPromotion) {
        var vPromotion = (Byte)((Byte)promotion - vFirst);
        // Only Pawns Promote:
        mPromotion = weightP(vPromotion);
      }

      //
      // Test isEndgame() based on the Total of material remaining for both sides:
      //
      var mPositionDelta = (Eval)(mPromotion - mCapture);
      var mPositionTotal = (Eval)(StaticTotal + mPositionDelta);

      var bEndGame = isEndgame(mPositionTotal);
      if (!bEndGame) {
        var mMoveDelta = (Eval)(mPromotion + mCapture);
        var mDelta = (Eval)(mDeltaBaseWeight + mMoveDelta);
        var mValue = (Eval)(mStand + mDelta);
        bPrune = mValue <= mAlpha;
        if (bPrune)
          traceVal("Quiet Delta Pruned", mValue);       //[Conditional]
      }

      return bPrune;
    }
    #endregion
    #endregion
  }
}
