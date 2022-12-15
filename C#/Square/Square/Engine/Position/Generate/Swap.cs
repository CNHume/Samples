//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-05-07 CNHume]Created File
//
// Conditionals:
//
//#define DebugMove

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;

  using static System.Math;

  //
  // Type Aliases:
  //
  using Eval = Int16;

  partial class Position : Board {
    #region Methods
    protected void sortSwaps(List<Move> moves) {
      var child = Push();               // Push Position to make the moves
      try {
        //var uLegalMoves = 0U;
        foreach (var mov in moves) {
          var move = mov;
          var nTo = To(move);
#if DebugMove
          unpackMove1(move, out @sq sqFrom, out @sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
          //unpackMove2(move, out @sq sqFrom, out @sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
          if (child.tryMove(ref move)) {
            //uLegalMoves++;
            var vCapture = CaptureIndex(nTo, ref move, out Boolean bEnPassant);
            var mCapture = weight(vCapture);
            var uPromotion2 = Promoted(move);
            if (uPromotion2 > 0) {
              var vPromotion = PieceIndex((Byte)uPromotion2);
              // Only Pawns Promote:
              mCapture += weightP(vPromotion);
            }

            //
            // Divide Piece captures into Good and Bad Capture lists:
            //
            var bGood = child.swap(nTo, (Eval)(-mCapture)) < 0;
            var list = bGood ? PseudoGoodCaptures : PseudoBadCaptures;
            list.Add(move);
          }
        }
      }
      finally {
        Pop(ref child);                 // Pop Position used for this Ply
      }
    }

    private Eval swap(Int32 nTo, Eval mStand) {
      var moves = PseudoCaptures;
      var mValue = mStand;
      generateSwaps(moves, nTo);
      var child = Push();               // Push Position to make the moves
      try {
        //var uLegalMoves = 0U;
        foreach (var mov in moves) {
          var move = mov;
#if DebugMove
          unpackMove1(move, out @sq sqFrom, out @sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
          //unpackMove2(move, out @sq sqFrom, out @sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
          var vCapture = CaptureIndex(nTo, ref move, out Boolean bEnPassant);
          // EP unexpected here: Prior move was a capture
          Debug.Assert(!bEnPassant, "Unexpected EnPassant");

          if (child.tryMove(ref move)) {
            //uLegalMoves++;
            var mCapture = weight(vCapture);
            var uPromotion2 = Promoted(move);
            if (uPromotion2 > 0) {
              var vPromotion = PieceIndex((Byte)uPromotion2);
              // Only Pawns Promote:
              mCapture += weightP(vPromotion);
            }

            mValue = (Eval)(mStand - child.swap(nTo, (Eval)(-mCapture)));
            break;
          }
        }
      }
      finally {
        Pop(ref child);                 // Pop Position used for this Ply
      }

      return Max(mStand, mValue);
    }
    #endregion
  }
}
