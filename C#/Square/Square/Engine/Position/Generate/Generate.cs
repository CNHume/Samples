﻿//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split PseudoMove Generation into this file
//
// Conditionals:
//
#define UnshadowRay
#define UnshadowRay2
#define UseMoveSort

namespace Engine {
  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Runtime.CompilerServices;

  using static MoveOrder.TypedMove;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    #region Pseudo Move Clears
    private void clearPseudoMoves() {
      PseudoPawnAboveMove.Clear();
      PseudoPawnBelowMove.Clear();
      PseudoKnightMove.Clear();
      PseudoKingMove.Clear();
      PseudoDiagAboveMove.Clear();
      PseudoDiagBelowMove.Clear();
      PseudoRectAboveMove.Clear();
      PseudoRectBelowMove.Clear();
    }

    private void clearPseudoCaptures() {
      PseudoPawnAboveCapture.Clear();
      PseudoPawnBelowCapture.Clear();
      PseudoKnightCapture.Clear();
      PseudoKingCapture.Clear();
      PseudoDiagAboveCapture.Clear();
      PseudoDiagBelowCapture.Clear();
      PseudoRectAboveCapture.Clear();
      PseudoRectBelowCapture.Clear();
    }

    //
    //[Warning]Duplicate Moves can result in a number of strange, difficult to debug side-effects.
    //
    protected void clearPseudoMoveLists(List<Move> moves, Boolean bSwap) {    // ~32 MHz
      moves.Clear();
      clearPseudoCaptures();
      clearPseudoMoves();

      PseudoCastles.Clear();
      PseudoEPCapture.Clear();
      PseudoQueenPromotion.Clear();
      PseudoUnderPromotion.Clear();
      PseudoQueenPromotionCapture.Clear();
      PseudoUnderPromotionCapture.Clear();
      PseudoCaptures.Clear();

      if (bSwap) {
        PseudoBadCaptures.Clear();
        PseudoGoodCaptures.Clear();
      }
#if !UseMoveSort
      SiftedMoves.Clear();              // See sortMoves()
#endif
    }

    protected void clearPseudoMaterialMoveLists(List<Move> moves) {
      moves.Clear();
      clearPseudoCaptures();

      PseudoEPCapture.Clear();
      PseudoQueenPromotion.Clear();
      PseudoUnderPromotion.Clear();
      PseudoQueenPromotionCapture.Clear();
      PseudoUnderPromotionCapture.Clear();

      PseudoCaptures.Clear();
    }

    // Called by generateSwaps()
    protected void clearPseudoSwapLists(List<Move> moves) {
      moves.Clear();
      clearPseudoCaptures();

      PseudoQueenPromotionCapture.Clear();
      PseudoUnderPromotionCapture.Clear();
      //PseudoEPCapture.Clear();        // Not needed for Swaps

      PseudoBadCaptures.Clear();
      PseudoGoodCaptures.Clear();
    }
    #endregion

    #region Search Move Generators
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane includeEnPassant(Plane qpFoe) {
      return IsPassed() && (qpFoe & Pawn) != 0 ?
        qpFoe | BIT0 << ep(FlagsTurn) : qpFoe;
    }

    // Adds all Pseudo Moves at 400 to 1000 KHz; Generates moves at ~18 MHz
    protected Int32 generate(List<Move> moves, Boolean bSwap) {
      var bInCheck = InCheck();
      var qpFriend = Friend.Piece;
      var qpFoe = Foe.Piece;
      var vKingPos = Friend.GetKingPos();

      clearPseudoMoveLists(moves, bSwap);
#if UnshadowRay2
      var bRayCheck = false;
#endif
      if (bInCheck) {
        var qpKing = Friend.Piece & King;
        var qpChx = Foe.Checkers(vKingPos, qpKing);
#if UnshadowRay
        bRayCheck = (qpChx & (DiagPiece | RectPiece)) != 0;
#endif
        var qpChx2 = qpChx;
        var nChx = RemoveLo(ref qpChx2);
        var bSingleCheck = qpChx2 == 0;

        if (bSingleCheck) {
          //
          // Interposition includes capture of the checking
          // piece, and interpositions along a ray giving check.
          // Only Moves for these To Squares will be considered:
          //
          var qpRay = interpositions(nChx, vKingPos);
          var qpTo = qpChx | qpRay;
          if (qpTo != 0)
            addPieceCapturesAndMoves(qpTo);

          Friend.AddPawnCaptures(this, includeEnPassant(qpChx));
          Friend.AddPawnMoves(this, qpRay);
        }                               // bSingleCheck
      }
      else {                            //!bInCheck
        addPieceCapturesAndMoves(~qpFriend);
        Friend.AddPawnCaptures(this, includeEnPassant(qpFoe));
        Friend.AddPawnMoves(this, ~RankPiece);

        addCastles();
      }                                 //!bInCheck
#if UnshadowRay2
      addKingCapturesAndMoves(~qpFriend, vKingPos, bRayCheck);
#else
      addKingCapturesAndMoves(~qpFriend, vKingPos);
#endif
      if (bSwap && !bInCheck)
        addPseudoMovesGood(moves);
      else
        addPseudoMoves(moves);

      return State!.IncPseudoMoveTotal(moves.Count);
    }
    #endregion

    #region Quiet Move Generator
    protected Int32 generateMaterialMoves(List<Move> moves) {
      var bInCheck = InCheck();
      var qpFoe = Foe.Piece;
      var vKingPos = Friend.GetKingPos();

      clearPseudoMaterialMoveLists(moves);
#if UnshadowRay2
      var bRayCheck = false;
#endif
      if (bInCheck) {
        var qpKing = Friend.Piece & King;
        var qpChx = Foe.Checkers(vKingPos, qpKing);
#if UnshadowRay
        bRayCheck = (qpChx & (DiagPiece | RectPiece)) != 0;
#endif
        var qpChx2 = qpChx;
        var nChx = RemoveLo(ref qpChx2);
        var bSingleCheck = qpChx2 == 0;

        if (bSingleCheck) {
          //
          // Interposition includes capture of the checking
          // piece, and interpositions along a ray giving check.
          // Only Moves for these To Squares will be considered:
          //
          var qpRay = interpositions(nChx, vKingPos);

          if (qpChx != 0) {
            addPieceCaptures(qpChx);
            Friend.AddPawnCaptures(this, includeEnPassant(qpChx));
            Friend.AddPromotions(this, qpRay);
          }
        }                               // bSingleCheck
      }                                 //!bInCheck
      else {
        addPieceCaptures(qpFoe);
        Friend.AddPawnCaptures(this, includeEnPassant(qpFoe));
        Friend.AddPromotions(this, ~RankPiece);
      }                                 //!bInCheck
#if UnshadowRay2
      addKingCaptures(qpFoe, vKingPos, bRayCheck);
#else
      var bWTM = WTM();
      addKingCaptures(qpFoe, vKingPos, bWTM);
#endif
      addPseudoMaterialMoves(moves);
      return State!.IncPseudoMoveTotal(moves.Count);
    }
    #endregion

    #region Swap Move Generator
    protected Int32 generateSwaps(List<Move> moves, Int32 nTo) {
      var qpTo = BIT0 << nTo;
      var qpFriend = Friend.Piece;
      var vKingPos = Friend.GetKingPos();
      var bInCheck = InCheck();

      clearPseudoSwapLists(moves);      // ~32 MHz
#if UnshadowRay2
      var bRayCheck = false;
#endif
      if (bInCheck) {
        var qpKing = qpFriend & King;
        var qpChx = Foe.Checkers(vKingPos, qpKing);
#if UnshadowRay
        bRayCheck = (qpChx & (DiagPiece | RectPiece)) != 0;
#endif
        var bSingleCheck = IsOneOrNone(qpChx);
        if (bSingleCheck) {
          var qpFoe = qpChx & qpTo;

          if (qpFoe != 0) {
            addPieceCaptures(qpFoe);
            Friend.AddPawnCaptures(this, qpFoe);
          }
        }                               // bSingleCheck
      }                                 //!bInCheck
      else {
        var qpFoe = qpTo & ~qpFriend;
        addPieceCaptures(qpFoe);
        Friend.AddPawnCaptures(this, qpFoe);
      }                                 //!bInCheck
#if UnshadowRay2
      addKingCaptures(qpTo & ~qpFriend, vKingPos, bRayCheck);
#else
      addKingCaptures(qpTo & ~qpFriend, vKingPos);
#endif
      addPseudoSwaps(moves);
      return State!.IncPseudoMoveTotal(moves.Count);
    }
    #endregion

    #region Pseudo Move Pre-Sort
    //
    // Arrange moves in a reasonable order, whether or not UseMoveSort is in effect:
    //
    protected void addPseudoMoves(List<Move> moves) {
      Expand(MoveTypes, MoveTypeOrdering);

      foreach (var moveType in MoveTypes) {
        switch (moveType) {
        case MoveType.PawnAboveCapture:
          moves.AddRange(PseudoQueenPromotionCapture);
          moves.AddRange(PseudoUnderPromotionCapture);
          moves.AddRange(PseudoEPCapture);              //[Note]Legality is assessed via tryMoves()
          moves.AddRange(PseudoPawnAboveCapture);
          break;
        case MoveType.PawnBelowCapture:
          moves.AddRange(PseudoPawnBelowCapture);
          break;
        case MoveType.KnightCapture:
          moves.AddRange(PseudoKnightCapture);
          break;
        case MoveType.KingCapture:
          moves.AddRange(PseudoKingCapture);
          break;
        case MoveType.DiagAboveCapture:
          moves.AddRange(PseudoDiagAboveCapture);
          break;
        case MoveType.DiagBelowCapture:
          moves.AddRange(PseudoDiagBelowCapture);
          break;
        case MoveType.RectAboveCapture:
          moves.AddRange(PseudoRectAboveCapture);
          break;
        case MoveType.RectBelowCapture:
          moves.AddRange(PseudoRectBelowCapture);
          break;
        case MoveType.PawnAboveMove:
          moves.AddRange(PseudoQueenPromotion);
          moves.AddRange(PseudoUnderPromotion);
          moves.AddRange(PseudoPawnAboveMove);
          break;
        case MoveType.PawnBelowMove:
          moves.AddRange(PseudoPawnBelowMove);
          break;
        case MoveType.KnightMove:
          moves.AddRange(PseudoKnightMove);
          break;
        case MoveType.KingMove:
          moves.AddRange(PseudoCastles);
          moves.AddRange(PseudoKingMove);
          break;
        case MoveType.DiagAboveMove:
          moves.AddRange(PseudoDiagAboveMove);
          break;
        case MoveType.DiagBelowMove:
          moves.AddRange(PseudoDiagBelowMove);
          break;
        case MoveType.RectAboveMove:
          moves.AddRange(PseudoRectAboveMove);
          break;
        case MoveType.RectBelowMove:
          moves.AddRange(PseudoRectBelowMove);
          break;
        default:
          throw new PositionException("Unexpected MoveType");
        }
      }
    }

    protected void addPseudoMovesGood(List<Move> moves) {
      //expandMoveTypeOrdering();
      var captures = PseudoCaptures;

      captures.AddRange(PseudoPawnAboveCapture);        // 2a
      captures.AddRange(PseudoPawnBelowCapture);        // 2b
      captures.AddRange(PseudoKnightCapture);           // 4
      captures.AddRange(PseudoDiagAboveCapture);        // 3d
      captures.AddRange(PseudoDiagBelowCapture);        // 5
      captures.AddRange(PseudoRectAboveCapture);        // 3r
      captures.AddRange(PseudoRectBelowCapture);        // 6
      captures.AddRange(PseudoEPCapture);               //[Note]Legality is assessed via tryMoves()

      sortSwaps(captures);

      //
      //[Note]In certain positions adding King Moves after the
      // Piece Moves improved performance by a factor of three
      //
      // Establish Move Priorities:
      //                                                // Best Cutoff Order
      moves.AddRange(PseudoQueenPromotionCapture);      // 1A
      moves.AddRange(PseudoQueenPromotion);             // 1B
      moves.AddRange(PseudoUnderPromotionCapture);      // 1C

      moves.AddRange(PseudoGoodCaptures);               // 2-6 Good

      moves.AddRange(PseudoDiagAboveMove);              // 9d
      moves.AddRange(PseudoDiagBelowMove);              // 11
      moves.AddRange(PseudoRectAboveMove);              // 9r
      moves.AddRange(PseudoRectBelowMove);              // 12
      moves.AddRange(PseudoKnightMove);                 // 10
      moves.AddRange(PseudoKingCapture);                // 7
      moves.AddRange(PseudoCastles);                    // 13
      moves.AddRange(PseudoKingMove);                   // 14
      moves.AddRange(PseudoPawnAboveMove);              // 15a
      moves.AddRange(PseudoPawnBelowMove);              // 15b
      moves.AddRange(PseudoUnderPromotion);             // 16

      moves.AddRange(PseudoBadCaptures);                // 8B
    }

    protected void addPseudoSwaps(List<Move> moves) {
      //expandMoveTypeOrdering();
      moves.AddRange(PseudoQueenPromotionCapture);      // 1A
      //moves.AddRange(PseudoUnderPromotionCapture);    // 1C Not needed for Swaps
      moves.AddRange(PseudoPawnAboveCapture);           // 2a
      moves.AddRange(PseudoPawnBelowCapture);           // 2b
      moves.AddRange(PseudoKingCapture);                // 7
      moves.AddRange(PseudoKnightCapture);              // 4
      moves.AddRange(PseudoDiagAboveCapture);           // 5
      moves.AddRange(PseudoDiagBelowCapture);           // 3d
      moves.AddRange(PseudoRectAboveCapture);           // 6
      moves.AddRange(PseudoRectBelowCapture);           // 3r
      //moves.AddRange(PseudoEPCapture);                // Not needed for Swaps
    }

    protected void addPseudoMaterialMoves(List<Move> moves) {
      //expandMoveTypeOrdering();
      moves.AddRange(PseudoPawnAboveCapture);           // 2a
      moves.AddRange(PseudoPawnBelowCapture);           // 2b
      moves.AddRange(PseudoKnightCapture);              // 4
      moves.AddRange(PseudoKingCapture);                // 7
      moves.AddRange(PseudoDiagAboveCapture);           // 3d
      moves.AddRange(PseudoDiagBelowCapture);           // 5
      moves.AddRange(PseudoRectAboveCapture);           // 3r
      moves.AddRange(PseudoRectBelowCapture);           // 6
      moves.AddRange(PseudoEPCapture);
      moves.AddRange(PseudoQueenPromotionCapture);      // 1A
      moves.AddRange(PseudoQueenPromotion);             // 1B
      moves.AddRange(PseudoUnderPromotionCapture);      // 1C
      moves.AddRange(PseudoUnderPromotion);             // 16
    }
    #endregion
  }
}
