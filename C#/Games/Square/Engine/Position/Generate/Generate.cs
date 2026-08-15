//
// Copyright (C) 2010-2026, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split PseudoMove Generation into this file
//
// Conditionals:
//
#define DebugEPTarget
#define RemoveKingShadow
#define RemoveKingShadow2               // RemoveKingShadow2 <= RemoveKingShadow

using System.Runtime.CompilerServices;

namespace Engine;

using Exceptions;

using static MoveType;

//
// Type Aliases:
//
using Plane = UInt64;

partial class Position : Board {
  #region Clear Pseudo Moves
  private void clearPseudoMoves() {
    PseudoPawnAboveMove.Clear();
    PseudoPawnBelowMove.Clear();
    PseudoKnightMove.Clear();
    PseudoKingMove.Clear();
    PseudoDiagAboveMove.Clear();
    PseudoDiagBelowMove.Clear();
    PseudoOrthAboveMove.Clear();
    PseudoOrthBelowMove.Clear();
  }

  private void clearPseudoCaptures() {
    PseudoPawnAboveCapture.Clear();
    PseudoPawnBelowCapture.Clear();
    PseudoKnightCapture.Clear();
    PseudoKingCapture.Clear();
    PseudoDiagAboveCapture.Clear();
    PseudoDiagBelowCapture.Clear();
    PseudoOrthAboveCapture.Clear();
    PseudoOrthBelowCapture.Clear();
  }

  //
  //[Warning]Duplicate Moves can result in a number of strange, difficult to debug side-effects.
  //
  private void clearPseudoMoveLists(List<Move> moves) {  // ~32 MHz
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
  }

  private void clearPseudoMaterialMoveLists(List<Move> moves) {
    moves.Clear();
    clearPseudoCaptures();

    PseudoEPCapture.Clear();
    PseudoQueenPromotion.Clear();
    PseudoUnderPromotion.Clear();
    PseudoQueenPromotionCapture.Clear();
    PseudoUnderPromotionCapture.Clear();

    PseudoCaptures.Clear();
  }
  #endregion                            // Clear Pseudo Moves

  #region Search Move Generators
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Plane includeEPTarget(Plane qpFoe) {
    const string message = "No En Passant Square";
    if (IsEPLegal()) {
      if (EPTarget.HasValue)
        return qpFoe | bit(EPTarget.Value);
#if DebugEPTarget
      DisplayCurrent(message);
#endif                                  // DebugEPTarget
    }

    return qpFoe;
  }

  private Int32 generate(List<Move> moves) {
    var bInCheck = InCheck();
    var vKingPos = Friend.GetKingPos();

    clearPseudoMoveLists(moves);
#if RemoveKingShadow2
    var bRayCheck = false;
#endif
    if (bInCheck) {
      var qpChx = Foe.Checkers(vKingPos, Friend.Piece & King);
#if RemoveKingShadow
      bRayCheck = (qpChx & (DiagPiece | OrthPiece)) != 0;
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

        Friend.AddPawnCaptures(includeEPTarget(qpChx));
        Friend.AddPawnMoves(this, qpRay);
      }                                 // bSingleCheck
    }
    else {                              //!bInCheck
      addPieceCapturesAndMoves(~Friend.Piece);
      Friend.AddPawnCaptures(includeEPTarget(Foe.Piece));
      Friend.AddPawnMoves(this, ~RankPiece);

      addCastles();
    }                                   //!bInCheck
#if RemoveKingShadow2
    addKingCapturesAndMoves(~Friend.Piece, vKingPos, bRayCheck);
#else
    addKingCapturesAndMoves(~Friend.Piece, vKingPos);
#endif
    addPseudoMovesByTypeOrdering(moves);

    return State.IncPseudoMoveTotal(moves.Count);
  }
  #endregion

  #region Quiet Move Generator
  private Int32 generateMaterialMoves(List<Move> moves) {
    var bInCheck = InCheck();
    var vKingPos = Friend.GetKingPos();

    clearPseudoMaterialMoveLists(moves);
#if RemoveKingShadow2
    var bRayCheck = false;
#endif
    if (bInCheck) {
      var qpChx = Foe.Checkers(vKingPos, Friend.Piece & King);
#if RemoveKingShadow
      bRayCheck = (qpChx & (DiagPiece | OrthPiece)) != 0;
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
          Friend.AddPawnCaptures(includeEPTarget(qpChx));
          Friend.AddPromotionMoves(qpRay);
        }
      }                                 // bSingleCheck
    }                                   //!bInCheck
    else {
      addPieceCaptures(Foe.Piece);
      Friend.AddPawnCaptures(includeEPTarget(Foe.Piece));
      Friend.AddPromotionMoves(~RankPiece);
    }                                   //!bInCheck
#if RemoveKingShadow2
    addKingCaptures(Foe.Piece, vKingPos, bRayCheck);
#else
    addKingCaptures(Foe.Piece, vKingPos);
#endif
    addPseudoMaterialMoves(moves);
    return State.IncPseudoMoveTotal(moves.Count);
  }
  #endregion

  #region Pseudo Move Pre-Sort
  //
  // Order moves based on moveTypeOrdering:
  //
  private void addPseudoMovesByTypeOrdering(List<Move> moves) {
    expandMoveTypes(moveTypes, moveTypeOrdering);

    foreach (var moveType in moveTypes) {
      switch (moveType) {
      case PawnAboveCapture:
        moves.AddRange(PseudoQueenPromotionCapture);
        moves.AddRange(PseudoUnderPromotionCapture);
        moves.AddRange(PseudoEPCapture);        //[Note]Legality is assessed via tryMoves()
        moves.AddRange(PseudoPawnAboveCapture);
        break;
      case PawnBelowCapture:
        moves.AddRange(PseudoPawnBelowCapture);
        break;
      case KnightCapture:
        moves.AddRange(PseudoKnightCapture);
        break;
      case KingCapture:
        moves.AddRange(PseudoKingCapture);
        break;
      case DiagAboveCapture:
        moves.AddRange(PseudoDiagAboveCapture);
        break;
      case DiagBelowCapture:
        moves.AddRange(PseudoDiagBelowCapture);
        break;
      case OrthAboveCapture:
        moves.AddRange(PseudoOrthAboveCapture);
        break;
      case OrthBelowCapture:
        moves.AddRange(PseudoOrthBelowCapture);
        break;
      case PawnAboveMove:
        moves.AddRange(PseudoQueenPromotion);
        moves.AddRange(PseudoUnderPromotion);
        moves.AddRange(PseudoPawnAboveMove);
        break;
      case PawnBelowMove:
        moves.AddRange(PseudoPawnBelowMove);
        break;
      case KnightMove:
        moves.AddRange(PseudoKnightMove);
        break;
      case KingMove:
        moves.AddRange(PseudoCastles);
        moves.AddRange(PseudoKingMove);
        break;
      case DiagAboveMove:
        moves.AddRange(PseudoDiagAboveMove);
        break;
      case DiagBelowMove:
        moves.AddRange(PseudoDiagBelowMove);
        break;
      case OrthAboveMove:
        moves.AddRange(PseudoOrthAboveMove);
        break;
      case OrthBelowMove:
        moves.AddRange(PseudoOrthBelowMove);
        break;
      default:
        throw new PositionException("Unexpected MoveType");
      }
    }
  }

  private void addPseudoMaterialMoves(List<Move> moves) {
    //expandMoveTypeOrdering();
    moves.AddRange(PseudoPawnAboveCapture);           // 2a
    moves.AddRange(PseudoPawnBelowCapture);           // 2b
    moves.AddRange(PseudoKnightCapture);              // 4
    moves.AddRange(PseudoKingCapture);                // 7
    moves.AddRange(PseudoDiagAboveCapture);           // 3d
    moves.AddRange(PseudoDiagBelowCapture);           // 5
    moves.AddRange(PseudoOrthAboveCapture);           // 3r
    moves.AddRange(PseudoOrthBelowCapture);           // 6
    moves.AddRange(PseudoEPCapture);
    moves.AddRange(PseudoQueenPromotionCapture);      // 1A
    moves.AddRange(PseudoQueenPromotion);             // 1B
    moves.AddRange(PseudoUnderPromotionCapture);      // 1C
    moves.AddRange(PseudoUnderPromotion);             // 16
  }
  #endregion
}
