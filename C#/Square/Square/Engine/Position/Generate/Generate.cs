//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split PseudoMove Generation into this file
//
// Conditionals:
//
#define DebugEPTarget
#define RemoveKingShadow
#define RemoveKingShadow2               // RemoveKingShadow2 <= RemoveKingShadow
#define UseMoveSort

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
  private void clearPseudoMoveLists(List<Move> moves, Boolean bSwap) {  // ~32 MHz
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

  // Called by generateSwaps()
  private void clearPseudoSwapLists(List<Move> moves) {
    moves.Clear();
    clearPseudoCaptures();

    PseudoQueenPromotionCapture.Clear();
    PseudoUnderPromotionCapture.Clear();
    //PseudoEPCapture.Clear();            // Not needed for Swaps

    PseudoBadCaptures.Clear();
    PseudoGoodCaptures.Clear();
  }
  #endregion                            // Clear Pseudo Moves

  #region Search Move Generators
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Plane includeEPTarget(Plane qpFoe) {
    if (IsEPLegal()) {
      if (EPTarget.HasValue)
        return qpFoe | bit(EPTarget.Value);
#if DebugEPTarget
      var sMessage = "No En Passant Square";
      DisplayCurrent(sMessage);
#endif                                  // DebugEPTarget
    }

    return qpFoe;
  }

  private Int32 generate(List<Move> moves, Boolean bSwap) {
    var bInCheck = InCheck();
    var vKingPos = Friend.GetKingPos();

    clearPseudoMoveLists(moves, bSwap);
#if RemoveKingShadow2
    var bRayCheck = false;
#endif
    if (bInCheck) {
      var qpKing = Friend.Piece & King;
      var qpChx = Foe.Checkers(vKingPos, qpKing);
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
    if (bSwap && !bInCheck)
      addPseudoMovesGood(moves);
    else
      addPseudoMoves(moves);

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
      var qpKing = Friend.Piece & King;
      var qpChx = Foe.Checkers(vKingPos, qpKing);
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

  #region Swap Move Generator
  private Int32 generateSwaps(List<Move> moves, Int32 nTo) {
    var qpTo = bit(nTo);
    var vKingPos = Friend.GetKingPos();
    var bInCheck = InCheck();

    clearPseudoSwapLists(moves);        // ~32 MHz
#if RemoveKingShadow2
    var bRayCheck = false;
#endif
    if (bInCheck) {
      var qpKing = Friend.Piece & King;
      var qpChx = Foe.Checkers(vKingPos, qpKing);
#if RemoveKingShadow
      bRayCheck = (qpChx & (DiagPiece | OrthPiece)) != 0;
#endif
      var bSingleCheck = IsOneOrLess(qpChx);
      if (bSingleCheck) {
        var qpFoe = qpChx & qpTo;

        if (qpFoe != 0) {
          addPieceCaptures(qpFoe);
          Friend.AddPawnCaptures(qpFoe);
        }
      }                                 // bSingleCheck
    }                                   //!bInCheck
    else {
      var qpFoe = qpTo & ~Friend.Piece;
      addPieceCaptures(qpFoe);
      Friend.AddPawnCaptures(qpFoe);
    }                                   //!bInCheck
#if RemoveKingShadow2
    addKingCaptures(qpTo & ~Friend.Piece, vKingPos, bRayCheck);
#else
    addKingCaptures(qpTo & ~Friend.Piece, vKingPos);
#endif
    addPseudoSwaps(moves);
    return State.IncPseudoMoveTotal(moves.Count);
  }
  #endregion

  #region Pseudo Move Pre-Sort
  //
  // Arrange moves in a reasonable order, whether or not UseMoveSort is in effect:
  //
  private void addPseudoMoves(List<Move> moves) {
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

  private void addPseudoMovesGood(List<Move> moves) {
    //expandMoveTypeOrdering();
    var captures = PseudoCaptures;

    captures.AddRange(PseudoPawnAboveCapture);      // 2a
    captures.AddRange(PseudoPawnBelowCapture);      // 2b
    captures.AddRange(PseudoKnightCapture);         // 4
    captures.AddRange(PseudoDiagAboveCapture);      // 3d
    captures.AddRange(PseudoDiagBelowCapture);      // 5
    captures.AddRange(PseudoOrthAboveCapture);      // 3r
    captures.AddRange(PseudoOrthBelowCapture);      // 6
    captures.AddRange(PseudoEPCapture);             //[Note]Legality is assessed via tryMoves()

    sortSwaps(captures);

    //
    //[Note]In certain positions adding King Moves after the
    // Piece Moves improved performance by a factor of three
    //
    // Establish Move Priorities:
    //                                              // Best Cutoff Order
    moves.AddRange(PseudoQueenPromotionCapture);    // 1A
    moves.AddRange(PseudoQueenPromotion);           // 1B
    moves.AddRange(PseudoUnderPromotionCapture);    // 1C

    moves.AddRange(PseudoGoodCaptures);             // 2-6 Good

    moves.AddRange(PseudoDiagAboveMove);            // 9d
    moves.AddRange(PseudoDiagBelowMove);            // 11
    moves.AddRange(PseudoOrthAboveMove);            // 9r
    moves.AddRange(PseudoOrthBelowMove);            // 12
    moves.AddRange(PseudoKnightMove);               // 10
    moves.AddRange(PseudoKingCapture);              // 7
    moves.AddRange(PseudoCastles);                  // 13
    moves.AddRange(PseudoKingMove);                 // 14
    moves.AddRange(PseudoPawnAboveMove);            // 15a
    moves.AddRange(PseudoPawnBelowMove);            // 15b
    moves.AddRange(PseudoUnderPromotion);           // 16

    moves.AddRange(PseudoBadCaptures);              // 8B
  }

  private void addPseudoSwaps(List<Move> moves) {
    //expandMoveTypeOrdering();
    moves.AddRange(PseudoQueenPromotionCapture);    // 1A
                                                    //moves.AddRange(PseudoUnderPromotionCapture);
                                                    // 1C Not needed for Swaps
    moves.AddRange(PseudoPawnAboveCapture);         // 2a
    moves.AddRange(PseudoPawnBelowCapture);         // 2b
    moves.AddRange(PseudoKingCapture);              // 7
    moves.AddRange(PseudoKnightCapture);            // 4
    moves.AddRange(PseudoDiagAboveCapture);         // 5
    moves.AddRange(PseudoDiagBelowCapture);         // 3d
    moves.AddRange(PseudoOrthAboveCapture);         // 6
    moves.AddRange(PseudoOrthBelowCapture);         // 3r
                                                    //moves.AddRange(PseudoEPCapture);
                                                    // Not needed for Swaps
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
