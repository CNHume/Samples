//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2017-08-13 CNHume]Added Class
//
// Conditionals:
//

namespace Engine;

using Exceptions;

//
// Type Aliases:
//
using MoveTypeOrdering = UInt64;

partial class Position {
  #region Methods
  private static MoveType moveType(
    Int32 nFrom, Int32 nTo, UInt32 uPiece, Boolean bCapture, Boolean bAbove) {
    const string methodName = nameof(moveType);
    var vPiece = PieceIndex(uPiece);
    var type = vPiece switch {
      vP6 => bCapture ?
        bAbove ? MoveType.PawnAboveCapture : MoveType.PawnBelowCapture :
        bAbove ? MoveType.PawnAboveMove : MoveType.PawnBelowMove,
      vB6 => bCapture ?
        bAbove ? MoveType.DiagAboveCapture : MoveType.DiagBelowCapture :
        bAbove ? MoveType.DiagAboveMove : MoveType.DiagBelowMove,
      vR6 => bCapture ?
        bAbove ? MoveType.OrthAboveCapture : MoveType.OrthBelowCapture :
        bAbove ? MoveType.OrthAboveMove : MoveType.OrthBelowMove,
      vQ6 => IsOrth(nFrom, nTo) ?
        bCapture ?
          bAbove ? MoveType.OrthAboveCapture : MoveType.OrthBelowCapture :
          bAbove ? MoveType.OrthAboveMove : MoveType.OrthBelowMove :
        bCapture ?
          bAbove ? MoveType.DiagAboveCapture : MoveType.DiagBelowCapture :
          bAbove ? MoveType.DiagAboveMove : MoveType.DiagBelowMove,
      vN6 => bCapture ? MoveType.KnightCapture : MoveType.KnightMove,
      vK6 => bCapture ? MoveType.KingCapture : MoveType.KingMove,
      _ => throw new PieceException($"Unexpected Piece = {vPiece} [{methodName}]"),
    };

    return type;
  }

  private static MoveTypeOrdering compressMoveTypes(MoveType[] moveTypes) {
    MoveTypeOrdering ordering = 0;
    var nFinal = moveTypes.Length - 1;
    for (var n = 0; nFinal >= 0; n++, nFinal--) {
      var moveType = (MoveTypeOrdering)Nibble((int)moveTypes[nFinal]);
      ordering <<= nPerNibble;
      ordering |= moveType;
    }
    return ordering;
  }

  private static void expandMoveTypes(MoveType[] moveTypes, MoveTypeOrdering ordering) {
    for (var n = 0; n < moveTypes.Length; n++, ordering >>= nPerNibble) {
      var moveType = (MoveType)Nibble((int)ordering);
      moveTypes[n] = moveType;
    }
  }
  #endregion
}
