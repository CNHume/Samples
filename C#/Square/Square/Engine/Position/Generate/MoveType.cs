//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2017-08-13 CNHume]Added Class
//
// Conditionals:
//

namespace Engine;

using Exceptions;

using static MoveType;

//
// Type Aliases:
//
using MoveTypeOrdering = UInt64;

partial class Position : Board {
  #region Methods
  private static MoveType moveType(
    Int32 nFrom, Int32 nTo, UInt32 uPiece, Boolean bCapture, Boolean bAbove) {
    const String methodName = nameof(moveType);
    var vPiece = PieceIndex(uPiece);
    var type = vPiece switch {
      vP6 => bCapture ?
        bAbove ? PawnAboveCapture : PawnBelowCapture :
        bAbove ? PawnAboveMove : PawnBelowMove,
      vB6 => bCapture ?
        bAbove ? DiagAboveCapture : DiagBelowCapture :
        bAbove ? DiagAboveMove : DiagBelowMove,
      vR6 => bCapture ?
        bAbove ? OrthAboveCapture : OrthBelowCapture :
        bAbove ? OrthAboveMove : OrthBelowMove,
      vQ6 => IsOrth(nFrom, nTo) ?
        bCapture ?
          bAbove ? OrthAboveCapture : OrthBelowCapture :
          bAbove ? OrthAboveMove : OrthBelowMove :
        bCapture ?
          bAbove ? DiagAboveCapture : DiagBelowCapture :
          bAbove ? DiagAboveMove : DiagBelowMove,
      vN6 => bCapture ? KnightCapture : KnightMove,
      vK6 => bCapture ? KingCapture : KingMove,
      _ => throw new PieceException($"Unexpected Piece = {vPiece} [{methodName}]"),
    };

    return type;
  }

  private static MoveTypeOrdering compressMoveTypes(MoveType[] moveTypes) {
    MoveTypeOrdering ordering = 0;
    for (var nLimit = moveTypes.Length; nLimit > 0; nLimit--) {
      var moveType = (MoveTypeOrdering)Nibble((Int32)moveTypes[nLimit - 1]);
      ordering <<= nPerNibble;
      ordering |= moveType;
    }
    return ordering;
  }

  private static void expandMoveTypes(MoveType[] moveTypes, MoveTypeOrdering ordering) {
    for (var n = 0; n < moveTypes.Length; n++, ordering >>= nPerNibble) {
      var moveType = (MoveType)Nibble((Int32)ordering);
      moveTypes[n] = moveType;
    }
  }
  #endregion                            // Methods
}
