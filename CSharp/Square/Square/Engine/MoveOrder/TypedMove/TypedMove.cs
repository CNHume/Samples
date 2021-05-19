//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2017-08-13 CNHume]Added Class
//
// Conditionals:
//
namespace Engine.MoveOrder {
  using static Board;

  using System;

  //
  // Type Aliases:
  //
  using MoveTypeOrdering = System.UInt64;
  using Engine.Exceptions;

  static class TypedMove {
    #region MoveType Enum
    public enum MoveType : byte {
      PawnAboveCapture,
      PawnBelowCapture,
      DiagAboveCapture,
      DiagBelowCapture,
      RectAboveCapture,
      RectBelowCapture,
      KnightCapture,
      KingCapture,

      KnightMove,
      DiagAboveMove,
      DiagBelowMove,
      RectAboveMove,
      RectBelowMove,
      KingMove,
      PawnAboveMove,
      PawnBelowMove,
    }
    #endregion

    #region Static Fields
    public static readonly MoveType[] DefaultMoveTypes;
    public static readonly MoveTypeOrdering DefaultMoveTypeOrdering;
    #endregion

    #region Constructors
    static TypedMove() {
      DefaultMoveTypes = (MoveType[])Enum.GetValues(typeof(MoveType));
      DefaultMoveTypeOrdering = Compress(DefaultMoveTypes);
    }
    #endregion

    #region Methods
    public static MoveType moveType(Move move, Boolean bWTM) {
      MoveType type;
      unpack1(move, out Int32 nFrom, out Int32 nTo,
              out UInt32 uPiece, out Boolean bCapture);
      var bAbove = isAbove(nTo, bWTM);
      var vPiece = pieceIndex(uPiece);
      switch (vPiece) {
      case vP6:
        type = bCapture ?
          bAbove ? MoveType.PawnAboveCapture : MoveType.PawnBelowCapture :
          bAbove ? MoveType.PawnAboveMove : MoveType.PawnBelowMove;
        break;

      case vN6:
        type = bCapture ? MoveType.KnightCapture : MoveType.KnightMove;
        break;

      case vB6:
        type = bCapture ?
          bAbove ? MoveType.DiagAboveCapture : MoveType.DiagBelowCapture :
          bAbove ? MoveType.DiagAboveMove : MoveType.DiagBelowMove;
        break;

      case vR6:
        type = bCapture ?
          bAbove ? MoveType.RectAboveCapture : MoveType.RectBelowCapture :
          bAbove ? MoveType.RectAboveMove : MoveType.RectBelowMove;
        break;

      case vQ6:
        type = isRect(nFrom, nTo) ? bCapture ?
          bAbove ? MoveType.RectAboveCapture : MoveType.RectBelowCapture :
          bAbove ? MoveType.RectAboveMove : MoveType.RectBelowMove :
        bCapture ?
          bAbove ? MoveType.DiagAboveCapture : MoveType.DiagBelowCapture :
          bAbove ? MoveType.DiagAboveMove : MoveType.DiagBelowMove;
        break;

      case vK6:
        type = bCapture ? MoveType.KingCapture : MoveType.KingMove;
        break;

      default:
        throw new PieceException("Unexpected Piece [moveType]");
      }

      return type;
    }

    public static MoveTypeOrdering Compress(MoveType[] moveTypes) {
      MoveTypeOrdering ordering = 0;
      var nFinal = moveTypes.Length - 1;
      for (var n = 0; nFinal >= 0; n++, nFinal--) {
        var moveType = (MoveTypeOrdering)nibble((Int32)moveTypes[nFinal]);
        ordering <<= nPerNibble;
        ordering |= moveType;
      }
      return ordering;
    }

    public static void Expand(MoveType[] moveTypes, MoveTypeOrdering ordering) {
      for (var n = 0; n < moveTypes.Length; n++, ordering >>= nPerNibble) {
        var moveType = (MoveType)nibble((Int32)ordering);
        moveTypes[n] = moveType;
      }
    }
    #endregion
  }
}
