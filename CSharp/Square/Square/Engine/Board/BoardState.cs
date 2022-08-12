//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2022-08-11 CNHume]Renamed file to BoardState
//
// Conditionals:
//
//#define Magic
#define HashPieces
#define UnshadowRay

namespace Engine {
  using Exceptions;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute
  using System.Text;

  partial class Board {
    #region Methods
    //
    // getPieceIndex
    // verifyPieceColors
    //
    // oppositeBishops
    // sameBishops
    // hasBishopPair
    //
    // [Clr|Set][RayState|Rotations]
    //
    #region Square Pieces
    protected Byte getPieceIndex(Int32 n) {
      var vPiece = vPieceNull;          // Return Value
      var qp = BIT0 << n;

      //
      //[Speed]This is most often called to identify a captured piece.
      // Pawns require only two tests.  A third test is sufficient to
      // identify any other piece or an empty square.
      //
      if ((qp & (Pawn | Knight | King)) != 0) {
        if ((qp & Pawn) != 0)
          vPiece = vP6;
        else if ((qp & Knight) != 0)
          vPiece = vN6;
        else //if ((qp & King) != 0)
          vPiece = vK6;
      }
      else if ((qp & RectPiece) != 0) {
        if ((qp & DiagPiece) != 0)
          vPiece = vQ6;
        else
          vPiece = vR6;
      }
      else if ((qp & DiagPiece) != 0)
        vPiece = vB6;

      return vPiece;
    }

    [Conditional("VerifyPieceColor")]
    protected void verifyPieceColors() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      var qpBoth = blackSide.Piece & whiteSide.Piece;
      if (qpBoth != 0) {
        if ((qpBoth & RankPiece) != 0) {
          var sb = new StringBuilder("Pieces marked with Both Colors at")
            .AppendSquares(qpBoth & RankPiece);
          throw new ColorException(sb.ToString());
        }

        if ((qpBoth & ~RankPiece) != 0) {
          var sb = new StringBuilder("Empty Squares marked with Both Colors at")
            .AppendSquares(qpBoth & ~RankPiece);
          throw new ColorException(sb.ToString());
        }
      }

      var qpColor = blackSide.Piece | whiteSide.Piece;
      if (qpColor != RankPiece) {
        if ((RankPiece & ~qpColor) != 0) {
          var sb = new StringBuilder("Uncolored Pieces at")
            .AppendSquares(RankPiece & ~qpColor);
          throw new ColorException(sb.ToString());
        }

        foreach (var side in Side) {
          if ((side.Piece & ~RankPiece) != 0) {
            var sb = new StringBuilder($"Empty Squares marked as {side.Parameter.SideName} Pieces at")
              .AppendSquares(side.Piece & ~RankPiece);
            throw new ColorException(sb.ToString());
          }
        }
      }
    }
    #endregion                          // Square Pieces

    #region Bishop Tests
    protected static Boolean oppositeBishops(SideFlags fBlackSide, SideFlags fWhiteSide) {
      var blackPair = fBlackSide & SideFlags.Pair;
      var whitePair = fWhiteSide & SideFlags.Pair;

      return (whitePair == SideFlags.Lite && blackPair == SideFlags.Dark) ||
             (whitePair == SideFlags.Dark && blackPair == SideFlags.Lite);
    }

    protected static Boolean sameBishops(SideFlags fBlackSide, SideFlags fWhiteSide) {
      var blackPair = fBlackSide & SideFlags.Pair;
      var whitePair = fWhiteSide & SideFlags.Pair;

      return (whitePair == SideFlags.Lite && blackPair == SideFlags.Lite) ||
             (whitePair == SideFlags.Dark && blackPair == SideFlags.Dark);
    }

    protected static Boolean hasBishopPair(SideFlags fside) {
      return (fside & SideFlags.Pair) == SideFlags.Pair;
    }
    #endregion                          // Bishop Tests

    #region Rotations
#if !Magic
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal void ClrRotations(Int32 n) {
      FilePiece &= ~FileBit[n];
      A1H8Piece &= ~A1H8Bit[n];
      A8H1Piece &= ~A8H1Bit[n];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal void SetRotations(Int32 n) {
      FilePiece |= FileBit[n];
      A1H8Piece |= A1H8Bit[n];
      A8H1Piece |= A8H1Bit[n];
    }
#endif
#if UnshadowRay
    //
    // The following are called to remove and replace a King from the
    // board to unshadow its destination squares from any ray attacks.
    //
    //[Warning]The Hash is not updated during this interval.
    //
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void ClrRayState(Int32 nFrom) {
      var qp = BIT0 << nFrom;
      RankPiece &= ~qp;
#if !Magic
      ClrRotations(nFrom);
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void SetRayState(Int32 nTo) {
      var qp = BIT0 << nTo;
      RankPiece |= qp;
#if !Magic
      SetRotations(nTo);
#endif
    }
#endif                                  // UnshadowRay
    #endregion                          // Rotations
    #endregion                          // Methods
  }
}
