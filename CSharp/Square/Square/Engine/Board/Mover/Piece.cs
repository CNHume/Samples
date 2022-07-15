//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split Piece into its own file
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

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Board {
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
    #endregion

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
      var qpBoth = Side[White].Piece & Side[Black].Piece;
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

      var qpColor = Side[White].Piece | Side[Black].Piece;
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
#if UnshadowRay
    //
    // The following are called to remove and replace a King from the
    // board to unshadow its destination squares from any ray attacks.
    //
    //[Warning]The Hash is not updated during this interval.
    //
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void clrRayState(Int32 nFrom) {
      var qp = BIT0 << nFrom;
      RankPiece &= ~qp;
#if !Magic
      ClrRotations(nFrom);
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void setRayState(Int32 nTo) {
      var qp = BIT0 << nTo;
      RankPiece |= qp;
#if !Magic
      SetRotations(nTo);
#endif
    }
#endif
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
    public virtual void Clear() {
      foreach (var side in Side)
        side.Clear();

      RankPiece = Pawn = King = Knight = DiagPiece = RectPiece = 0UL;
#if !Magic
      A1H8Piece = A8H1Piece = FilePiece = 0UL;
#endif
      HashPawn = Hash = 0UL;
    }
    #endregion
  }
}
