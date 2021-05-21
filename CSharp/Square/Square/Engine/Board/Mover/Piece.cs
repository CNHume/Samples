//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split Piece into its own file
//
// Conditionals:
//
//#define Magic
//#define VerifySquarePiece               // Ensure move from an occupied square to an empty square
#define HashPieces
#define UnshadowRay

namespace Engine {
  using Command.Exceptions;

  using Exceptions;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute
  using System.Text;

  using static CastleRule;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class Board {
    #region Bishop Tests
    protected static Boolean oppositeBishops(HiFlags fBlackHi, HiFlags fWhiteHi) {
      var blackPair = fBlackHi & HiFlags.Pair;
      var whitePair = fWhiteHi & HiFlags.Pair;

      return (whitePair == HiFlags.Lite && blackPair == HiFlags.Dark) ||
             (whitePair == HiFlags.Dark && blackPair == HiFlags.Lite);
    }

    protected static Boolean sameBishops(HiFlags fBlackHi, HiFlags fWhiteHi) {
      var blackPair = fBlackHi & HiFlags.Pair;
      var whitePair = fWhiteHi & HiFlags.Pair;

      return (whitePair == HiFlags.Lite && blackPair == HiFlags.Lite) ||
             (whitePair == HiFlags.Dark && blackPair == HiFlags.Dark);
    }

    protected static Boolean bishopPair(HiFlags fhi) {
      return (fhi & HiFlags.Pair) == HiFlags.Pair;
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

    private Boolean raisePiece(BoardSide side, CastleRuleParameter rule, Byte vPiece, Int32 nFrom) {
      hashPiece(side, vPiece, nFrom);
      var qp = BIT0 << nFrom;
#if VerifySquarePiece
      if ((qp & RankPiece) == 0) {
        var sb = new StringBuilder($"Square empty where {side.Parameter.SideName} Piece was expected at")
          .AppendSquares(qp);
        throw new MoveException(sb.ToString());
      }
      else if ((qp & side.Piece) == 0) {
        var sb = new StringBuilder($"{side.Parameter.SideName} Piece was expected at")
          .AppendSquares(qp);
        throw new MoveException(sb.ToString());
      }
#endif
      clrPiece(side, qp);
#if !Magic
      clrRotations(nFrom);
#endif
      var bLite = false;
      switch (vPiece) {
      case vP6:
        Pawn &= ~qp;
        break;
      case vR6:
        RectPiece &= ~qp;
        if (nFrom == rule.RookOOFrom)
          side.FlagsHi &= ~HiFlags.CanOO;
        else if (nFrom == rule.RookOOOFrom)
          side.FlagsHi &= ~HiFlags.CanOOO;
        break;
      case vN6:
        Knight &= ~qp;
        break;
      case vB6:
        DiagPiece &= ~qp;
        bLite = (qp & LiteSquare) != 0;
        break;
      case vQ6:
        DiagPiece &= ~qp;
        RectPiece &= ~qp;
        break;
      case vK6:
        King &= ~qp;
        side.FlagsHi &= ~HiFlags.CanCastleMask;
        break;
      default:
        throw new PieceException("Unexpected Piece [raiseSide]");
      }

      return bLite;
    }

    protected void removePiece(BoardSide side, CastleRuleParameter rule, Byte vPiece, Int32 nFrom) {
      var bLite = raisePiece(side, rule, vPiece, nFrom);
      decSideCount(side, vPiece);

      //
      // Update BishopMask, assuming DiagPiece is up to date
      //
      if (vPiece == vB6) {
        var qpBishop = side.Piece & Bishop;

        if (bLite) {
          if ((qpBishop & LiteSquare) == 0)
            side.FlagsHi &= ~HiFlags.Lite;
        }
        else if ((qpBishop & DarkSquare) == 0)
          side.FlagsHi &= ~HiFlags.Dark;
#if HashPieces
        var u = (UInt32)(side.FlagsHi & HiFlags.Pair) >> nBishopPairBit;
        setTwoBits(ref side.PieceHash, 0, u);   // Piece == vHF
#endif
      }
#if HashPieces
      if (vP6 < vPiece && vPiece < vK6) {
        var u = nibble(side.Counts >> vPiece * nPerNibble);
        setTwoBits(ref side.PieceHash, vPiece - vHF, u % vMod4);
      }
#endif
    }

    protected Boolean lowerPiece(BoardSide side, Byte vPiece, Int32 nTo) {
      hashPiece(side, vPiece, nTo);
      var qp = BIT0 << nTo;
#if VerifySquarePiece
      foreach (var testSide in Side) {
        if ((qp & testSide.Piece) != 0) {
          var sb = new StringBuilder();
          sb.Append($"{testSide.Parameter.SideName} Piece prevents placement of {side.Parameter.SideName} Piece at")
            .AppendSquares(qp);
          throw new MoveException(sb.ToString());
        }
      }
#endif
      setPiece(side, qp);
#if !Magic
      setRotations(nTo);
#endif
      var bLite = false;
      switch (vPiece) {
      case vP6:
        Pawn |= qp;
        break;
      case vR6:
        RectPiece |= qp;
        break;
      case vN6:
        Knight |= qp;
        break;
      case vB6:
        DiagPiece |= qp;
        bLite = (qp & LiteSquare) != 0;
        break;
      case vQ6:
        DiagPiece |= qp;
        RectPiece |= qp;
        break;
      case vK6:
        King |= qp;
        side.KingPos = (Byte)nTo;
        break;
      default:
        throw new PieceException("Unexpected Piece [lowerSide]");
      }

      return bLite;
    }

    protected void placePiece(BoardSide side, Byte vPiece, Int32 nTo) {
      var bLite = lowerPiece(side, vPiece, nTo);
      incSideCount(side, vPiece);

      //
      // Update BishopMask
      //
      if (vPiece == vB6) {
        side.FlagsHi |= bLite ? HiFlags.Lite : HiFlags.Dark;
#if HashPieces
        var u = (UInt32)(side.FlagsHi & HiFlags.Pair) >> nBishopPairBit;
        setTwoBits(ref side.PieceHash, 0, u);   // Piece == vHF
#endif
      }
#if HashPieces
      if (vP6 < vPiece && vPiece < vK6) {
        var u = nibble(side.Counts >> vPiece * nPerNibble);
        setTwoBits(ref side.PieceHash, vPiece - vHF, u % vMod4);
      }
#endif
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

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void clrPiece(BoardSide side, Plane qp) {
      RankPiece &= ~qp;
      side.Piece &= ~qp;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void setPiece(BoardSide side, Plane qp) {
      RankPiece |= qp;
      side.Piece |= qp;
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
      clrRotations(nFrom);
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void setRayState(Int32 nTo) {
      var qp = BIT0 << nTo;
      RankPiece |= qp;
#if !Magic
      setRotations(nTo);
#endif
    }
#endif
#if !Magic
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void clrRotations(Int32 n) {
      FilePiece &= ~FileBit[n];
      A1H8Piece &= ~A1H8Bit[n];
      A8H1Piece &= ~A8H1Bit[n];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void setRotations(Int32 n) {
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
