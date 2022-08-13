//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2022-08-11 CNHume]Renamed file to BoardState
//
// Conditionals:
//
//#define BuildAtxTo
//#define Magic
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
  using Hashcode = UInt64;
  using Plane = UInt64;

  partial class Board {
    #region Methods
    //
    // getPieceIndex
    // verifyPieceColors
    //
    // [Clr|Set][RayState|Rotations]
    //
    // oppositeBishops
    // sameBishops
    // hasBishopPair
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

    #region Flag Methods
    #region TurnFlags
    public Boolean WTM() {
      return FlagsTurn.Has(TurnFlags.WTM);
    }

    protected void setWTM(Boolean bWTM) {
      if (bWTM)
        FlagsTurn |= TurnFlags.WTM;
      else
        FlagsTurn &= ~TurnFlags.WTM;

      //[Note]Friend and Foe must always correspond to TurnFlags.WTM
      (Friend, Foe) = getSides(WTM());
    }

    public Boolean InCheck() {
      return FlagsTurn.Has(TurnFlags.InCheck);
    }

    protected void setInCheck(Boolean bInCheck) {
      if (bInCheck)
        FlagsTurn |= TurnFlags.InCheck;
      else
        FlagsTurn &= ~TurnFlags.InCheck;
    }

    protected void setLegal(Boolean bLegal) {
      if (bLegal)
        FlagsTurn &= ~TurnFlags.Illegal;
      else
        FlagsTurn |= TurnFlags.Illegal;
    }

    public Boolean IsFinal() {
      return FlagsTurn.Has(TurnFlags.Final);
    }

    protected void setFinal() {
      FlagsTurn |= TurnFlags.Final;
    }

    public Boolean IsStalemate() {
      return IsFinal() && !InCheck();
    }
    #endregion                          // TurnFlags

    #region DrawFlags
    public Boolean IsPassed() {
      return FlagsTurn.Has(TurnFlags.Passed);
    }

    public Boolean IsDraw() {
      return FlagsDraw.Has(DrawFlags.DrawMask);
    }

    public Boolean IsDraw2() {
      return FlagsDraw.Has(DrawFlags.Draw2);
    }

    public Boolean IsDraw50() {
      return FlagsDraw.Has(DrawFlags.Draw50);
    }

    public Boolean IsInsufficient() {
      return FlagsDraw.Has(DrawFlags.DrawIM);
    }

    //
    // Recognize Draw by Insufficient Material:
    //
    protected void setInsufficient() {
      FlagsDraw &= ~DrawFlags.DrawIM;   //[Safe]
      if (IsInsufficient(RankPiece))
        FlagsDraw |= DrawFlags.DrawIM;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public Boolean IsInsufficient(Plane qpPiece) {
      var qpPawn = qpPiece & Pawn;
      var qpRect = qpPiece & RectPiece;
      var qpDiag = qpPiece & DiagPiece;
      var qpKnight = qpPiece & Knight;

      // No Pawn, Rook or Queen:
      if (qpPawn == 0 && qpRect == 0) {
        //
        // If either a single Knight or multiple Bishops covering squares
        // of only one color remain, then even a helpmate is not possible.
        //
        if (qpDiag == 0) {              // Test for KK[N]:
          if (IsOneOrNone(qpKnight))
            return true;
        }
        else if (qpKnight == 0) {       // Test for KB*KB+ of same color:
          if ((qpDiag & LiteSquare) == 0 ||
              (qpDiag & DarkSquare) == 0)
            return true;
        }
      }

      return false;
    }

    protected void clrRepetition() {
      FlagsDraw &= ~(DrawFlags.Draw3 | DrawFlags.Draw2);
    }

    protected void setRepetition(Boolean bDraw3) {
      FlagsDraw |= bDraw3 ? DrawFlags.Draw3 : DrawFlags.Draw2;
    }

    protected DrawFlags fdraw() {
      return FlagsDraw & (DrawFlags.Draw3 | DrawFlags.Draw2);
    }

    public Boolean IsDraw0() {
      return FlagsDraw.Has(DrawFlags.Draw0);
    }

    protected void clrDraw0() {
      FlagsDraw &= ~DrawFlags.Draw0;
    }

    protected void setDraw0() {
      FlagsDraw |= DrawFlags.Draw0;
    }

    protected void setDraw50() {
      if (HalfMoveClock < HalfMoveClockMax)
        FlagsDraw &= ~DrawFlags.Draw50;
      else                              // 50 Move Rule
        FlagsDraw |= DrawFlags.Draw50;
    }
    #endregion                          // DrawFlags

    #region ModeFlags
    protected Boolean IsNullMade() {
      return FlagsMode.Has(ModeFlags.NullMade);
    }

    private void clrNullMade() {
      FlagsMode &= ~ModeFlags.NullMade;
    }

    private void setNullMade() {
      FlagsMode |= ModeFlags.NullMade;
    }

    protected bool IsTrace() {
      return FlagsMode.Has(ModeFlags.Trace);
    }

    protected void clrTrace() {
      FlagsMode &= ~ModeFlags.Trace;
    }

    protected void setTrace(Hashcode qHashcode) {
      if (qHashcode == Hash)
        FlagsMode |= ModeFlags.Trace;
    }

    //[Speed]Use of params is slow.
    protected void setTrace(params Hashcode[] qHashcodes) {
      if (qHashcodes.Any(qHashcode => qHashcode == Hash))
        FlagsMode |= ModeFlags.Trace;
    }
    #endregion                          // ModeFlags
    #endregion                          // Flag Methods
    #endregion                          // Methods
  }
}
