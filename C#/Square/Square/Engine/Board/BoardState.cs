﻿//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2022-08-11 CNHume]Renamed file to BoardState
//
// Conditionals:
//
//#define BuildAtxTo
//#define Magic
#define UnshadowRay

using System.Diagnostics;
using System.Runtime.CompilerServices;  // For MethodImplAttribute
using System.Text;

namespace Engine {
  using Exceptions;

  using static Engine.Position;

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;
  using Plane = UInt64;

  partial class Board {
    #region Methods
    //
    // WTM
    // GetSide
    // GetSides
    // setSides
    //
    // GetPieceIndex
    // verifyPieceColors
    //
    // [Clr|Set][RayState|Rotations]
    //
    // OppositeBishops
    // SameBishops
    // HasBishopPair
    //
    #region Side Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public Boolean WTM() {
      return IsEven(GamePly);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected PositionSide GetSide(Boolean bWTM) {
      return bWTM ?
        Side[White] : Side[Black];
    }

    //[Speed]Inlining the following increased performance by 5%
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected (PositionSide friend, PositionSide foe) GetSides(Boolean bWTM) {
      return bWTM ?
        (Side[White], Side[Black]) :
        (Side[Black], Side[White]);
    }

    private void setSides(Boolean bWTM) {
      //[Note]Friend and Foe must remained synchronised to WTM()
      (Friend, Foe) = GetSides(bWTM);
    }
    #endregion                          // Side Methods

    #region Square Pieces
    protected Byte GetPieceIndex(Int32 n) {
      var vPiece = vPieceNull;          // Return Value
      var qp = bit(n);

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
      else if ((qp & OrthPiece) != 0) {
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
    private void verifyPieceColors() {
      var (blackSide, whiteSide) = Side.GetBothSides();

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
      FilePiece &= ~BitFile[n];
      A1H8Piece &= ~BitA1H8[n];
      A8H1Piece &= ~BitA8H1[n];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal void SetRotations(Int32 n) {
      FilePiece |= BitFile[n];
      A1H8Piece |= BitA1H8[n];
      A8H1Piece |= BitA8H1[n];
    }
#endif
#if UnshadowRay
    //
    // The following are called to remove and replace a King from the
    // board to unshadow its destination squares from any ray attacks.
    //
    //[Note]The Hash is not updated during this interval.
    //
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void ClrRayState(Int32 nFrom) {
      var qp = bit(nFrom);
      RankPiece &= ~qp;
#if !Magic
      ClrRotations(nFrom);
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void SetRayState(Int32 nTo) {
      var qp = bit(nTo);
      RankPiece |= qp;
#if !Magic
      SetRotations(nTo);
#endif
    }
#endif                                  // UnshadowRay
    #endregion                          // Rotations

    #region Bishop Tests
    protected static Boolean OppositeBishops(SideFlags fBlackSide, SideFlags fWhiteSide) {
      var blackPair = fBlackSide & SideFlags.Pair;
      var whitePair = fWhiteSide & SideFlags.Pair;

      return (whitePair == SideFlags.Lite && blackPair == SideFlags.Dark) ||
             (whitePair == SideFlags.Dark && blackPair == SideFlags.Lite);
    }

    protected static Boolean SameBishops(SideFlags fBlackSide, SideFlags fWhiteSide) {
      var blackPair = fBlackSide & SideFlags.Pair;
      var whitePair = fWhiteSide & SideFlags.Pair;

      return (whitePair == SideFlags.Lite && blackPair == SideFlags.Lite) ||
             (whitePair == SideFlags.Dark && blackPair == SideFlags.Dark);
    }

    protected static Boolean HasBishopPair(SideFlags fside) {
      return (fside & SideFlags.Pair) == SideFlags.Pair;
    }
    #endregion                          // Bishop Tests

    #region Flag Methods
    #region TurnFlags
    public Boolean InCheck() {
      return FlagsTurn.Has(TurnFlags.InCheck);
    }

    protected void SetInCheck(Boolean bInCheck) {
      if (bInCheck)
        FlagsTurn |= TurnFlags.InCheck;
      else
        FlagsTurn &= ~TurnFlags.InCheck;
    }

    protected void SetLegal(Boolean bLegal) {
      if (bLegal)
        FlagsTurn &= ~TurnFlags.Illegal;
      else
        FlagsTurn |= TurnFlags.Illegal;
    }

    public Boolean IsFinal() {
      return FlagsTurn.Has(TurnFlags.Final);
    }

    protected void SetFinal() {
      FlagsTurn |= TurnFlags.Final;
    }

    public Boolean IsStalemate() {
      return IsFinal() && !InCheck();
    }
    #endregion                          // TurnFlags

    #region DrawFlags
    public Boolean IsEPLegal() {
      return FlagsTurn.Has(TurnFlags.EPLegal);
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
    protected void SetDrawIM() {
      FlagsDraw &= ~DrawFlags.DrawIM;   //[Safe]
      if (IsInsufficient(RankPiece))
        FlagsDraw |= DrawFlags.DrawIM;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public Boolean IsInsufficient(Plane qpPiece) {
      var qpPawn = qpPiece & Pawn;
      var qpOrth = qpPiece & OrthPiece;
      var qpDiag = qpPiece & DiagPiece;
      var qpKnight = qpPiece & Knight;

      // No Pawn, Rook or Queen:
      if (qpPawn == 0 && qpOrth == 0) {
        //
        // If either a single Knight or multiple Bishops covering squares
        // of only one color remain, then even a helpmate is not possible.
        //
        if (qpDiag == 0) {              // Test for KK[N]:
          if (IsOneOrNone(qpKnight))
            return true;
        }
        else if (qpKnight == 0) {       // Test for KB*KB+ of same color:
          if ((qpDiag & SquareLite) == 0 ||
              (qpDiag & SquareDark) == 0)
            return true;
        }
      }

      return false;
    }

    protected void ClrRepetition() {
      FlagsDraw &= ~(DrawFlags.Draw3 | DrawFlags.Draw2);
    }

    protected void SetRepetition(Boolean bDraw3) {
      FlagsDraw |= bDraw3 ? DrawFlags.Draw3 : DrawFlags.Draw2;
    }

    protected DrawFlags fdraw() {
      return FlagsDraw & (DrawFlags.Draw3 | DrawFlags.Draw2);
    }

    public Boolean IsDraw0() {
      return FlagsDraw.Has(DrawFlags.Draw0);
    }

    private void clrDraw0() {
      FlagsDraw &= ~DrawFlags.Draw0;
    }

    protected void SetDraw0() {
      FlagsDraw |= DrawFlags.Draw0;
    }

    protected void SetDraw50() {
      if (HalfMoveClock < vHalfMoveClockMax)
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

    private void clrTrace() {
      FlagsMode &= ~ModeFlags.Trace;
    }

    private void setTrace(Hashcode qHashcode) {
      if (qHashcode == Hash)
        FlagsMode |= ModeFlags.Trace;
    }

    //[Speed]Use of params slowed performance by 13%.
    private void setTrace(params Hashcode[] qHashcodes) {
      if (qHashcodes.Any(qHashcode => qHashcode == Hash))
        FlagsMode |= ModeFlags.Trace;
    }
    #endregion                          // ModeFlags
    #endregion                          // Flag Methods

    #region Parity
    public static Boolean IsEven(UInt32 u) {
      return (u & 1) == 0;
    }

    public static Boolean IsOdd(UInt32 u) {
      return (u & 1) != 0;
    }
    #endregion                          // Parity
    
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public Int32? sqrEP() {
      return EPSquare;
    }
    #endregion                          // Methods
  }
}
