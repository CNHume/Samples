//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2022-08-11 CNHume]Renamed file to BoardState
//
// Conditionals:
//
//#define BuildAtxTo
//#define Magic

using System.Diagnostics;
using System.Runtime.CompilerServices;  // For MethodImplAttribute
using System.Text;

namespace Engine;

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
  // GetPieceIndex - Returns Piece at any square
  // verifyPieceColors
  //
  // [Clr|Set][RayState|Rotations]
  //
  // OppositeBishops
  // SameBishops
  // HasBishopPair
  //
  // clrEPLegal
  // setEPLegal
  // IsEPLegal
  //
  // applyEPHash
  // resetTurnFlags
  // setEPTarget
  //
  // InCheck
  // SetInCheck
  // SetLegal
  //
  // SetFinal
  // IsFinal
  // IsStalemate
  //
  // IsDraw
  // IsDraw2
  // IsInsufficient
  // SetDrawIM
  //
  // [Clr|Set]Repetition
  // fdraw
  // [clr|set]Draw0
  // IsDraw0
  //
  // updateDraw50
  // IsDraw50
  //
  // [clr|set]NullMade
  // IsNullMade
  // [clr|set]Trace
  // IsTrace
  //
  // IsEven
  // IsOdd
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
  #endregion                            // Side Methods

  #region Square Pieces
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected Byte GetPieceIndex(Int32 n) {
    var vPiece = vPieceNull;            // Return Value
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
  #endregion                            // Square Pieces

  #region Rotations
#if !Magic
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void clrRotations(Int32 n) {
    FilePiece &= ~BitFile[n];
    A1H8Piece &= ~BitA1H8[n];
    A8H1Piece &= ~BitA8H1[n];
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void setRotations(Int32 n) {
    FilePiece |= BitFile[n];
    A1H8Piece |= BitA1H8[n];
    A8H1Piece |= BitA8H1[n];
  }
#endif
  //
  // The following are called to remove and replace a King from the
  // board to unshadow its destination squares from ray attacks.
  //
  //[Note]Hash is not updated between these calls.
  //
  [Conditional("RemoveKingShadow")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void ClrRayState(Int32 nFrom) {
    var qp = bit(nFrom);
    RankPiece &= ~qp;
#if !Magic
    clrRotations(nFrom);
#endif
  }

  [Conditional("RemoveKingShadow")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetRayState(Int32 nTo) {
    var qp = bit(nTo);
    RankPiece |= qp;
#if !Magic
    setRotations(nTo);
#endif
  }
  #endregion                            // Rotations

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
  #endregion                            // Bishop Tests

  #region Flag Methods
  #region TurnFlags
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void clrEPLegal() {
    FlagsTurn &= ~TurnFlags.EPLegal;
    EPTarget = default;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void setEPLegal() {
    FlagsTurn |= TurnFlags.EPLegal;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsEPLegal() {
    return FlagsTurn.Has(TurnFlags.EPLegal);
  }

  //
  // The following is called by resetTurnFlags(),
  // and when tryEP() sets TurnFlags.EPLegal.
  //
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void applyEPHash(ref Hashcode qHash) {
    if (IsEPLegal()) qHash ^= epHash();
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void resetTurnFlags() {
    clrDraw0();
    // applyEPHash() calls epHash() which requires EPTarget.
    applyEPHash(ref Hash);
    // clrEPLegal() clears EPTarget.
    clrEPLegal();
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void setEPTarget(int nFrom, int nTo, int pawnStep) {
    if (nTo == nFrom + 2 * pawnStep)
      EPTarget = (Byte)(nFrom + pawnStep);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean InCheck() {
    return FlagsTurn.Has(TurnFlags.InCheck);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetInCheck(Boolean bInCheck) {
    if (bInCheck)
      FlagsTurn |= TurnFlags.InCheck;
    else
      FlagsTurn &= ~TurnFlags.InCheck;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetLegal(Boolean bLegal) {
    if (bLegal)
      FlagsTurn &= ~TurnFlags.Illegal;
    else
      FlagsTurn |= TurnFlags.Illegal;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetFinal() {
    FlagsTurn |= TurnFlags.Final;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsFinal() {
    return FlagsTurn.Has(TurnFlags.Final);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsStalemate() {
    return IsFinal() && !InCheck();
  }
  #endregion                            // TurnFlags

  #region DrawFlags
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsDraw() {
    return FlagsDraw.Has(DrawFlags.DrawMask);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsDraw2() {
    return FlagsDraw.Has(DrawFlags.Draw2);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected Boolean IsDraw50() {
    return FlagsDraw.Has(DrawFlags.Draw50);
  }

  //
  // Recognize Draw by Insufficient Material:
  //
  protected void SetDrawIM() {
    FlagsDraw &= ~DrawFlags.DrawIM;     //[Safe]
    if (IsInsufficient(RankPiece))
      FlagsDraw |= DrawFlags.DrawIM;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsInsufficient() {
    return FlagsDraw.Has(DrawFlags.DrawIM);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsInsufficient(Plane qpPiece, Boolean bHelpmate = false) {
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
      if (qpDiag == 0) {
        if (bHelpmate) {
          if (IsTwoOrLess(qpKnight))    // Test for KK[N[N]]:
            return true;
        }
        else if (IsOneOrLess(qpKnight)) // Test for KK[N]:
          return true;
      }
      else if (qpKnight == 0) {         // Test for KB*KB+ of same color:
        if ((qpDiag & SquareLite) == 0 ||
            (qpDiag & SquareDark) == 0)
          return true;
      }
    }

    return false;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void ClrRepetition() {
    FlagsDraw &= ~(DrawFlags.Draw3 | DrawFlags.Draw2);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetRepetition(Boolean bDraw3) {
    FlagsDraw |= bDraw3 ? DrawFlags.Draw3 : DrawFlags.Draw2;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected DrawFlags fdraw() {
    return FlagsDraw & (DrawFlags.Draw3 | DrawFlags.Draw2);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void clrDraw0() {
    FlagsDraw &= ~DrawFlags.Draw0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetDraw0() {
    FlagsDraw |= DrawFlags.Draw0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public Boolean IsDraw0() {
    return FlagsDraw.Has(DrawFlags.Draw0);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void updateDraw50() {
    if (HalfMoveClock < vHalfMoveClockMax)
      FlagsDraw &= ~DrawFlags.Draw50;
    else                                // 50 Move Rule
      FlagsDraw |= DrawFlags.Draw50;
  }
  #endregion                            // DrawFlags

  #region ModeFlags
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void clrNullMade() {
    FlagsMode &= ~ModeFlags.NullMade;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected void SetNullMade() {
    FlagsMode |= ModeFlags.NullMade;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected Boolean IsNullMade() {
    return FlagsMode.Has(ModeFlags.NullMade);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void clrTrace() {
    FlagsMode &= ~ModeFlags.Trace;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void setTrace(Hashcode qHashcode) {
    if (qHashcode == Hash)
      FlagsMode |= ModeFlags.Trace;
  }

  //[Speed]Use of params slowed performance by 13%.
  private void setTrace(params Hashcode[] qHashcodes) {
    if (qHashcodes.Any(qHashcode => qHashcode == Hash))
      FlagsMode |= ModeFlags.Trace;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  protected Boolean IsTrace() {
    return FlagsMode.Has(ModeFlags.Trace);
  }
  #endregion                            // ModeFlags
  #endregion                            // Flag Methods

  #region Parity
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean IsEven(UInt32 u) {
    return (u & 1) == 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean IsOdd(UInt32 u) {
    return (u & 1) != 0;
  }
  #endregion                            // Parity
  #endregion                            // Methods
}
