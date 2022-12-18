//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2022-12-08 CNHume]Created Class
//
// Conditionals:
//
//#define TestPawnAdvances

namespace Engine {
#if TestPawnAdvances
  using static Logging.Logger;
#endif
  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    internal partial class PositionSide : BoardSide {
      #region Constructors
      public PositionSide(Position position, PositionParameter positionParameter) :
        base(position, positionParameter) {
      }
      #endregion                        // Constructors

      #region Methods
      #region Pawn Move Generators
      public void AddPawnCaptures(Plane qpTo) {
        var nEP = Board.IsPassed() ? Board.FlagsTurn.sqrEP() : nSquareUndefined;
        AddPawnCaptures2(PawnA1H8Atx & qpTo, Parameter.PawnA1H8, nEP);
        AddPawnCaptures2(PawnA8H1Atx & qpTo, Parameter.PawnA8H1, nEP);
      }

      protected void AddPawnCaptures2(
        Plane qpAtx, Int32 nDiag, Int32 nEP) {
        var qpFrom = ShiftR(qpAtx, nDiag);
        while (qpFrom != 0) {
          var nFrom = RemoveLo(ref qpFrom);
          var nTo = nFrom + nDiag;
          var bAbove = Parameter.IsAbove(nTo);
          var bPromote = Parameter.IsLastRank(nTo);
          var bEnPassant = nTo == nEP;
          Position.AddPawnCapture(nFrom, nTo, bAbove, bPromote, bEnPassant);
        }
      }

      public void AddPawnMoves(Position position, Plane qpTo) {
        var qpPawn = Piece & Board.Pawn;

        //
        // Pawn Advances:
        //
        var qpAdvance1 = ShiftL(qpPawn, Parameter.PawnStep) & ~Board.RankPiece;
        var qpAdvance2 = ShiftL(qpAdvance1 & Parameter.RankPass, Parameter.PawnStep) & ~Board.RankPiece;
        var qpAdv1From = ShiftR(qpAdvance1 & qpTo, Parameter.PawnStep);
        var qpAdv2From = ShiftR(qpAdvance2 & qpTo, 2 * Parameter.PawnStep);
#if TestPawnAdvances
        LogLine("Pawn Advance:\n");
        WriteOrth(qpAdvance1 | qpAdvance2);
        LogLine();
#endif
        while (qpAdv1From != 0) {
          var nFrom = RemoveLo(ref qpAdv1From);
          var nTo = nFrom + Parameter.PawnStep;
          var bAbove = Parameter.IsAbove(nTo);
          var bPromote = Parameter.IsLastRank(nTo);
          Position.AddPawnMove(nFrom, nTo, bAbove, bPromote);
        }

        while (qpAdv2From != 0) {
          var nFrom = RemoveLo(ref qpAdv2From);
          var nTo = nFrom + 2 * Parameter.PawnStep;
          Position.AddPawnMove(nFrom, nTo, false, false);
        }
      }

      // The following method is used by generateMaterialMoves()
      public void AddPromotionMoves(Plane qpTo) {
        var qpPawn = Piece & Board.Pawn;
        var qpAdvance1 = ShiftL(qpPawn, Parameter.PawnStep) & ~Board.RankPiece;
        var qpAdv1From = ShiftR(qpAdvance1 & qpTo & Parameter.RankLast, Parameter.PawnStep);

        while (qpAdv1From != 0) {
          var nFrom = RemoveLo(ref qpAdv1From);
          var nTo = nFrom + Parameter.PawnStep;
          Position.AddPawnMove(nFrom, nTo, true, true);
        }
      }
      #endregion                        // Pawn Move Generators
      #endregion                        // Methods
    }                                   // PositionSide
  }                                     // Position
}
