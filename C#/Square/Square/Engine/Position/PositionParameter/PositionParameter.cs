//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2021-05-01 CNHume]Created Class
//
// Conditionals:
//
#define InitFree                        //[Default]
//#define InitHelp                        //[Test]
//#define TestInitFree
//#define TestInitHelp

namespace Engine {
  using static Engine.Board.BoardSide;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    internal class PositionParameter : BoardParameter {
      #region Constructors
      public PositionParameter(SideName sideName) : base(sideName) {
        KingToMoveLoss = new Plane[nSquares];
        PawnToMoveWins = new Plane[nSquares];
#if TestInitHelp || InitFree || !InitHelp
        Free = new Plane[nSquares];
#endif
#if TestInitFree || InitHelp || !InitFree
        Help = new Plane[nSquares];
#endif
      }
      #endregion

      #region Pawn Feature Fields
      public readonly Plane[] KingToMoveLoss;
      public readonly Plane[] PawnToMoveWins;
#if TestInitHelp || InitFree || !InitHelp
      public readonly Plane[] Free;
#endif
#if TestInitFree || InitHelp || !InitFree
      public readonly Plane[] Help;
#endif
      #endregion
    }
  }
}
