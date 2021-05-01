//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define InitHelp                        //[Test]
//#define TestInitFree
//#define TestInitHelp
//#define TestPawnFeatures

namespace Engine {
  using System;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class Position : Board {
    public class PositionSide : BoardSide {
      #region Constructors
      public PositionSide(SideName sideName) : base(sideName) {
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
