//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-06-22 CNHume]Created Class
//
namespace Engine {
  using static Board;
  using static Board.BoardSide;
  using Exceptions;
  using System;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class CastleRule {
    #region Constructors
    public CastleRule() {
      RuleSide = new CastleRuleSide[nSides];
      foreach (var sideName in (SideName[])Enum.GetValues(typeof(SideName)))
        RuleSide[(Int32)sideName] = new CastleRuleSide(sideName);

      Clear();
    }
    #endregion

    #region Methods
    public void Clear() {
      foreach (var ruleSide in RuleSide)
        ruleSide.Clear();

      IsChess960 = false;
    }

    public void ValidateCastlingSymmetry(Boolean bFromSameFile) {
      if (RuleSide[White].CastlesFrom.HasValue && RuleSide[Black].CastlesFrom.HasValue) {
        if (!bFromSameFile)
          throw new ParsePositionException("Both Kings must castle from the same file");

        if (RuleSide[White].RookOOFrom.HasValue && RuleSide[Black].RookOOFrom.HasValue &&
            RuleSide[White].RookOOFrom + nRankLast != RuleSide[Black].RookOOFrom)
          throw new ParsePositionException("Both sides must OO with Rooks from the same file");

        if (RuleSide[White].RookOOOFrom.HasValue && RuleSide[Black].RookOOOFrom.HasValue &&
            RuleSide[White].RookOOOFrom + nRankLast != RuleSide[Black].RookOOOFrom)
          throw new ParsePositionException("Both sides must OOO with Rooks from the same file");
      }
    }
    #endregion

    #region Fields
    public CastleRuleSide[] RuleSide;

    //
    //[Chess 960]The following are derived in initCastleRules():
    //
    public Boolean IsChess960;
    #endregion
  }
}
