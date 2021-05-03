//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-06-22 CNHume]Created Class
//
namespace Engine {
  using Exceptions;

  using System;

  using static Board;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class CastleRule {
    #region Constructors
    public CastleRule() {
      RuleParameter = new CastleRuleParameter[nSides];
      newCastleRuleSides();

      Clear();
    }

    private void newCastleRuleSides() {
      foreach (var parameter in Parameter) {
        var nParameter = (Int32)parameter.SideName;
        RuleParameter[nParameter] = new CastleRuleParameter(parameter);
      }
    }
    #endregion

    #region Methods
    public void Init() {
      foreach (var ruleParameter in RuleParameter) {
        if (ruleParameter is null)
          throw new BoardException("Null CastleRuleSide Instance");

        ruleParameter.Init();
      }
    }

    public void Clear() {
      foreach (var ruleParameter in RuleParameter)
        ruleParameter.Clear();

      IsChess960 = false;
    }

    public void ValidateCastlingSymmetry(Boolean bFromSameFile) {
      if (RuleParameter[White].CastlesFrom.HasValue && RuleParameter[Black].CastlesFrom.HasValue) {
        if (!bFromSameFile)
          throw new ParsePositionException("Both Kings must castle from the same file");

        if (RuleParameter[White].RookOOFrom.HasValue && RuleParameter[Black].RookOOFrom.HasValue &&
            RuleParameter[White].RookOOFrom + nRankLast != RuleParameter[Black].RookOOFrom)
          throw new ParsePositionException("Both sides must OO with Rooks from the same file");

        if (RuleParameter[White].RookOOOFrom.HasValue && RuleParameter[Black].RookOOOFrom.HasValue &&
            RuleParameter[White].RookOOOFrom + nRankLast != RuleParameter[Black].RookOOOFrom)
          throw new ParsePositionException("Both sides must OOO with Rooks from the same file");
      }
    }
    #endregion

    #region Fields
    public readonly CastleRuleParameter[] RuleParameter;

    //
    //[Chess 960]The following are derived in InitCastleRules():
    //
    public Boolean IsChess960;
    #endregion
  }
}
