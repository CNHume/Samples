//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2016-09-04 CNHume]Created Class
//
// Conditionals:
//
namespace Command {
  using System;
  using System.Text.RegularExpressions;

  class TokenRule {
    #region Properties
    public TokenRuleType TokenRuleType { get; }
    private Regex Regex { get; }
    #endregion

    #region Constructors
    public TokenRule(TokenRuleType tokenRuleType, String regex, RegexOptions options = RegexOptions.None) {
      TokenRuleType = tokenRuleType;
      var anchor = $"^({regex})";
      Regex = new Regex(anchor, options);
    }
    #endregion

    #region Methods
    public Match Match(String input) {
      return Regex.Match(input);
    }
    #endregion
  }
}
