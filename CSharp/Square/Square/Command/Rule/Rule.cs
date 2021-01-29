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

  #region Enumerations
  enum RuleType : byte {
    code,
    delimiter,
    enableKeyword,
    eol,
    @float,
    goKeyword,
    hyphen,
    line,
    movesKeyword,
    nameKeyword,
    pacnMove,
    opcode,
    option,
    registerKeyword,
    rootKeyword,
    sanMove,
    setup,
    setupType,
    space,
    @string,
    unsigned,
    valueKeyword,
    verb
  }
  #endregion

  class Rule {
    #region Properties
    public RuleType Type { get; }
    private Regex Regex { get; }
    #endregion

    #region Constructors
    public Rule(RuleType ruleType, String regex, RegexOptions options = RegexOptions.None) {
      Type = ruleType;
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
