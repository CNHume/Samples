//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using System.Text.RegularExpressions;

namespace Fermat {
  #region Enumerations
  public enum Token : byte {
    NavigatorID,
  }
  #endregion

  public class Rule {
    #region Properties
    public Token Type { get; private set; }
    private Regex Regex { get; set; }
    #endregion

    #region Constructors
    public Rule(Token type, string regex, RegexOptions options = RegexOptions.None) {
      Type = type;
      Regex = new Regex($"^({regex})", options);
    }
    #endregion

    #region Methods
    public Match Match(string input) {
      return Regex.Match(input);
    }
    #endregion
  }
}
