//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Fermat.Exceptions;

using System;

namespace Fermat {
  public static class Parser {
    #region Properties
    public static Rule[] NavigatorIDRules { get; set; }
    #endregion

    #region Constructors
    static Parser() {
      NavigatorIDRules = new Rule[] {
        new Rule(Token.NavigatorID, @"\d{3} \d{3} \d{4}"),
        new Rule(Token.NavigatorID, @"\d{3}-\d{3}-\d{4}"),
        new Rule(Token.NavigatorID, @"\d{10}"),
      };
    }
    #endregion

    #region Methods
    public static void ParseNavigatorID(string text) {
      foreach (var rule in NavigatorIDRules) {
        var match = rule.Match(text);
        if (match.Success) {
#if DEBUG
          Console.WriteLine($"Parsed {match.Value}");
#endif
          var end = text.Substring(match.Length);
          if (!string.IsNullOrEmpty(end))
            throw new ParseException($"{end.Trim()} found at end of line");
          return;
        }
      }
      throw new ParseException($"Could not parse {text}");
    }
    #endregion
  }
}
