//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Fermat.Exceptions;

using System;
using System.Text;

namespace Fermat {
  public static class Parser {
    #region Constants
    const string space = " ";
    const char zero = '0';
    #endregion

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
    public static string FormatNavigatorId(decimal id) {
      var sb = new StringBuilder().AppendId(id);
      return sb.ToString();
    }

    public static StringBuilder AppendId(this StringBuilder sb, decimal id, int width = 10, int grouping = 3) {
      var padded = id.ToString().PadLeft(width, zero);
      var index = 0;
      for (var n = 0; n < 2; n++, index += grouping)
        sb.Append(padded.Substring(index, grouping))
          .Append(space);
      return sb.Append(padded.Substring(index));
    }

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
