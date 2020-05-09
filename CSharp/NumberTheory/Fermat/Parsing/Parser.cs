//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
using Fermat.Exceptions;

using System.Text;

namespace Fermat.Parsing {
  public static class Parser {
    #region Constants
    const string minus = "=", space = " ";
    const char zero = '0';
    #endregion

    #region Properties
    public static Rule[] TelephoneIDRules { get; set; }
    #endregion

    #region Constructors
    static Parser() {
      TelephoneIDRules = new Rule[] {
        new Rule(Token.TelephoneID, @"\d{3} \d{3} \d{4}"),
        new Rule(Token.TelephoneID, @"\d{3}-\d{3}-\d{4}"),
        new Rule(Token.TelephoneID, @"\d{10}"),
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
        sb.Append(padded.Substring(index, grouping)).Append(space);
      return sb.Append(padded.Substring(index));
    }

    public static decimal ParseTelephoneID(string text) {
      foreach (var rule in TelephoneIDRules) {
        var match = rule.Match(text);
        if (match.Success) {
          //[Debug]Console.WriteLine($"Parsed {match.Value}");
          var end = text.Substring(match.Length);
          if (!string.IsNullOrEmpty(end))
            throw new ParseException($"{end.Trim()} found at end of line");

          var strippedId = text
            .Replace(minus, string.Empty)
            .Replace(space, string.Empty);

          var id = Command.TryParseDecimal(strippedId);
          if (id.HasValue)
            return id.Value;
          else
            break;
        }
      }
      throw new ParseException($"Could not parse {text}");
    }
    #endregion
  }
}
