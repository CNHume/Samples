//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
using System.Text.RegularExpressions;

namespace Fermat.Parsing {
  #region Enumerations
  public enum Token : byte {
    TelephoneID,
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
