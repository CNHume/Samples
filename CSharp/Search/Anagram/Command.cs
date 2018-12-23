//
// Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2018-12-23 CNHume  Added Command class
//
namespace Anagram {
  using System;

  public class Command {
    #region Virtual Fields
    public string Letters;
    public bool Subset;
    #endregion

    #region Methods
    public void Parse(string[] args) {
      Letters = null;
      Subset = false;

      var usage = false;
      var count = args.Length;
      var n = 0;
      for (; n < count && !usage; n++) {
        var token = args[n];            // parse any switches
        var len = token.Length;
        if (len < 1 || token[0] != '-')
          break;                        // end of switches
        else if (len < 2)
          usage = true;
        else {                          // parse single character switch
          switch (token[1]) {
          case 's':                     // the subset switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              Subset = true;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      // Require Letters
      if (count <= n)
        usage = true;
      else
        Letters = args[n++];

      usage |= n < count;               // superfluous argument specified

      if (usage)                        // throw usage line if parse failed
        throw new ApplicationException("Usage: Anagram [-s] letters");
    }
    #endregion
  }
}
