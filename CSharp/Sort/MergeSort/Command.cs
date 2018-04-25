//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
//
namespace Sort {
  using System;

  public class Command {
    #region Virtual Fields
    public Int32? Length;
    public Int32? Merges;
    public Int32? InsertionLimit;
    public Boolean Print;
    #endregion

    #region Methods
    public void Parse(String[] args) {
      Length = null;
      InsertionLimit = null;
      Merges = null;
      Print = false;

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
          case 'i':                     // the insertion-limit switch
            if (len > 2)                // whitespace optional
              usage = !TryParse(token.Substring(2, len - 2), out InsertionLimit);
            else if (n < count)         // whitespace allowed
              usage = !TryParse(args[++n], out InsertionLimit);
            else
              usage = true;
            break;

          case 'm':                     // the merges switch
            if (len > 2)                // whitespace optional
              usage = !TryParse(token.Substring(2, len - 2), out Merges);
            else if (n < count)         // whitespace allowed
              usage = !TryParse(args[++n], out Merges);
            else
              usage = true;
            break;

          case 'p':                     // the print switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              Print = true;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      // length is required
      usage |= count <= n || !TryParse(args[n++], out Length);

      usage |= n < count;               // superfluous argument specified

      if (usage)                        // throw usage line if parse failed
        throw new ApplicationException("Usage: MergeSort [-i <insertion-limit>] [-m <merges>] [-p] length");
    }

    private static Boolean TryParse(String s, out Int32? value) {
      var valid = Int32.TryParse(s, out Int32 result);
      value = valid ? (Int32?)result : null;
      return valid;
    }
    #endregion
  }
}
