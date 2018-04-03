//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
//
namespace InsertionSort {
  using System;

  public class Command {
    #region Virtual Fields
    public Int32? Length;
    public Boolean Print;
    #endregion

    #region Methods
    public void Parse(String[] args) {
      Length = null;
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
        throw new ApplicationException("Usage: InsertionSort [-p] length");
    }

    private static Boolean TryParse(String s, out Int32? value) {
      var valid = Int32.TryParse(s, out Int32 result);
      value = valid ? (Int32?)result : null;
      return valid;
    }
    #endregion
  }
}
