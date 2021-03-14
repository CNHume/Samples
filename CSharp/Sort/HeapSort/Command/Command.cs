//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
//
namespace Sort {
  using Exceptions;

  using System;

  public class Command {
    #region Properties
    public Int32? Length { get; set; }
    public Boolean Print { get; set; }
    #endregion

    #region Methods
    public void Parse(String[] args) {
      Length = default;
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
      if (n < count)
        Length = TryParseInt32(args[n++]);

      usage |= !Length.HasValue;

      usage |= n < count;               // superfluous argument specified

      if (usage)                        // throw usage line if parse failed
        throw new CommandException("Usage: HeapSort [-p] length");
    }

    #endregion

    #region Parsers
    private static DateTime? TryParseDateTime(String s) {
      return DateTime.TryParse(s, out DateTime result) ? (DateTime?)result : null;
    }

    private static decimal? TryParseDecimal(String s) {
      return decimal.TryParse(s, out decimal result) ? (decimal?)result : null;
    }

    private static Int32? TryParseInt32(String s) {
      return Int32.TryParse(s, out Int32 result) ? (Int32?)result : null;
    }
    #endregion
  }
}
