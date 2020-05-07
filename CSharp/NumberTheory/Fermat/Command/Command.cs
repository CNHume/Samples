//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Fermat.Exceptions;

using System;

namespace Fermat {
  public class Command {
    #region Properties
    public decimal? Input { get; set; }
    public decimal? Phi { get; set; }
    public decimal? Modulus { get; set; }
    public bool IsVerbose { get; set; }
    #endregion

    #region Constructors
    public Command(string[] args) {
      Parse(args);
    }
    #endregion

    #region Methods
    public void Parse(string[] args) {
      Input = null;
      IsVerbose = false;

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
          case 'v':                     // the verbose switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              IsVerbose = true;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      // input is required
      if (n < count) {
        Input = TryParseDecimal(args[n++]);

        // phi is required
        if (n < count) {
          Phi = TryParseDecimal(args[n++]);

          // modulus is required
          if (n < count)
            Modulus = TryParseDecimal(args[n++]);
        }
      }

      usage |= !(Input.HasValue && Phi.HasValue && Modulus.HasValue);

      usage |= n < count;               // superfluous argument specified

      if (usage)                        // throw usage line if parse failed
        throw new CommandException("Usage: Fermat [-v] input phi modulus");
    }

    #endregion

    #region Parsers
    public static DateTime? TryParseDateTime(String s) {
      return DateTime.TryParse(s, out DateTime result) ? (DateTime?)result : null;
    }

    public static decimal? TryParseDecimal(String s) {
      return decimal.TryParse(s, out decimal result) ? (decimal?)result : null;
    }

    public static Int32? TryParseInt32(String s) {
      return Int32.TryParse(s, out Int32 result) ? (Int32?)result : null;
    }
    #endregion
  }
}
