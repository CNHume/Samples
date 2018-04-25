//
// Copyright (C) 2018, Christopher N. Hume
//
// 2018-03-28 CNHume    Added Command class
//
// Usage Notes:
//
// The -c command line switch passes an integer CategoryId:
// The associated Category will be looked up and reported as specified in Part A of the problem.
//
// The -l command line switch passes an integer Level:
// CategoryId values appearing at the specified level will be reported as specified in Part B of the problem.
//
// The -d command line switch is intended to enable debugging output; but is currently unused.
//
namespace CategoryProblem {
  using System;

  public class Command {
    #region Virtual Fields
    public int? CategoryId;
    public int? Level;
    public bool Debug;
    #endregion

    #region Methods
    /// <summary>
    /// Parse command line arguments
    /// </summary>
    /// <param name="args">command line arguments</param>
    public void Parse(string[] args) {
      Debug = false;

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
          case 'c':                     // category id switch
            if (len > 2)
              usage = !TryParse(token.Substring(2, len - 2), out CategoryId);
            else if (n < count)         // allow whitespace
              usage = !TryParse(args[++n], out CategoryId);
            else
              usage = true;
            break;

          case 'l':                     // subcategory level switch
            if (len > 2)
              usage = !TryParse(token.Substring(2, len - 2), out Level);
            else if (n < count)         // allow whitespace
              usage = !TryParse(args[++n], out Level);
            else
              usage = true;
            break;

          case 'd':                     // debug switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              Debug = true;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      usage |= n < count;               // superfluous argument specified

      if (usage)                        // throw usage line if parse failed
        throw new ArgumentException("Usage: Category [-c id] [-l level] [-d]");
    }

    //
    // TryParse() wrapper with a nullable out parameter
    //
    private static bool TryParse(string s, out int? value) {
      var valid = int.TryParse(s, out int result);
      value = valid ? (int?)result : null;
      return valid;
    }
    #endregion
  }
}
