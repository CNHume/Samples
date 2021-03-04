//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2014-12-13 CNHume  Added parse() method
// 2009-01-07 CNHume  Sort Test Platform
//
// Conditionals:
//
//#define LinearEntries
//#define Reverse

namespace Sort {
  using System;

  class Program {
    static void Main(String[] args) {
      try {
        var cmd = new Command();
        cmd.Parse(args);

        if (cmd.Merges.HasValue)
          MergeList<Int32>.Merges = cmd.Merges.Value;
        if (cmd.InsertionLimit.HasValue)
          MergeList<Int32>.InsertionLimit = cmd.InsertionLimit.Value;
#if LinearEntries
        var entries = linearEntries(cmd.Length.Value);
#else
        var entries = randomEntries(cmd.Length.Value);
#endif
#if Reverse
        MergeList<Int32>.Reverse(entries);
#endif
        SortTest<Int32>.TestSort(entries, cmd.Print);
      }
      catch (ApplicationException ex) {
        Console.WriteLine(ex.Message);
      }
      catch (Exception ex) {
        Console.WriteLine(ex);
      }
#if DEBUG
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }

    private static Int32[] linearEntries(Int32 length) {
      var dt = DateTime.Now;
      Console.WriteLine("{0:HH:mm:ss.fff} Building {1:n0} linear entries", dt, length);

      var entries = new Int32[length];
      for (var index = 0; index < length; index++)
        entries[index] = index;

      return entries;
    }

    private static Int32[] randomEntries(Int32 length) {
      const Int32 scale = 3;
      var range = scale * length;

      var dt = DateTime.Now;
      Console.WriteLine("{0:HH:mm:ss.fff} Building {1:n0} random entries", dt, length);

      var seed = (Int32)dt.Ticks;
      var r = new Random(seed);

      var entries = new Int32[length];
      for (var index = 0; index < length; index++)
        entries[index] = r.Next(range) + 1;

      return entries;
    }

    static void parse(String[] args, out Int32? length, out Int32? merges, out Int32? insertionLimit, out Boolean print) {
      length = null;
      insertionLimit = null;
      merges = null;
      print = false;

      var usage = false;
      var count = args.Length;
      var n = 0;
      for (; n < count && !usage; n++) {
        String token = args[n];         // parse any switches
        var len = token.Length;
        if (len < 1 || token[0] != '-')
          break;                        // end of switches
        else if (len < 2)
          usage = true;
        else {                          // parse single character switch
          switch (token[1]) {
          case 'i':                     // the insertion-limit switch
            if (len > 2)                // whitespace optional
              tryParse(token.Substring(2, len - 2), ref insertionLimit, ref usage);
            else if (n < count)         // whitespace allowed
              tryParse(args[++n], ref insertionLimit, ref usage);
            else
              usage = true;
            break;

          case 'm':                     // the merges switch
            if (len > 2)                // whitespace optional
              tryParse(token.Substring(2, len - 2), ref merges, ref usage);
            else if (n < count)         // whitespace allowed
              tryParse(args[++n], ref merges, ref usage);
            else
              usage = true;
            break;

          case 'p':                     // the print switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              print = true;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      if (n < count)                    // parse the length
        length = Int32.Parse(args[n++]);
      else                              // length is required
        usage = true;

      if (n < count)                    // superfluous argument specified
        usage = true;

      if (usage)                        // throw usage line if parse failed
        throw new ApplicationException("Usage: MergeList [-i <insertion-limit>] [-m <merges>] [-p] length");
    }

    private static void tryParse(String s, ref Int32? value, ref Boolean usage) {
      Int32 result;
      if (Int32.TryParse(s, out result))
        value = result;
      else
        usage = true;
    }
  }
}
