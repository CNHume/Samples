//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2016-06-14 CNHume  Bentley-McIlroy 3-way Partitioning
// 2016-05-24 CNHume  Assertion and dynamic pivotSamples
// 2014-12-27 CNHume  Converted to an instantiable class
// 2014-12-13 CNHume  Added Parse() method
//
// Conditionals:
//
//#define LinearEntries
#define Reverse

namespace Sort {
  using System;
  using System.Linq;

  class Program {
    #region Properties
    public static SortTest<Int32> Tester { get; private set; }
    #endregion

    static void Main(String[] args) {
      try {
        var cmd = new Command();
        cmd.Parse(args);
        var length = cmd.Length.Value;
#if LinearEntries
        var entries = linearEntries(length);
#if Reverse
        entries.Reverse();
#endif
#else
        //
        //[Note]fillRandom() case ran about four times faster
        // over 108M entries under the Tripartite conditional
        //
        var entries = randomEntries(length);
#endif
        Tester = new SortTest<Int32>();
        Tester.TestSort(entries, cmd.InsertionLimit, cmd.Print);
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
      const Int32 scale = 120;
      var range = length / scale;

      var dt = DateTime.Now;
      Console.WriteLine("{0:HH:mm:ss.fff} Building {1:n0} random entries", dt, length);

      var seed = (Int32)dt.Ticks;
      var r = new Random(seed);

      var entries = new Int32[length];
      for (var index = 0; index < length; index++)
        entries[index] = r.Next(range) + 1;

      return entries;
    }
  }
}
