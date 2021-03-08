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
//#define LinearFill
#define Reverse

namespace Sort {
  using Sort;
  using System;

  class Program {
    static void Main(String[] args) {
      try {
        var cmd = new Command();
        cmd.Parse(args);

        var entries = new Int32[cmd.Length.Value];
#if LinearFill
        fillLinear(entries);
#if Reverse
        QuickSort<Int32>.Reverse(entries);
#endif
#else
        //
        //[Note]fillRandom() case ran about four times faster
        // over 108M entries under the Tripartite conditional
        //
        fillRandom(entries);
#endif
        SortTest<Int32>.TestSort(entries, cmd.InsertionLimit, cmd.Print);
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

    private static void fillLinear(Int32[] entries) {
      var length = entries.Length;
      var dt = DateTime.Now;
      Console.WriteLine("{0:HH:mm:ss.fff} Building {1:n0} linear entries", dt, length);

      for (var index = 0; index < length; index++)
        entries[index] = index;
    }

    private static void fillRandom(Int32[] entries) {
      var length = entries.Length;
      const Int32 scale = 120;
      var range = length / scale;

      var dt = DateTime.Now;
      Console.WriteLine("{0:HH:mm:ss.fff} Building {1:n0} random entries", dt, length);

      var seed = (Int32)dt.Ticks;
      var r = new Random(seed);

      for (var index = 0; index < length; index++)
        entries[index] = r.Next(range) + 1;
    }
  }
}
