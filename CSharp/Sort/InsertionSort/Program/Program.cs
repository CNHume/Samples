//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2014-12-13 CNHume  Added Parse() method
//
// Conditionals:
//
//#define LinearEntries
//#define Reverse

namespace InsertionSort {
  using Sort;
  using System;

  class Program {
    static void Main(String[] args) {
      try {
        var cmd = new Command();
        cmd.Parse(args);
#if LinearEntries
        var entries = linearEntries(cmd.Length.Value);
#else
        var entries = randomEntries(cmd.Length.Value);
#endif
#if Reverse
        InsertionSort<Int32>.Reverse(entries);
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
  }
}
