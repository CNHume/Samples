//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2014-12-14 CNHume  Added Parse() method
// 2012-04-04 CNHume  Created HeapSort
//
// Conditionals:
//
//#define LinearEntries

namespace Sort {
  using System;

  class Program {
    static void Main(String[] args) {
      try {
        var cmd = new Command(args);
#if LinearEntries
        var entries = linearEntries(cmd.Length.Value);
#else
        var entries = randomEntries(cmd.Length.Value);
#endif
        SortTest<Int32>.TestSort(entries, cmd.Print);
      }
      catch (ApplicationException ex) {
	Console.WriteLine(ex.Message);
      }
#if DEBUG
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }

    private static Int32[] linearEntries(Int32 length) {
      var dt = DateTime.Now;
      Console.WriteLine("{0:HH:mm:ss.fff} Building {1:n0} linear entries", dt, length);

      var entries = new Int32[length + 3];
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
