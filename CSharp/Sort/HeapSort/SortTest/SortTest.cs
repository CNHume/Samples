//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define PrintLists

namespace HeapSort {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  using static System.String;

  static class SortTest<T> where T : IComparable {
    public static void TestSort(T[] entries, Boolean print) {
      const String sDelimiter = ", ";
      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(Join<T>(sDelimiter, entries));
      }
      var timer = new Stopwatch();
      var length = entries.Length;
      var sorter = new Heap<T>(entries, length);
      for (var n = 0; n < 4; n++) {
        timer.Reset();
        Console.WriteLine("{0:HH:mm:ss.fff} Starting", DateTime.Now);
        timer.Start();
#if TestRuntimeSort
        var ascending = true;
        Array.Sort(entries);
#else
        var ascending = sorter.IsAscending;
        sorter.Sort();
#endif
        timer.Stop();
        sorter.Reverse();
        //
        // There are 10,000 ticks per msec
        //
        var msec = (Double)timer.ElapsedTicks / 10000;
        var sorted = IsSorted(entries, ascending);
        Console.WriteLine("{0:HH:mm:ss.fff} Finished, Sorted = {1}", DateTime.Now, sorted);
        if (print) {
          Console.WriteLine("output:");
          Console.WriteLine(Join<T>(sDelimiter, entries));
        }
        if (msec == 0)
          Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec",
                            length, msec / 1000);
        else {
          var rate = length / msec; // From 1.4 to 2.5 MHz on an i7-4702HQ @ 2.2 GHz
          Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec, Rate = {2:0.0##} KHz",
                            length, msec / 1000, rate);
        }
      }
    }

    public static Boolean IsSorted(IEnumerable<T> en, Boolean ascending = true) {
      if (en.Any()) {
        var last = en.First();
        foreach (var next in en.Skip(1)) {
          var sense = next.CompareTo(last);
          if (sense < 0 && ascending ||
              sense > 0 && !ascending)
            return false;

          last = next;
        }
      }

      return true;
    }
  }
}
