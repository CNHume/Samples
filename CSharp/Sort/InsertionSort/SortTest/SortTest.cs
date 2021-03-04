//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define PrintLists

namespace Sort {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  static class SortTest<T> where T : IComparable {
    public static void TestSort(T[] entries, Boolean print) {
      const String sDelimiter = ", ";
      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(String.Join(sDelimiter, entries));
      }
      var timer = new Stopwatch();
      Console.WriteLine("{0:HH:mm:ss.fff} Starting", DateTime.Now);
      timer.Start();
#if TestRuntimeSort
      Array.Sort<T>(entries);
#else
      InsertionSort<T>.Sort(entries);
#endif
      timer.Stop();
      var msec = (Double)timer.ElapsedMilliseconds;
      Console.WriteLine("{0:HH:mm:ss.fff} Finished, Sorted = {1}", DateTime.Now, IsSorted(entries));
      if (print) {
        Console.WriteLine("output:");
        Console.WriteLine(String.Join(sDelimiter, entries));
      }
      var rate = entries.Length / msec; // ~3.3 MHz over 10 M entries on an i7-4702HQ @ 2.2 GHz
      Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec, Rate = {2:0.0##} KHz",
                        entries.Length, msec / 1000, rate);
    }

    public static Boolean IsSorted(IEnumerable<T> en) {
      if (en.Any()) {
        var last = en.First();
        foreach (var next in en.Skip(1))
          if (last.CompareTo(next) > 0)
            return false;
          else
            last = next;
      }

      return true;
    }
  }
}
