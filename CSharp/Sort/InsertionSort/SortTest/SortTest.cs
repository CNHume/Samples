﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
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
      Array.Sort(entries);
#else
      InsertionSort<T>.Sort(entries);
#endif
      timer.Stop();
      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)timer.ElapsedTicks / 10000;
      Console.WriteLine("{0:HH:mm:ss.fff} Finished, Sorted = {1}", DateTime.Now, IsSorted(entries));
      if (print) {
        Console.WriteLine("output:");
        Console.WriteLine(String.Join(sDelimiter, entries));
      }

      var length = entries.Length;
      if (msec == 0)
        Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec",
                          length, msec / 1000);
      else {
        var rate = length / msec; // ~3.3 MHz over 10 M entries on an i7-4702HQ @ 2.2 GHz
        Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec, Rate = {2:0.0##} KHz",
                          length, msec / 1000, rate);
      }
    }

    #region Test Methods
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
    #endregion
  }
}
