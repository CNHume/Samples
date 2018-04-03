//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
#define Tripartite
//#define CountCompare
//#define CountMove
//#define CountPart

namespace Sort {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  static class SortTest<T> where T : IComparable {
    public static void TestSort(T[] entries, Int32? insertionLimit, Boolean print) {
      const String sDelimiter = ", ";

      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(insertionLimit.Value) : new QuickSort<T>();

      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(String.Join(sDelimiter, entries));
      }
#if TestRuntimeSort
      Console.WriteLine("TestRuntimeSort");
#endif
      var sMode = String.Empty;
#if Tripartite
     sMode += " Tripartite";
#endif
      var timer = new Stopwatch();
      Console.WriteLine("{0:HH:mm:ss.fff} Starting{1}", DateTime.Now, sMode);
      timer.Start();
#if TestRuntimeSort
      Array.Sort<T>(entries);
#else
      sorter.Sort(entries);
      //[Test]InsertionSort<T>.Sort(entries);
#endif
      timer.Stop();
      var msec = (Double)timer.ElapsedMilliseconds;
      Console.WriteLine("{0:HH:mm:ss.fff} Finished, Sorted = {1}", DateTime.Now, IsSorted(entries));
      if (print) {
        Console.WriteLine("output:");
        Console.WriteLine(String.Join(sDelimiter, entries));
      }

      var n = entries.Length;
      var dNLogN = n * Math.Log(n);
      var rate = dNLogN / msec; // 50 MHz over 108 M entries on an i7-4702HQ @ 2.2 GHz in Release Mode
      Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec, n * Log(n) Rate = {2:0.0##} KHz",
                        n, msec / 1000, rate);
#if CountCompare
      Console.WriteLine("CompareCount = {0:n0}", QuickSort<T>.CompareCount);
#endif
#if CountMove
      Console.WriteLine("MoveCount = {0:n0}", QuickSort<T>.MoveCount);
#endif
#if CountPart
      Console.WriteLine("PartCount = {0:n0}", QuickSort<T>.PartCount);
#endif
    }

    #region Test Methods
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

    public static void Reverse(T[] entries) {
      for (Int32 left = 0, right = entries.Length - 1; left < right; left++, right--)
        QuickSort<T>.Swap(entries, left, right);
    }
    #endregion
  }
}
