//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
#define Tripartite
//#define CountCompare
//#define CountMove
//#define CountPart
//#define TestRuntimeSort

//
// Use ScaleWork to compare performance of a given algorithm at increasing scales.
// Do not use ScaleWork to compare different Sort Algorithms at the same scale.
//
//#define ScaleWork

namespace Sort {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  using static System.String;

  static class SortTest<T> where T : IComparable {
    #region Properties
#if CountCompare
    public static UInt64 CompareCount { get; set; }
#endif
#if CountMove
    public static UInt64 MoveCount { get; set; }
#endif
#if CountPart
    public static UInt64 PartCount { get; set; }
#endif
    #endregion

    #region Test Methods
    public static void TestSort(T[] entries, Int32? insertionLimit, Boolean print) {
      const String sDelimiter = ", ";

      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(insertionLimit.Value) : new QuickSort<T>();

      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(Join(sDelimiter, entries));
      }
#if TestRuntimeSort
      Console.WriteLine("TestRuntimeSort");
#endif
      var sMode = Empty;
#if Tripartite
      sMode += " Tripartite";
#endif
      var timer = new Stopwatch();
      Console.WriteLine("{0:HH:mm:ss.fff} Starting{1}", DateTime.Now, sMode);
      timer.Start();
#if TestRuntimeSort
      Array.Sort(entries);
#else
      sorter.Sort(entries);
      //[Test]InsertionSort<T>.Sort(entries);
#endif
      timer.Stop();
      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)timer.ElapsedTicks / 10000;
      Console.WriteLine("{0:HH:mm:ss.fff} Finished, Sorted = {1}", DateTime.Now, IsSorted(entries));
      if (print) {
        Console.WriteLine("output:");
        Console.WriteLine(Join(sDelimiter, entries));
      }

      var length = entries.Length;
      if (msec == 0)
        Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec",
                          length, msec / 1000);
      else {
#if ScaleWork
        //
        // On a Dell XPS 9530 [i7-4702HQ @ 2.2 GHz w 16 GB ram] in Release Mode:
        // For Random Fill with scale = 120
        // C# sorted 12 M entries in 36 sec, n * Log(n) Rate = ~5.5 MHz
        //
        var dNLogN = scaleLog10(length);
        var rate = dNLogN / msec;
        Console.WriteLine("Sorted a total of {0:n0} * log10(n) = {1:0.0##} entries in {2:0.0##} sec, Rate = {3:0.0##} KHz",
                          length, dNLogN, msec / 1000, rate);
#else
        var rate = length / msec;
        Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec, Rate = {2:0.0##} KHz",
                          length, msec / 1000, rate);
#endif
      }
#if CountCompare
      Console.WriteLine("CompareCount = {0:n0}", CompareCount);
#endif
#if CountMove
      Console.WriteLine("MoveCount = {0:n0}", MoveCount);
#endif
#if CountPart
      Console.WriteLine("PartCount = {0:n0}", PartCount);
#endif
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
    #endregion

    #region Math Methods
    private static double scaleLog10(Int32 length) {
      return length * Math.Log10(length);
    }
    #endregion
  }
}
