﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
#define Tripartite
#define ShowCounts

namespace Sort {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  using static System.String;

  class SortTimer<T> where T : IComparable {
    #region Constants
    private const String delim = ", ";
    private const char space = ' ';
    #endregion

    #region Constructors
    public SortTimer() {
      var sb = new StringBuilder("Starting");
#if Tripartite
      if (sb.Length > 0) sb.Append(space);
      sb.Append("Tripartite");
#endif
#if TestRuntimeSort
      if (sb.Length > 0) sb.Append(space);
      sb.Append("Runtime Sort");
#endif
      Counter = new Counter(sb.ToString(), typeof(QuickSort<T>));
    }
    #endregion

    #region Properties
    public Counter Counter { get; init; }
    #endregion

    #region Methods
    public void Sort(T[] entries, Int32? insertionLimit, Boolean print = false) {
      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(Join(delim, entries));
      }

      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(Counter, insertionLimit.Value) :
        new QuickSort<T>(Counter);

      Counter.Header();
      Counter.Start();
#if TestRuntimeSort
      Array.Sort(entries);
#else
      sorter.Sort(entries);
#endif
      Counter.Stop();
      Counter.Display();
      //
      // On a Dell XPS 9530 [i7-4702HQ @ 2.2 GHz w 16 GB ram] in Release Mode:
      // For Random Fill with scale = 120
      // C# sorted 12 M entries in 36 sec, n * Log(n) Rate = ~5.5 MHz
      //
      Counter.Footer(entries.Length, IsSorted(entries));

      if (print) {
        Console.WriteLine("output:");
        Console.WriteLine(Join(delim, entries));
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
    #endregion
  }
}
