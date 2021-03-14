﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
#define ShowCounts

namespace Sort {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  using static System.String;

  class SortTimer<T> where T : IComparable {
    #region Constants
    private const Int32 SORT_TRIALS = 4;

    private const String delim = ", ";
    private const char space = ' ';
    #endregion

    #region Constructors
    public SortTimer() {
      var sb = new StringBuilder("Starting");
#if TestRuntimeSort
      if (sb.Length > 0) sb.Append(space);
      sb.Append("Runtime Sort");
#endif
      Counter = new Counter(sb.ToString(), typeof(Heap<T>));
    }
    #endregion

    #region Properties
    public Counter Counter { get; init; }
    #endregion

    #region Methods
    public void Sort(T[] entries, Boolean print = false) {
      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(Join(delim, entries));
      }

      //
      // Note: The Heap class appropriates the entries array to its own use.
      //       It does not make a private copy!
      //
      var sorter = new Heap<T>(entries, entries.Length);

      for (var trial = 0; trial < SORT_TRIALS; trial++) {
        Counter.Header();
        Counter.Start();
#if TestRuntimeSort
        var ascending = true;
        Array.Sort(entries);
#else
        var ascending = sorter.IsAscending;
        sorter.Sort();
#endif
        Counter.Stop();
        Counter.Display();
        Counter.Footer(entries.Length, IsSorted(entries, ascending));

        if (print) {
          Console.WriteLine("output:");
          Console.WriteLine(Join(delim, entries));
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
    #endregion
  }
}