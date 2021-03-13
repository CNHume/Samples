//
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

  class SortTest<T> where T : IComparable {
    #region Constants
    private const String delim = ", ";
    private const char space = ' ';
    #endregion

    #region Constructors
    public SortTest() {
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

    #region Test Methods
    public void TestSort(T[] entries, Int32? insertionLimit, Boolean print = false) {
      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(Counter, insertionLimit.Value) :
        new QuickSort<T>(Counter);

      if (print) {
        Console.WriteLine("input:");
        Console.WriteLine(Join(delim, entries));
      }
      Counter.Header();
      Counter.Start();
#if TestRuntimeSort
      Array.Sort(entries);
#else
      sorter.Sort(entries);
#endif
      Counter.Stop();
      Counter.Display();
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
