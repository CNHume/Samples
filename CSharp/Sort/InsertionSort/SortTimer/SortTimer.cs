//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define TestList
#define ShowCounts

namespace Sort {
  using InsertionSort;

  using SortTest;
  using SortTest.Extensions;            // For AppendDelim()

  using System;
  using System.Linq;
  using System.Text;

  class SortTimer<T> : SortMeter<T> where T : IComparable {
    #region Constants
    private const string space = " ";
    #endregion

    #region Constructors
    public SortTimer() {
      var sb = new StringBuilder("Starting");
#if TestRuntimeSort
      sb.AppendDelim("RuntimeSort", space);
#endif
      this.Mode = sb.ToString();
    }
    #endregion

    #region Methods
    public void Sort(T[] entries, Boolean print, Int32? trials) {
      if (!trials.HasValue) trials = 1;
#if TestList
      var input = entries.ToList();
      Header(input, print, typeof(InsertionList<T>));
#else
      Header(entries, print, typeof(InsertionSort<T>));
#endif
      Start();
      var meter = (IMeter)this;
#if !TestRuntimeSort
#if TestList
      var sorter = new InsertionList<T>(meter);
#else
      var sorter = new InsertionSort<T>(meter);
#endif
#endif
      for (var trial = 0; trial < trials; trial++) {
        if (trial > 0) {
          Reset();
          Start();
        }
#if TestRuntimeSort
#if TestList
        input.Sort();
        var output = input;
#else
        Array.Sort(entries);
#endif
#else
#if TestList
        sorter.Sort(input);
#else
        sorter.Sort(entries);
#endif
#endif
        Stop();
        Display();
        Footer(entries, print);
      }
    }
    #endregion
  }
}
