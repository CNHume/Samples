//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
#define ShowCounts

namespace Sort {
  using System;
  using System.Text;

  class SortTimer<T> : Counter<T> where T : IComparable {
    #region Constants
    private const Int32 SORT_TRIALS = 4;
    private const char space = ' ';
    #endregion

    #region Constructors
    public SortTimer() {
      var sb = new StringBuilder("Starting");
#if TestRuntimeSort
      if (sb.Length > 0) sb.Append(space);
      sb.Append("Runtime Sort");
#endif
    }
    #endregion

    #region Methods
    public void Sort(T[] entries, Boolean print) {
      Header(entries, print, GetType());
      Start();

      //
      // Note: The Heap class appropriates the entries array to its own use.
      //       It does not make a private copy!
      //
      var sorter = new Heap<T>(this, entries, entries.Length);

      for (var trial = 0; trial < SORT_TRIALS; trial++) {
        if (trial > 0) {
          Reset();
          Start();
        }
#if TestRuntimeSort
        var ascending = true;
        Array.Sort(entries);
#else
        var ascending = sorter.IsAscending;
        sorter.Sort();
#endif
        Stop();
        Display();
        Footer(entries, print, ascending);
      }
    }
    #endregion
  }
}
