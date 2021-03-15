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
    public void Sort(T[] entries, Boolean print, Int32? insertionLimit, Int32? merges) {
      Header(entries, print, GetType());

      var sorter = insertionLimit.HasValue ?
        merges.HasValue ?
          new MergeSort<T>(this, insertionLimit.Value, merges.Value) :
          new MergeSort<T>(this, insertionLimit.Value) :
        merges.HasValue ?
          new MergeSort<T>(this, MergeSort<T>.INSERTION_LIMIT_DEFAULT, merges.Value) :
          new MergeSort<T>(this);

      Start();
#if TestRuntimeSort
      Array.Sort(entries);
#else
      sorter.Sort(entries);
#endif
      Stop();
      Display();
      Footer(entries, print);
    }
    #endregion
  }
}
