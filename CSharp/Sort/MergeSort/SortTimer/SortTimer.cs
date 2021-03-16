//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
#define ShowCounts

namespace Sort {
  using SortTest;
  using SortTest.Extension;

  using System;
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
    public void Sort(T[] entries, Boolean print, Int32? insertionLimit, Int32? merges) {
      Header(entries, print, GetType());

      var meter = (IMeter)this;
      var sorter = insertionLimit.HasValue ?
        merges.HasValue ?
          new MergeSort<T>(meter, insertionLimit.Value, merges.Value) :
          new MergeSort<T>(meter, insertionLimit.Value) :
        merges.HasValue ?
          new MergeSort<T>(meter, MergeSort<T>.INSERTION_LIMIT_DEFAULT, merges.Value) :
          new MergeSort<T>(meter);

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
