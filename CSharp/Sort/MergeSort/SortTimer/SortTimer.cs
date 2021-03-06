﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define TestList
#define ShowCounts

namespace Sort {
  using MergeSort;

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
    public void Sort(T[] entries, Boolean print, UInt32? trials, UInt32? insertionLimit, Int32? merges) {
      if (!trials.HasValue) trials = 1;
#if TestList
      var input = entries.ToList();
      Header(input, print, typeof(MergeList<T>));
#else
      Header(entries, print, typeof(MergeSort<T>));
#endif
      Start();
      var meter = (IMeter)this;
#if TestList
      var sorter = insertionLimit.HasValue ?
        merges.HasValue ?
          new MergeList<T>(meter, insertionLimit.Value, merges.Value) :
          new MergeList<T>(meter, insertionLimit.Value) :
        merges.HasValue ?
          new MergeList<T>(meter, MergeList<T>.INSERTION_LIMIT_DEFAULT, merges.Value) :
          new MergeList<T>(meter);
#else
      var sorter = insertionLimit.HasValue ?
        merges.HasValue ?
          new MergeSort<T>(meter, insertionLimit.Value, merges.Value) :
          new MergeSort<T>(meter, insertionLimit.Value) :
        merges.HasValue ?
          new MergeSort<T>(meter, MergeSort<T>.INSERTION_LIMIT_DEFAULT, merges.Value) :
          new MergeSort<T>(meter);
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
        var output = sorter.Sort(input);
#else
        sorter.Sort(entries);
#endif
#endif
        Stop();
        Display();
#if TestList
        Footer(output, print);
#else
        Footer(entries, print);
#endif
      }
    }
    #endregion
  }
}
