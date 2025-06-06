﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
#define ShowCounts

using System.Text;

namespace Sorters;

using SortTests;

class SortTimer<T> : SortMeter<T> where T : IComparable {
  #region Constants
  private const String space = " ";
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
  public void Sort(T[] entries, Boolean print, UInt32? trials) {
    if (!trials.HasValue) trials = 1;

    Header(entries, typeof(Heap<T>), print);
    Start();

    //
    // Note: The Heap class appropriates the entries array to its own use.
    //       It does not make a private copy!
    //
    var meter = (IMeter)this;
    Heap<T> sorter = new(entries, meter);

    for (var trial = 0; trial < trials; trial++) {
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
