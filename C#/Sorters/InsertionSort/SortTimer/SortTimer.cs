//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define TestList
#define ShowCounts

using System.Text;

namespace Sorters;

using InsertionSort;

using SortTests;

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
  public void Sort(T[] entries, Boolean print, UInt32? trials) {
    if (!trials.HasValue) trials = 1;
#if TestList
      var input = entries.ToList();
      Header(input, typeof(InsertionList<T>), print);
#else
    Header(entries, typeof(InsertionSort<T>), print);
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
