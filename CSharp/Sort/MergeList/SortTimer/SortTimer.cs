//
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

  class SortTimer<T> : SortMeter<T> where T : IComparable {
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
      this.Mode = sb.ToString();
    }
    #endregion

    #region Methods
    public void Sort(IEnumerable<T> entries, Boolean print, Int32? insertionLimit, Int32? merges) {
      var input = entries.ToList();
      Header(input, print, GetType());

      var counter = (IMeter)this;
      var sorter = insertionLimit.HasValue ?
        merges.HasValue ?
          new MergeList<T>(counter, insertionLimit.Value, merges.Value) :
          new MergeList<T>(counter, insertionLimit.Value) :
        merges.HasValue ?
          new MergeList<T>(counter, MergeList<T>.INSERTION_LIMIT_DEFAULT, merges.Value) :
          new MergeList<T>(counter);

      Start();
#if TestRuntimeSort
      input.Sort();
      var output = input;
#else
      var output = sorter.Sort(input);
#endif
      Stop();
      Display();
      Footer(output, print);
    }
    #endregion
  }
}
