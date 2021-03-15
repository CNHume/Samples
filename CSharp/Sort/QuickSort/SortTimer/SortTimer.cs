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
  using System.Text;

  class SortTimer<T> : SortMeter<T> where T : IComparable {
    #region Constants
    private const char space = ' ';
    #endregion

    #region Constructors
    public SortTimer() {
      var sb = new StringBuilder("Starting");
#if Tripartite
      if (sb.Length > 0) sb.Append(space);
      sb.Append("Tripartite");
#endif
#if TestRuntimeSort
      if (sb.Length > 0) sb.Append(space);
      sb.Append("Runtime Sort");
#endif
      this.Mode = sb.ToString();
    }
    #endregion

    #region Methods
    public void Sort(T[] entries, Boolean print, Int32? insertionLimit) {
      Header(entries, print, GetType());

      var counter = (IMeter)this;
      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(counter, insertionLimit.Value) :
        new QuickSort<T>(counter);

      Start();
#if TestRuntimeSort
      Array.Sort(entries);
#else
      sorter.Sort(entries);
#endif
      Stop();
      Display();

      //
      // On a Dell XPS 9530 [i7-4702HQ @ 2.2 GHz w 16 GB ram] in Release Mode:
      // For Random Fill with scale = 120
      // C# sorted 12 M entries in 36 sec, n * Log(n) Rate = ~5.5 MHz
      //
      Footer(entries, print);
    }
    #endregion
  }
}
