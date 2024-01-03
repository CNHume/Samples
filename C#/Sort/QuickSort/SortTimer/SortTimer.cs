//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define SwapInPlace
//#define SampleMiddle
#define Tripartite
#define ShowCounts

using System.Text;

namespace Sort {
  using QuickSort;

  using SortTest;
  using SortTest.Extensions;            // For AppendDelim()

  class SortTimer<T> : SortMeter<T> where T : IComparable {
    #region Constants
    private const string space = " ";
    #endregion

    #region Constructors
    public SortTimer() {
      var sb = new StringBuilder("Starting");
#if TestRuntimeSort
      sb.AppendDelim("RuntimeSort", space);
#else
#if SampleMiddle
      sb.AppendDelim("SampleMiddle", space);
#endif
#if Tripartite
      sb.AppendDelim("Tripartite", space);
#endif
#endif
      this.Mode = sb.ToString();
    }
    #endregion

    #region Methods
    public void Sort(T[] entries, Boolean print, UInt32? trials, UInt32? insertionLimit) {
      if (!trials.HasValue) trials = 1;
#if SwapInPlace
      var type = typeof(QuickSortSwapInPlace<T>);
#else
      var type = typeof(QuickSort<T>);
#endif
      Header(entries, type, print);
      Start();

      var meter = (IMeter)this;
#if SwapInPlace
      var sorter = insertionLimit.HasValue ?
        new QuickSortSwapInPlace<T>(meter, insertionLimit.Value) :
        new QuickSortSwapInPlace<T>(meter);
#else
      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(insertionLimit.Value, meter) :
        new QuickSort<T>(meter);
#endif
      for (var trial = 0; trial < trials; trial++) {
        if (trial > 0) {
          Reset();
          Start();
        }
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
    }
    #endregion
  }
}
