//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestRuntimeSort
//#define SampleMiddle
//#define SampleRandomly
#define Tripartite
#define ShowCounts

namespace Sort {
  using QuickSort;

  using SortTest;
  using SortTest.Extensions;

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
#else
#if SampleMiddle
      sb.AppendDelim("SampleMiddle", space);
#elif SampleRandomly
      sb.AppendDelim("SampleRandomly", space);
#endif
#if Tripartite
      sb.AppendDelim("Tripartite", space);
#endif
#endif
      this.Mode = sb.ToString();
    }
    #endregion

    #region Methods
    public void Sort(T[] entries, Boolean print, Int32? trials, Int32? insertionLimit) {
      if (!trials.HasValue) trials = 1;

      Header(entries, print, typeof(QuickSort<T>));
      Start();

      var meter = (IMeter)this;
      var sorter = insertionLimit.HasValue ?
        new QuickSort<T>(meter, insertionLimit.Value) :
        new QuickSort<T>(meter);

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
