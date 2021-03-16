//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define TestInsertionList
//#define TestRuntimeSort
#define ShowCounts

namespace InsertionSort {
  using SortTest;
  using SortTest.Extensions;

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
    public void Sort(T[] entries, Boolean print) {
#if TestInsertionList
      var input = entries.ToList();
      Header(input, print, typeof(InsertionList<T>));
#else
      Header(entries, print, typeof(InsertionSort<T>));
#endif
      var meter = (IMeter)this;

      Start();
#if TestInsertionList
#if TestRuntimeSort
      input.Sort();
      var output = input;
#else
      var sorter = new InsertionList<T>(meter);
      sorter.Sort(input);
#endif
#else
#if TestRuntimeSort
      Array.Sort(entries);
#else
      var sorter = new InsertionSort<T>(meter);
      sorter.Sort(entries);
#endif
#endif
      Stop();
      Display();
      Footer(entries, print);
    }
#endregion
  }
}
