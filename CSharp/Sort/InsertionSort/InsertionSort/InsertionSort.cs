﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// Conditionals:
//
#define CountCompare
#define CountMove

namespace Sort {
  using System;

  public class InsertionSort<T> where T : IComparable {
    #region Constructors
    public InsertionSort(Counter counter = default) {
      this.Counter = counter;
    }
    #endregion

    #region Properties
    public Counter Counter { get; init; }
    #endregion

    #region Methods
    public void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public void Sort(T[] entries, Int32 first, Int32 last) {
      for (var index = first + 1; index <= last; index++)
        insert(entries, first, index);
    }

    private void insert(T[] entries, Int32 first, Int32 index) {
      var entry = entries[index];

      while (index > first) {
#if CountCompare
        Counter.CompareCount++;
#endif
        if (entries[index - 1].CompareTo(entry) <= 0) break;

        entries[index] = entries[--index];
#if CountMove
        Counter.MoveCount++;
#endif
      }

      entries[index] = entry;
#if CountMove
      Counter.MoveCount++;
#endif
    }
    #endregion
  }
}
