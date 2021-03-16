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

namespace InsertionSort {
  using SortTest;

  using System;
  using System.Collections.Generic;

  public class InsertionList<T> where T : IComparable {
    #region Constructors
    public InsertionList(IMeter meter = default) {
      this.Meter = meter;
    }
    #endregion

    #region Properties
    public IMeter Meter { get; init; }
    #endregion

    #region Methods
    public void Sort(List<T> entries) {
      Sort(entries, 0, entries.Count - 1);
    }

    public void Sort(List<T> entries, Int32 first, Int32 last) {
      for (var index = first + 1; index <= last; index++)
        insert(entries, first, index);
    }

    private void insert(List<T> entries, Int32 first, Int32 index) {
      var entry = entries[index];

      while (index > first) {
        Meter?.IncCompare();
        if (entries[index - 1].CompareTo(entry) <= 0) break;

        entries[index] = entries[--index];
        Meter?.IncMove();
      }

      entries[index] = entry;
      Meter?.IncMove();
    }
    #endregion
  }
}
