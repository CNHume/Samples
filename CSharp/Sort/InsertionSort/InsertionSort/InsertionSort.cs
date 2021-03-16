//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//

namespace InsertionSort {
  using SortTest;

  using System;

  public class InsertionSort<T> where T : IComparable {
    #region Constructors
    public InsertionSort(IMeter meter = default) {
      this.Meter = meter;
    }
    #endregion

    #region Properties
    public IMeter Meter { get; init; }
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
