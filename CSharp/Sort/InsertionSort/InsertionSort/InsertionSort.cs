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
    public IMeter Meter { get; }
    #endregion

    #region Methods
    public void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public void Sort(T[] entries, Int32 first, Int32 last) {
      for (var next = first + 1; next <= last; next++)
        insert(entries, first, next);
    }

    /// <summary>Bubble next entry up to its sorted location, assuming entries[first:next - 1] are already sorted.</summary>
    private void insert(T[] entries, Int32 first, Int32 next) {
      var entry = entries[next];

      while (next > first) {
        Meter?.IncCompare();
        if (entries[next - 1].CompareTo(entry) <= 0) break;

        entries[next] = entries[--next];
        Meter?.IncMove();
      }

      entries[next] = entry;
      Meter?.IncMove();
    }
    #endregion
  }
}
