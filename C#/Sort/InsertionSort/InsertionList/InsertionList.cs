//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//

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
    public IMeter Meter { get; }
    #endregion

    #region Methods
    public List<T> Sort(List<T> entries) {
      return Sort(entries, 0, entries.Count - 1);
    }

    public List<T> Sort(List<T> entries, Int32 first, Int32 last) {
      for (var next = first + 1; next <= last; next++)
        insert(entries, first, next);
      return entries;
    }

    /// <summary>Bubble next entry up to its sorted location, assuming entries[first:next - 1] are already sorted.</summary>
    private void insert(List<T> entries, Int32 first, Int32 next) {
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
