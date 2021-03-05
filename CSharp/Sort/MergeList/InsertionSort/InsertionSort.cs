//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
namespace Sort {
  using System;
  using System.Collections.Generic;

  static class InsertionSort<T> where T : IComparable {
    public static void Sort(List<T> entries) {
      Sort(entries, 0, entries.Count - 1);
    }

    public static void Sort(List<T> entries, Int32 first, Int32 last) {
      for (var index = first + 1; index <= last; index++)
        insert(entries, first, index);
    }

    private static void insert(List<T> entries, Int32 first, Int32 index) {
      var entry = entries[index];
      while (index > first && entries[index - 1].CompareTo(entry) > 0)
        entries[index] = entries[--index];
      entries[index] = entry;
    }
  }
}
