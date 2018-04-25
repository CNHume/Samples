//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// Conditionals:
//
//#define CountCompare
//#define CountMove

namespace Sort {
  using System;

  static class InsertionSort<T> where T : IComparable {
    public static void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public static void Sort(T[] entries, Int32 first, Int32 last) {
      for (var index = first + 1; index <= last; index++)
        insert(entries, first, index);
    }

    private static void insert(T[] entries, Int32 first, Int32 index) {
      var entry = entries[index];

      while (index > first) {
#if CountCompare
        QuickSort<T>.CompareCount++;
#endif
        if (entries[index - 1].CompareTo(entry) <= 0) break;

        entries[index] = entries[--index];
#if CountMove
        QuickSort<T>.MoveCount++;
#endif
      }

      entries[index] = entry;
#if CountMove
      QuickSort<T>.MoveCount++;
#endif
    }
  }
}
