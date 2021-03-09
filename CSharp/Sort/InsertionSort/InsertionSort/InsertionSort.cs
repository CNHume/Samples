//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
namespace Sort {
  using System;

  static class InsertionSort<T> where T : IComparable {
    #region Sort Methods
    public static void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public static void Sort(T[] entries, Int32 first, Int32 last) {
      for (var index = first + 1; index <= last; index++)
        insert(entries, first, index);
    }

    private static void insert(T[] entries, Int32 first, Int32 index) {
      var entry = entries[index];
      while (index > first && entries[index - 1].CompareTo(entry) > 0)
        entries[index] = entries[--index];
      entries[index] = entry;
    }
    #endregion

    #region Swap Methods for Worst Case Testing
    /// <summary>Swap two entities of type T.</summary>
    public static void Swap(ref T e1, ref T e2) {
      var e = e1;
      e1 = e2;
      e2 = e;
    }

    /// <summary>Swap entries at the left and right indicies.</summary>
    /// <param name="entries"></param>
    /// <param name="left">Left index</param>
    /// <param name="right">Right index</param>
    public static void Swap(T[] entries, Int32 left, Int32 right) {
      Swap(ref entries[left], ref entries[right]);
    }

    public static void Reverse(T[] entries) {
      for (Int32 left = 0, right = entries.Length - 1; left < right; left++, right--)
        Swap(entries, left, right);
    }
    #endregion
  }
}
