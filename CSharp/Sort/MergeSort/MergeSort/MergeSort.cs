//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2014-12-27 CNHume  Converted to an instantiable class
// 2014-12-22 CNHume  Clarified size calculation
// 2014-12-13 CNHume  Converted from List Ranges to Array Slices
// 2014-12-09 CNHume  Created File
//

namespace MergeSort {
  using InsertionSort;

  using SortTest;

  using System;

  public class MergeSort<T> where T : IComparable {
    #region Constants
    public const Int32 INSERTION_LIMIT_DEFAULT = 12;
    public const Int32 MERGES_DEFAULT = 6;
    #endregion

    #region Properties
    public IMeter Meter { get; }
    public Int32 InsertionLimit { get; }
    protected Int32[] Positions { get; set; }

    private Int32 merges;
    public Int32 Merges {
      get { return merges; }
      set {
        // A minimum of 2 merges are required
        if (value > 1)
          merges = value;
        else
          throw new ArgumentOutOfRangeException();

        if (Positions is null || Positions.Length != merges)
          Positions = new Int32[merges];
      }
    }

    private InsertionSort<T> InsertionSorter { get; }
    #endregion

    #region Constructors
    public MergeSort(IMeter meter = default, Int32 insertionLimit = INSERTION_LIMIT_DEFAULT, Int32 merges = MERGES_DEFAULT) {
      this.Meter = meter;
      this.InsertionLimit = insertionLimit;
      this.Merges = merges;
      this.InsertionSorter = new InsertionSort<T>(Meter);
    }
    #endregion

    #region Sort Methods
    public void Sort(T[] entries) {
      // Allocate merge buffer
      var entries2 = new T[entries.Length];
      Sort(entries, entries2, 0, entries.Length - 1);
    }

    // Top-Down K-way Merge Sort
    public void Sort(T[] entries1, T[] entries2, Int32 first, Int32 last) {
      var length = last + 1 - first;
      if (length < 2)
        return;
      else if (length < InsertionLimit) {
        InsertionSorter.Sort(entries1, first, last);
        return;
      }

      var left = first;
      var size = ceiling(length, Merges);
      for (var remaining = length; remaining > 0; remaining -= size, left += size) {
        var right = left + Math.Min(remaining, size) - 1;
        Sort(entries1, entries2, left, right);
      }

      Merge(entries1, entries2, first, last);
      Meter?.IncMove((UInt32)length);
      Array.Copy(entries2, first, entries1, first, length);
    }
    #endregion

    #region Merge Methods
    public void Merge(T[] entries1, T[] entries2, Int32 first, Int32 last) {
      Array.Clear(Positions, 0, Merges);
      // This implementation has a quadratic time dependency on the number of merges
      for (var index = first; index <= last; index++)
        entries2[index] = remove(entries1, first, last);
    }

    private T remove(T[] entries, Int32 first, Int32 last) {
      T entry = default;
      Int32? found = default;
      var length = last + 1 - first;

      var index = 0;
      var left = first;
      var size = ceiling(length, Merges);
      for (var remaining = length; remaining > 0; remaining -= size, left += size, index++) {
        var position = Positions[index];
        if (position < Math.Min(remaining, size)) {
          var next = entries[left + position];
          var isLess = !found.HasValue;
          if (found.HasValue) {
            Meter?.IncCompare();
            isLess = entry.CompareTo(next) > 0;
          }

          if (isLess) {
            found = index;
            entry = next;
          }
        }
      }

      Meter?.IncMove(2);
      // Remove entry
      Positions[found.Value]++;
      return entry;
    }
    #endregion

    #region Math Methods
    private static Int32 ceiling(Int32 numerator, Int32 denominator) {
      return (numerator + denominator - 1) / denominator;
    }
    #endregion
  }
}
