//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2014-12-09 CNHume  Created File
//
namespace Sort {
  using System;
  using System.Collections.Generic;

  public static class MergeList<T> where T : IComparable {
    #region Constants
    private const Int32 insertionLimit = 20;
    private const Int32 mergesDefault = 6;
    #endregion

    #region Properties
    public static Int32 InsertionLimit { get; set; }
    public static Int32 Merges { get; set; }
    #endregion

    #region Constructors
    static MergeList() {
      Merges = mergesDefault;
    }
    #endregion

    #region Sort Methods
    public static List<T> Sort(List<T> entries) {
      return Sort(entries, 0, entries.Count - 1);
    }

    // top-down n-way Merge Sort
    public static List<T> Sort(List<T> entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      if (length < 2)
        return entries;

      if (length < Merges || length < insertionLimit) {
        InsertionSort<T>.Sort(entries, first, last);
        return entries;
      }

      var ranges = new List<List<T>>(Merges);
      var remaining = length;
      var mergeSize = length / Merges;
      for (var left = first; left <= last; left += mergeSize, remaining -= mergeSize) {
        var rangeSize = Math.Min(mergeSize, remaining);
        var range = entries.GetRange(left, rangeSize);
        ranges.Add(Sort(range));
      }

      return Merge(ranges);
    }

    public static List<T> Merge(List<List<T>> ranges) {
      var merge = new List<T>();

      while (true) {
        var found = (List<T>)null;
        T node = default(T);
        foreach (var range in ranges)
          if (range.Count > 0) {
            var next = range[0];
            if (found == null || node.CompareTo(next) > 0) {
              found = range;
              node = next;
            }
          }

        if (found == null)
          break;

        found.RemoveAt(0);
        merge.Add(node);
      }

      return merge;
    }
    #endregion
  }
}
