//
// (C) Copyright 2013, Christopher N. Hume.  All rights reserved.
//
// 2014-12-09 CNHume  Created File
//

using System;
using System.Collections.Generic;
using System.Linq;

namespace Sort {
  public static class MergeSort<T> where T : IComparable {
    #region Constants
    private const Int32 insertionLimit = 20;
    #endregion

    #region Properties
    private const Int32 MergesDefault = 6;
    public static Int32 Merges { get; set; }
    #endregion

    #region Constructors
    static MergeSort() {
      Merges = MergesDefault;
    }
    #endregion

    public static List<T> Sort(List<T> entries) {
      return Sort(entries, 0, entries.Count - 1);
    }

    // top-down n-way Merge Sort
    public static List<T> Sort(List<T> entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      if (length < 2)
        return entries;

      if (length < Merges/* || length < insertionLimit*/) {
        InsertionSort<T>.Sort(entries.ToArray(), first, last);
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
  }
}
