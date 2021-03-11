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

  public class MergeList<T> where T : IComparable {
    #region Constants
    public const Int32 INSERTION_LIMIT_DEFAULT = 20;
    public const Int32 MERGES_DEFAULT = 6;
    #endregion

    #region Properties
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

        if (Positions == null || Positions.Length != merges)
          Positions = new Int32[merges];
      }
    }
    public Int32 InsertionLimit { get; set; }
    public Counter Counter { get; init; }
    private InsertionSort<T> InsertionSorter { get; init; }
    #endregion

    #region Constructors
    public MergeList(Counter counter = default, Int32 merges = MERGES_DEFAULT, Int32 insertionLimit = INSERTION_LIMIT_DEFAULT) {
      this.Counter = counter;
      this.Merges = merges;
      this.InsertionLimit = insertionLimit;
      this.InsertionSorter = new InsertionSort<T>(Counter);
    }
    #endregion

    #region Sort Methods
    public List<T> Sort(List<T> entries) {
      return Sort(entries, 0, entries.Count - 1);
    }

    // top-down n-way Merge Sort
    public List<T> Sort(List<T> entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      if (length < 2)
        return entries;

      if (length < Merges || length < INSERTION_LIMIT_DEFAULT) {
        InsertionSorter.Sort(entries, first, last);
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

    public List<T> Merge(List<List<T>> ranges) {
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
