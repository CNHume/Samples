//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2014-12-09 CNHume  Created File
//
// Conditionals:
//
//#define TestRanges

namespace MergeSort {
  using InsertionSort;

  using SortTests;

  public class MergeList<T> where T : IComparable {
    #region Constants
    public const UInt32 INSERTION_LIMIT_DEFAULT = 20;
    public const Int32 MERGES_DEFAULT = 6;
    #endregion

    #region Properties
    public IMeter? Meter { get; }
    public UInt32 InsertionLimit { get; set; }
    protected UInt32[]? Positions { get; set; }
    private Int32 merges;
    public Int32 Merges {
      get { return merges; }
      set {
        // A minimum of 2 merges are required
        if (value > 1)
          merges = value;
        else {
          var message = $"value = {value} must be greater than one";
          throw new ArgumentOutOfRangeException(nameof(Merges), message);
        }

        if (Positions?.Length != merges)
          Positions = new UInt32[merges];
      }
    }
    private InsertionList<T> InsertionSorter { get; }
    #endregion

    #region Constructors
    public MergeList(UInt32 insertionLimit = INSERTION_LIMIT_DEFAULT, Int32 merges = MERGES_DEFAULT, IMeter? meter = default) {
      Meter = meter;
      InsertionLimit = insertionLimit;
      Merges = merges;
      InsertionSorter = new InsertionList<T>(Meter);
    }

    public MergeList(IMeter meter) : this(INSERTION_LIMIT_DEFAULT, MERGES_DEFAULT, meter) {
    }
    #endregion

    #region Sort Methods
    public List<T> Sort(List<T> entries) {
      return Sort(entries, 0, entries.Count - 1);
    }

    // top-down K-way Merge Sort
    public List<T> Sort(List<T> entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      if (length < 2) return entries;
      if (length < Merges || length < InsertionLimit) {
        return InsertionSorter.Sort(entries, first, last);
      }
#if TestRanges
      Console.WriteLine("Entries: " + String.Join(" ", entries));
#endif
      List<List<T>> ranges = new(Merges);
      var remaining = length;
      var mergeSize = length / Merges;
      for (var left = first; left <= last; left += mergeSize, remaining -= mergeSize) {
        var rangeSize = Math.Min(mergeSize, remaining);
        var range = entries.GetRange(left, rangeSize);
        var merge = Sort(range);
        Meter?.IncMove((UInt32)merge.Count);
        ranges.Add(merge);
      }

      var result = Merge(ranges);
#if TestRanges
      Console.WriteLine("Result: " + String.Join(" ", result));
#endif
      return result;
    }

    public List<T> Merge(List<List<T>> ranges) {
      List<T> merge = [];

      while (true) {
        List<T>? found = default;
        T? node = default;
        foreach (var range in ranges)
          if (range.Count > 0) {
            var next = range[0];
            var isLess = found == null;
            if (found != null) {
              Meter?.IncCompare();
              isLess = next.CompareTo(node) <= 0;
            }

            if (isLess) {
              found = range;
              node = next;
            }
          }

        if (found == null)
          break;

        Meter?.IncMove(2);
        // Remove entry
        found.RemoveAt(0);
        merge.Add(node);
      }

      return merge;
    }
    #endregion
  }
}
