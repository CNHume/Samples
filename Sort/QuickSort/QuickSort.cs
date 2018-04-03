//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// Conditionals:
//
//#define VerifyAssertions
//#define CountCompare
//#define CountMove
//#define CountPart

//
// The Tripartite conditional enables Bentley-McIlroy 3-way Partitioning.
// This performs additional compares to isolate islands of keys equal to
// the pivot value.  Use unless key-equivalent classes are of small size.
//
#define Tripartite

namespace Sort {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;

  class QuickSort<T> where T : IComparable {
    #region Constants
    private const Int32 insertionLimitDefault = 12;
    #endregion

    #region Properties
#if CountCompare
    public static UInt64 CompareCount { get; set; }
#endif
#if CountMove
    public static UInt64 MoveCount { get; set; }
#endif
#if CountPart
    public static UInt64 PartCount { get; set; }
#endif
    public Int32 InsertionLimit { get; set; }
    protected Random Random { get; set; }
    #endregion

    #region Constructors
    public QuickSort(Int32 insertionLimit, Random random) {
      InsertionLimit = insertionLimit;
      Random = random;
    }

    public QuickSort(Int32 insertionLimit)
      : this(insertionLimit, new Random()) {
    }

    public QuickSort()
      : this(insertionLimitDefault) {
    }
    #endregion

    #region Sort Methods
    public void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public void Sort(T[] entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      while (length > 1) {
        if (length < InsertionLimit) {
          InsertionSort<T>.Sort(entries, first, last);
          return;
        }

        var median = pivot(entries, first, last);

        var left = first;
        var right = last;
        partition(entries, median, ref left, ref right);

        var leftLength = right + 1 - first;
        var rightLength = last + 1 - left;

        //
        // First recurse over shorter partition, then loop
        // on the longer partition to elide tail recursion.
        //
        if (leftLength < rightLength) {
          Sort(entries, first, right);
          first = left;
          length = rightLength;
        }
        else {
          Sort(entries, left, last);
          last = right;
          length = leftLength;
        }
      }
    }

    private T pivot(T[] entries, Int32 first, Int32 last) {
      //
      // An odd sample size is chosen based on the log of the interval size.
      // The median of a randomly chosen set of samples is then returned as
      // an estimate of the true median.
      //
      var length = last + 1 - first;
      var logLen = (Int32)Math.Log10(length);
      var pivotSamples = 2 * logLen + 1;
      var sampleSize = Math.Min(pivotSamples, length);
      var right = first + sampleSize - 1;
      // Sample without replacement
      for (var left = first; left <= right; left++) {
        // Random sampling avoids pathological cases
        var random = Random.Next(left, last + 1);
        Swap(entries, left, random);
      }

      InsertionSort<T>.Sort(entries, first, right);
      //[Test]Console.WriteLine("Samples: " + String.Join(" ", entries.Skip(first).Take(sampleSize)));
      var median = entries[first + sampleSize / 2];
      return median;
    }

    private static void partition(T[] entries, T median, ref Int32 left, ref Int32 right) {
#if CountPart
      PartCount++;
#endif
      var first = left;
      var last = right;
#if Tripartite
      var leftMedian = first;
      var rightMedian = last;
#endif
      while (true) {
        //[Assert]There exists some index >= left where entries[index] >= median
        //[Assert]There exists some index <= right where entries[index] <= median
        // So, there is no need for left or right bound checks
        while (median.CompareTo(entries[left]) > 0) {
          left++;
#if CountCompare
          CompareCount++;
#endif
        }
        while (median.CompareTo(entries[right]) < 0) {
          right--;
#if CountCompare
          CompareCount++;
#endif
        }
#if CountCompare
        CompareCount += 2;
#endif
        //[Assert]entries[right] <= median <= entries[left]
        if (right <= left) break;

        Swap(entries, left, right);
        swapOut(entries, median, left, right, ref leftMedian, ref rightMedian);
        left++;
        right--;
        //[Assert]entries[first:left - 1] <= median <= entries[right + 1:last]
      }

      if (left == right) {
        left++;
        right--;
      }
      //[Assert]right < left
      swapIn(entries, ref left, ref right, leftMedian, rightMedian, first, last);

      //[Assert]entries[first:right] <= median <= entries[left:last]
      //[Assert]entries[right + 1:left - 1] == median when non-empty
      verify(entries, median, left, right, first, last);
    }

    [Conditional("VerifyAssertions")]
    private static void verify(T[] entries, T median, Int32 left, Int32 right,
                               Int32 first, Int32 last) {
      Debug.Assert(right < left, "left <= right");
      var index = first;
      for (; index <= right; index++)
        Debug.Assert(median.CompareTo(entries[index]) >= 0, "median < left entry");
      for (; index < left; index++)
        Debug.Assert(median.CompareTo(entries[index]) == 0, "median != middle entry");
      for (; index <= last; index++)
        Debug.Assert(median.CompareTo(entries[index]) <= 0, "right entry < median");
    }
    #endregion

    #region Swap Methods
    [Conditional("Tripartite")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static void swapOut(T[] entries, T median, Int32 left, Int32 right,
                                ref Int32 leftMedian, ref Int32 rightMedian) {
      if (median.CompareTo(entries[left]) == 0) Swap(entries, leftMedian++, left);
      if (median.CompareTo(entries[right]) == 0) Swap(entries, right, rightMedian--);
#if CountCompare
      CompareCount += 2;
#endif
    }

    [Conditional("Tripartite")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static void swapIn(T[] entries, ref Int32 left, ref Int32 right,
                               Int32 leftMedian, Int32 rightMedian, Int32 first, Int32 last) {
      // Restore median entries
      for (var prefix = first; prefix < leftMedian;)
        Swap(entries, prefix++, right--);
      for (var suffix = last; rightMedian < suffix;)
        Swap(entries, left++, suffix--);
    }

    public static void Swap(T[] entries, Int32 index1, Int32 index2) {
      if (index1 != index2) {
        var entry = entries[index1];
        entries[index1] = entries[index2];
        entries[index2] = entry;
#if CountMove
        MoveCount += 3;
#endif
      }
    }
    #endregion
  }
}
