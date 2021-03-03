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
  using System.Linq;
  using System.Runtime.CompilerServices;

  class QuickSort<T> where T : IComparable {
    #region Constants
    private const Int32 INSERTION_LIMIT_DEFAULT = 12;
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
    private Random Random { get; set; }
    private T Median { get; set; }

    private Int32 Left { get; set; }
    private Int32 Right { get; set; }
    private Int32 LeftMedian { get; set; }
    private Int32 RightMedian { get; set; }
    #endregion

    #region Constructors
    public QuickSort(Int32 insertionLimit, Random random) {
      this.InsertionLimit = insertionLimit;
      this.Random = random;
    }

    public QuickSort(Int32 insertionLimit)
      : this(insertionLimit, new Random()) {
    }

    public QuickSort()
      : this(INSERTION_LIMIT_DEFAULT) {
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

        Left = first;
        Right = last;
        pivot(entries);
        partition(entries);
        //[Note]Right < Left

        var leftLength = Right + 1 - first;
        var rightLength = last + 1 - Left;

        //
        // First recurse over shorter partition, then loop
        // on the longer partition to elide tail recursion.
        //
        if (leftLength < rightLength) {
          Sort(entries, first, Right);
          first = Left;
          length = rightLength;
        }
        else {
          Sort(entries, Left, last);
          last = Right;
          length = leftLength;
        }
      }
    }

    private void pivot(T[] entries) {
      //
      // An odd sample size is chosen based on the log of the interval size.
      // The median of a randomly chosen set of samples is then returned as
      // an estimate of the true median.
      //
      var length = Right + 1 - Left;
      var logLen = (Int32)Math.Log10(length);
      var pivotSamples = 2 * logLen + 1;
      var sampleSize = Math.Min(pivotSamples, length);
      var last = Left + sampleSize - 1;
      // Sample without replacement
      for (var first = Left; first <= last; first++) {
        // Random sampling avoids pathological cases
        var random = Random.Next(first, Right + 1);
        Swap(entries, first, random);
      }

      InsertionSort<T>.Sort(entries, Left, last);
      //[Test]Console.WriteLine("Samples: " + String.Join(" ", entries.Skip(Left).Take(sampleSize)));
      Median = entries[Left + sampleSize / 2];
    }

    private void partition(T[] entries) {
#if CountPart
      PartCount++;
#endif
      var first = Left;
      var last = Right;
#if Tripartite
      LeftMedian = first;
      RightMedian = last;
#endif
      while (true) {
        //[Assert]There exists some index >= Left where entries[index] >= Median
        //[Assert]There exists some index <= Right where entries[index] <= Median
        // So, there is no need for Left or Right bound checks
        while (Median.CompareTo(entries[Left]) > 0) {
          Left++;
#if CountCompare
          CompareCount++;
#endif
        }
        while (Median.CompareTo(entries[Right]) < 0) {
          Right--;
#if CountCompare
          CompareCount++;
#endif
        }
#if CountCompare
        CompareCount += 2;
#endif
        //[Assert]entries[Right] <= Median <= entries[Left]
        if (Right <= Left) break;

        Swap(entries, Left, Right);
        swapOut(entries);
        Left++;
        Right--;
        //[Assert]entries[first:Left - 1] <= Median <= entries[Right + 1:last]
      }

      if (Left == Right) {
        Left++;
        Right--;
      }
      //[Assert]Right < Left
      swapIn(entries, first, last);

      //[Assert]entries[first:Right] <= Median <= entries[Left:last]
      //[Assert]entries[Right + 1:Left - 1] == Median when non-empty
      verify(entries, first, last);
    }

    [Conditional("VerifyAssertions")]
    private void verify(T[] entries, Int32 first, Int32 last) {
      Debug.Assert(Right < Left, "Left <= Right");
      for (; first <= Right; first++)
        Debug.Assert(Median.CompareTo(entries[first]) >= 0, "Median < left entry");
      for (; first < Left; first++)
        Debug.Assert(Median.CompareTo(entries[first]) == 0, "Median != middle entry");
      for (; first <= last; first++)
        Debug.Assert(Median.CompareTo(entries[first]) <= 0, "right entry < Median");
    }
    #endregion

    #region Swap Methods
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
#if CountMove
        MoveCount += 3;
#endif
    }

    [Conditional("Tripartite")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void swapOut(T[] entries) {
      if (Median.CompareTo(entries[Left]) == 0) Swap(entries, LeftMedian++, Left);
      if (Median.CompareTo(entries[Right]) == 0) Swap(entries, Right, RightMedian--);
#if CountCompare
      CompareCount += 2;
#endif
    }

    [Conditional("Tripartite")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void swapIn(T[] entries, Int32 first, Int32 last) {
      // Restore Median entries
      while (first < LeftMedian) Swap(entries, first++, Right--);
      while (RightMedian < last) Swap(entries, Left++, last--);
    }
    #endregion
  }
}
