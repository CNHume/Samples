﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// Conditionals:
//
//#define VerifyPartitions

//
// The Tripartite conditional enables Bentley-McIlroy 3-way Partitioning.
// This performs additional compares to isolate islands of keys equal to
// the pivot value.  Use unless key-equivalent classes are of small size.
//
#define Tripartite

namespace Sort {
  using InsertionSort;

  using SortTest;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;

  public class QuickSort<T> where T : IComparable {
    #region Constants
    public const Int32 INSERTION_LIMIT_DEFAULT = 12;
    #endregion

    #region Properties
    public IMeter Meter { get; init; }
    public Int32 InsertionLimit { get; init; }
    private Random Random { get; init; }
    private InsertionSort<T> InsertionSorter { get; init; }

    private T Median { get; set; }

    private Int32 Left { get; set; }
    private Int32 Right { get; set; }
    private Int32 LeftMedian { get; set; }
    private Int32 RightMedian { get; set; }
    #endregion

    #region Constructors
    public QuickSort(IMeter meter, Int32 insertionLimit, Random random) {
      this.Meter = meter;
      this.InsertionLimit = insertionLimit;
      this.Random = random;
      this.InsertionSorter = new InsertionSort<T>(Meter);
    }

    public QuickSort(IMeter meter = default, Int32 insertionLimit = INSERTION_LIMIT_DEFAULT)
      : this(meter, insertionLimit, new Random()) {
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
          InsertionSorter.Sort(entries, first, last);
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

      InsertionSorter.Sort(entries, Left, last);
      //[Test]Console.WriteLine("Samples: " + String.Join(" ", entries.Skip(Left).Take(sampleSize)));
      Median = entries[Left + sampleSize / 2];
    }

    private void partition(T[] entries) {
      Meter?.IncPart();
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
          Meter?.IncCompare();
        }
        while (Median.CompareTo(entries[Right]) < 0) {
          Right--;
          Meter?.IncCompare();
        }
        Meter?.IncCompare(2);
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

    [Conditional("VerifyPartitions")]
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
    public void Swap(T[] entries, Int32 left, Int32 right) {
      Swap(ref entries[left], ref entries[right]);
      Meter?.IncMove(3);
    }

    [Conditional("Tripartite")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void swapOut(T[] entries) {
      if (Median.CompareTo(entries[Left]) == 0) Swap(entries, LeftMedian++, Left);
      if (Median.CompareTo(entries[Right]) == 0) Swap(entries, Right, RightMedian--);
      Meter?.IncCompare(2);
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
