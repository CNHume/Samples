﻿//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// Conditionals:
//
//#define VerifyPartitions
//#define TestSamples
//#define SampleMiddle
//#define SampleRandomly

//
// The Tripartite conditional enables Bentley-McIlroy 3-way Partitioning.
// This performs additional compares to isolate islands of keys equal to
// the pivot value.  Use unless key-equivalent classes are of small size.
//
#define Tripartite

using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Sorters;

using SortTests;

public class QuickSort<T> where T : IComparable {
  #region Constants
  public const UInt32 INSERTION_LIMIT_DEFAULT = 12;
  private const Int32 SAMPLES_MAX = 19;
  #endregion

  #region Properties
  public IMeter? Meter { get; }
  public UInt32? InsertionLimit { get; }
  private InsertionSort<T> InsertionSorter { get; }
  private Random? Random { get; init; }
  private T[] Samples { get; }

  private Int32 Left { get; set; }
  private Int32 Right { get; set; }
  private Int32 LeftMedian { get; set; }
  private Int32 RightMedian { get; set; }
  #endregion

  #region Constructors
  public QuickSort(UInt32 insertionLimit, Random? random, IMeter? meter) {
    this.Meter = meter;
    this.InsertionLimit = insertionLimit;
    this.InsertionSorter = new InsertionSort<T>(Meter);
    this.Samples = new T[SAMPLES_MAX];
    this.Random = random;
  }
#if SampleRandomly                      // Sample randomly, without replacement:
  public QuickSort(UInt32 insertionLimit = INSERTION_LIMIT_DEFAULT, IMeter? meter = default)
    : this(insertionLimit, new Random(), meter) {
  }
#else
  public QuickSort(UInt32 insertionLimit = INSERTION_LIMIT_DEFAULT, IMeter? meter = default)
    : this(insertionLimit, default, meter) {
  }
#endif
  public QuickSort(IMeter? meter = default)
    : this(INSERTION_LIMIT_DEFAULT, default, meter) {
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
      var median = pivot(entries);
      partition(median, entries);
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

  /// <summary>Return an odd sample size proportional to the log of a large interval size.</summary>
  private static Int32 sampleSize(Int32 length, Int32 max = SAMPLES_MAX) {
    var logLen = (Int32)Math.Log10(length);
    var samples = Math.Min(2 * logLen + 1, max);
    return Math.Min(samples, length);
  }

  /// <summary>Estimate the median value of entries[Left:Right]</summary>
  /// <remarks>A sample median is used as an estimate the true median.</remarks>
  private T pivot(T[] entries) {
    var length = Right + 1 - Left;
#if SampleMiddle
    var middle = length / 2;
    return entries[Left + middle];
#else
    var samples = sampleSize(length);
    for (var sample = 0; sample < samples; sample++) {
      var first = Left + sample;
#if SampleRandomly                      // Sample randomly, without replacement:
      var index = Random.Next(first, Right + 1);
#else                                   // Sample Linearly:
      // Guard against Arithmetic Overflow:
      var index = (Int64)length * sample / samples + Left;
#endif
      Debug.Assert(first <= index, $"index = {index} < first = {first}");
      Debug.Assert(index <= Right, $"index = {index} > Right = {Right}");
      Samples[sample] = entries[index];
    }
    Meter?.IncMove((UInt32)samples);
    InsertionSorter.Sort(Samples, 0, samples - 1);
#if TestSamples
    if (samples > 10)
      Console.WriteLine("Samples: " + String.Join(" ", Samples.Take(samples)));
#endif
    return Samples[samples / 2];
#endif
  }

  private void partition(T median, T[] entries) {
    Meter?.IncPart();
    var first = Left;
    var last = Right;
#if Tripartite
    LeftMedian = first;
    RightMedian = last;
#endif
    while (true) {
      //[Assert]There exists some index >= Left where entries[index] >= median
      //[Assert]There exists some index <= Right where entries[index] <= median
      // So, there is no need for Left or Right bound checks
      while (median.CompareTo(entries[Left]) > 0) {
        Left++;
        Meter?.IncCompare();
      }
      while (median.CompareTo(entries[Right]) < 0) {
        Right--;
        Meter?.IncCompare();
      }
      Meter?.IncCompare(2);
      //[Assert]entries[Right] <= median <= entries[Left]
      if (Right <= Left) break;

      Swap(entries, Left, Right);
      swapOut(median, entries);
      Left++;
      Right--;
      //[Assert]entries[first:Left - 1] <= median <= entries[Right + 1:last]
    }

    if (Left == Right) {
      Left++;
      Right--;
    }
    //[Assert]Right < Left
    swapIn(entries, first, last);

    //[Assert]entries[first:Right] <= median <= entries[Left:last]
    //[Assert]entries[Right + 1:Left - 1] == median when non-empty
    verify(median, entries, first, last);
  }

  [Conditional("VerifyPartitions")]
  private void verify(T median, T[] entries, Int32 first, Int32 last) {
    Debug.Assert(Right < Left, "Left <= Right");
    for (; first <= Right; first++)
      Debug.Assert(median.CompareTo(entries[first]) >= 0, "median < left entry");
    for (; first < Left; first++)
      Debug.Assert(median.CompareTo(entries[first]) == 0, "median != middle entry");
    for (; first <= last; first++)
      Debug.Assert(median.CompareTo(entries[first]) <= 0, "right entry < median");
  }
  #endregion                            // Sort Methods

  #region Swap Methods
  [Conditional("Tripartite")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void swapOut(T median, T[] entries) {
    if (median.CompareTo(entries[Left]) == 0) Swap(entries, LeftMedian++, Left);
    if (median.CompareTo(entries[Right]) == 0) Swap(entries, Right, RightMedian--);
    Meter?.IncCompare(2);
  }

  [Conditional("Tripartite")]
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void swapIn(T[] entries, Int32 first, Int32 last) {
    // Restore Median entries
    while (first < LeftMedian) Swap(entries, first++, Right--);
    while (RightMedian < last) Swap(entries, Left++, last--);
  }

  /// <summary>Swap entries at the left and right indicies.</summary>
  public void Swap(T[] entries, Int32 left, Int32 right) {
    Swap(ref entries[left], ref entries[right]);
    Meter?.IncMove(3);
  }

  /// <summary>Swap two entities of type T.</summary>
  public static void Swap(ref T e1, ref T e2) {
    (e1, e2) = (e2, e1);
  }
  #endregion                            // Swap Methods
}
