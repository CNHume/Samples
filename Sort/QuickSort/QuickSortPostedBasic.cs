namespace Sort {
  using System;

  class QuickSort<T> where T : IComparable {
    #region Constants
    private const Int32 insertionLimitDefault = 12;
    #endregion

    #region Properties
    public Int32 InsertionLimit { get; set; }
    protected Random Random { get; set; }
    #endregion

    #region Constructors
    public QuickSort()
      : this(insertionLimitDefault, new Random()) {
    }

    public QuickSort(Int32 insertionLimit, Random random) {
      InsertionLimit = insertionLimit;
      Random = random;
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

    private static void partition(T[] entries, T median, ref Int32 left, ref Int32 right) {
      //var first = left;
      //var last = right;

      do {
        //[Assert]There exists some index >= left where entries[index] >= median
        //[Assert]There exists some index <= right where entries[index] <= median
        // So, there is no need for left or right bound checks
        while (median.CompareTo(entries[left]) > 0) left++;
        while (median.CompareTo(entries[right]) < 0) right--;

        //[Assert]entries[right] <= median <= entries[left]
        if (left <= right) Swap(entries, left++, right--);
        //[Assert]entries[first:left - 1] <= median <= entries[right + 1:last]
      } while (left <= right);

      //[Assert]right < left
      // entries[first:right] <= median <= entries[left:last]
      // entries[right + 1:left - 1] == median when non-empty
    }

    private T pivot(T[] entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      var logLen = (Int32)Math.Log10(length);
      var pivotSamples = 2 * logLen + 1;
      var sampleSize = Math.Min(pivotSamples, length);
      var right = first + sampleSize - 1;
      for (var left = first; left <= right; left++) {
        // Random sampling avoids pathological cases
        var random = Random.Next(left, last + 1);
        // Sample without replacement
        Swap(entries, left, random);
      }

      InsertionSort<T>.Sort(entries, first, right);
      var median = entries[first + sampleSize / 2];
      return median;
    }

    public static void Swap(T[] entries, Int32 index1, Int32 index2) {
      if (index1 != index2) {
        var entry = entries[index1];
        entries[index1] = entries[index2];
        entries[index2] = entry;
      }
    }
    #endregion
  }

  #region Insertion Sort
  static class InsertionSort<T> where T : IComparable {
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
  }
  #endregion
}
