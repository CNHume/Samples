namespace Sort {
  using System;

  class QuickSortBasic<T> where T : IComparable {
    #region Constants
    private const Int32 INSERTION_LIMIT_DEFAULT = 12;
    #endregion

    #region Properties
    public Int32 InsertionLimit { get; set; }
    private Random Random { get; set; }
    private InsertionSort<T> InsertionSorter { get; init; }

    private T Median { get; set; }
    private Int32 Left { get; set; }
    private Int32 Right { get; set; }
    #endregion

    #region Constructors
    public QuickSortBasic(Int32 insertionLimit, Random random) {
      this.InsertionLimit = insertionLimit;
      this.Random = random;
      this.InsertionSorter = new InsertionSort<T>();
    }

    public QuickSortBasic(Int32 insertionLimit)
      : this(insertionLimit, new Random()) {
    }

    public QuickSortBasic()
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
      Median = entries[Left + sampleSize / 2];
    }

    private void partition(T[] entries) {
      //var first = Left;
      //var last = Right;
      while (true) {
        //[Assert]There exists some index >= Left where entries[index] >= Median
        //[Assert]There exists some index <= Right where entries[index] <= Median
        // So, there is no need for Left or Right bound checks
        while (Median.CompareTo(entries[Left]) > 0) Left++;
        while (Median.CompareTo(entries[Right]) < 0) Right--;

        //[Assert]entries[Right] <= Median <= entries[Left]
        if (Right <= Left) break;

        Swap(entries, Left, Right);
        Left++;
        Right--;
        //[Assert]entries[first:Left - 1] <= Median <= entries[Right + 1:last]
      }

      if (Left == Right) {
        Left++;
        Right--;
      }
      //[Assert]Right < Left
      //[Assert]entries[first:Right] <= Median <= entries[Left:last]
      //[Assert]entries[Right + 1:Left - 1] == Median when non-empty
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
    public static T[] Swap(T[] entries, Int32 left, Int32 right) {
      Swap(ref entries[left], ref entries[right]);
      return entries;
    }
    #endregion
  }
}
