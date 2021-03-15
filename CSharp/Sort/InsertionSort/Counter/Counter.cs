//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
// Use ScaleLength to compare performance of a given algorithm at increasing scales.
// Do not use ScaleLength to compare different Sort Algorithms at the same scale.
//
//#define ScaleLength

namespace Sort {
  using Extension;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;
  using System.Text;

  using static System.String;

  public class Counter<T> : ICounter where T : IComparable {
    #region Constants
    private const char space = ' ';

    private const String commaSpace = ", ";
    #endregion

    #region Properties
    protected UInt64 CompareCount { get; set; }
    protected UInt64 MoveCount { get; set; }
    protected UInt64 PartCount { get; set; }
    public String Mode { get; init; }
    private Stopwatch Timer { get; init; }
    #endregion

    #region Constructors
    public Counter(String mode = null, Type sortType = null) {
      this.Mode = mode;
      this.Timer = new Stopwatch();
    }
    #endregion

    #region Interface Methods

    public void IncCompare(UInt64 count = 1) {
      CompareCount += count;
    }

    public void IncMove(UInt64 count = 1) {
      MoveCount += count;
    }

    public void IncPart(UInt64 count = 1) {
      PartCount += count;
    }
    #endregion

    #region Methods
    public void Clear() {
      PartCount = MoveCount = CompareCount = 0;
    }

    [Conditional("ShowCounts")]
    protected void Display() {
      if (CompareCount > 0)
        Console.WriteLine($"CompareCount = {CompareCount:n0}");

      if (MoveCount > 0)
        Console.WriteLine($"MoveCount = {MoveCount:n0}");

      if (PartCount > 0)
        Console.WriteLine($"PartCount = {PartCount:n0}");
    }

    protected void Reset() {
      Clear();
      Timer.Reset();
    }

    protected void Start() {
      Timer.Start();
    }

    protected void Stop() {
      Timer.Stop();
    }

    protected void Header(IEnumerable<T> entries, Boolean print = false, Type sortType = null) {
      if (print) {
        Console.WriteLine("input:");
        if (entries is not null)
          Console.WriteLine(Join(commaSpace, entries));
      }

      var sb = new StringBuilder($"{DateTime.Now:HH:mm:ss.fff}");
      if (sortType is not null) sb.Append(space).AppendTypeName(sortType);
      if (!IsNullOrEmpty(Mode)) sb.Append(space).Append(Mode);
      Console.WriteLine(sb.ToString());
    }

    protected void Footer(IEnumerable<T> entries, Boolean print = false, Boolean ascending = true) {
      var length = entries.Count();
      var sorted = IsSorted(entries, ascending);

      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)Timer.ElapsedTicks / 10000;
      Console.WriteLine($"{DateTime.Now:HH:mm:ss.fff} Finished, Sorted = {sorted}");

      if (msec == 0)
        Console.WriteLine($"Sorted a total of {length:n0} entries in {msec / 1000:0.0##} sec");
      else {
#if ScaleLength
        var dNLogN = length * log(length);
        var rate = dNLogN / msec;
        Console.WriteLine(
          $"Sorted a total of {length:n0} * log(n) = {dNLogN:0.0##} entries in {msec / 1000:0.0##} sec, Rate = {rate:0.0##} KHz");
#else
        var rate = length / msec;
        Console.WriteLine(
          $"Sorted a total of {length:n0} entries in {msec / 1000:0.0##} sec, Rate = {rate:0.0##} KHz");
#endif
      }
      if (print) {
        Console.WriteLine("output:");
        if (entries is not null)
          Console.WriteLine(Join(commaSpace, entries));
      }
    }

    public static Boolean IsSorted(IEnumerable<T> en, Boolean ascending = true) {
      if (en.Any()) {
        var last = en.First();
        foreach (var next in en.Skip(1)) {
          var sense = next.CompareTo(last);
          if (sense < 0 && ascending ||
              sense > 0 && !ascending)
            return false;

          last = next;
        }
      }

      return true;
    }
    #endregion

    #region Math Methods
    private static double log(Int32 length) {
      return Math.Log10(length);
    }
    #endregion
  }
}
