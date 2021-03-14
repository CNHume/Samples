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
  using System.Diagnostics;
  using System.Text;

  using static System.String;

  public class Counter {
    #region Constants
    private const char space = ' ';
    #endregion

    #region Properties
    protected UInt64 CompareCount { get; set; }
    protected UInt64 MoveCount { get; set; }
    protected UInt64 PartCount { get; set; }
    public String Mode { get; init; }
    public Type SortType { get; init; }
    private Stopwatch Timer { get; init; }
    #endregion

    #region Constructors
    public Counter(String mode = null, Type sortType = null) {
      this.Mode = mode;
      this.SortType = sortType;
      this.Timer = new Stopwatch();
    }
    #endregion

    #region Methods
    public void Clear() {
      PartCount = MoveCount = CompareCount = 0;
    }

    public void Reset() {
      Clear();
      Timer.Reset();
    }

    public void Start() {
      Timer.Start();
    }

    public void Stop() {
      Timer.Stop();
    }

    [Conditional("CountCompare")]
    public void IncCompare(UInt64 count = 1) {
      CompareCount += count;
    }

    [Conditional("CountMove")]
    public void IncMove(UInt64 count = 1) {
      MoveCount += count;
    }

    [Conditional("CountPart")]
    public void IncPart(UInt64 count = 1) {
      PartCount += count;
    }

    [Conditional("ShowCounts")]
    public void Display() {
      if (CompareCount > 0)
        Console.WriteLine($"CompareCount = {CompareCount:n0}");

      if (MoveCount > 0)
        Console.WriteLine($"MoveCount = {MoveCount:n0}");

      if (PartCount > 0)
        Console.WriteLine($"PartCount = {PartCount:n0}");
    }

    public void Header() {
      var sb = new StringBuilder($"{DateTime.Now:HH:mm:ss.fff}");
      if (SortType is not null) sb.Append(space).AppendTypeName(SortType);
      if (!IsNullOrEmpty(Mode)) sb.Append(space).Append(Mode);
      Console.WriteLine(sb.ToString());
    }

    public void Footer(Int32 length, Boolean isSorted) {
      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)Timer.ElapsedTicks / 10000;
      Console.WriteLine($"{DateTime.Now:HH:mm:ss.fff} Finished, Sorted = {isSorted}");

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
    }
    #endregion

    #region Math Methods
    private static double log(Int32 length) {
      return Math.Log10(length);
    }
    #endregion
  }
}
