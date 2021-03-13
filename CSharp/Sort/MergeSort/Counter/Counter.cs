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
      if (!IsNullOrEmpty(Mode)) sb.Append(space).Append(Mode);
      if (SortType is not null) sb.Append(space).AppendTypeName(SortType);
      Console.WriteLine(sb.ToString());
    }

    public void Footer(Int32 length, Boolean isSorted) {
      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)Timer.ElapsedTicks / 10000;
      Console.WriteLine($"{DateTime.Now:HH:mm:ss.fff} Finished, Sorted = {isSorted}");

      if (msec == 0)
        Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec",
                          length, msec / 1000);
      else {
#if ScaleLength
        //
        // On a Dell XPS 9530 [i7-4702HQ @ 2.2 GHz w 16 GB ram] in Release Mode:
        // For Random Fill with scale = 120
        // C# sorted 12 M entries in 36 sec, n * Log(n) Rate = ~5.5 MHz
        //
        var dNLogN = length * log(length);
        var rate = dNLogN / msec;
        Console.WriteLine("Sorted a total of {0:n0} * log(n) = {1:0.0##} entries in {2:0.0##} sec, Rate = {3:0.0##} KHz",
                          length, dNLogN, msec / 1000, rate);
#else
        var rate = length / msec;
        Console.WriteLine("Sorted a total of {0:n0} entries in {1:0.0##} sec, Rate = {2:0.0##} KHz",
                          length, msec / 1000, rate);
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
