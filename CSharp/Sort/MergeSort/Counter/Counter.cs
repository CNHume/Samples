//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
#define CountCompare
#define CountMove
//#define CountPart

//
// Use ScaleLength to compare performance of a given algorithm at increasing scales.
// Do not use ScaleLength to compare different Sort Algorithms at the same scale.
//
//#define ScaleLength

namespace Sort {
  using System;
  using System.Diagnostics;

  public class Counter {
    #region Properties
    public UInt64 CompareCount { get; set; }
    public UInt64 MoveCount { get; set; }
    public UInt64 PartCount { get; set; }
    public String Mode { get; init; }
    public Stopwatch Timer { get; init; }
    #endregion

    #region Constructors
    public Counter(String mode) {
      this.Mode = mode;
      this.Timer = new Stopwatch();
    }
    #endregion

    #region Methods
    public void Clear() {
      PartCount = MoveCount = CompareCount = 0;
    }

    [Conditional("ShowCounts")]
    public void Display() {
#if CountCompare
      Console.WriteLine("CompareCount = {0:n0}", CompareCount);
#endif
#if CountMove
      Console.WriteLine("MoveCount = {0:n0}", MoveCount);
#endif
#if CountPart
      Console.WriteLine("PartCount = {0:n0}", PartCount);
#endif
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

    public void Header() {
      Console.WriteLine("{0:HH:mm:ss.fff} {1}", DateTime.Now, Mode);
    }

    public void Footer(Int32 length, Boolean isSorted) {
      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)Timer.ElapsedTicks / 10000;
      Console.WriteLine("{0:HH:mm:ss.fff} Finished, Sorted = {1}", DateTime.Now, isSorted);

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
