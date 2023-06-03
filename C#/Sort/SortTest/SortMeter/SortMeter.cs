//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
// Use ScaleLength to compare performance of a given algorithm at increasing scales.
// Do not use ScaleLength to compare different Sort Algorithms at the same scale.
//
//#define ScaleLength

using System.Diagnostics;
using System.Text;

using static System.String;

namespace SortTest {
  using Extensions;

  public class SortMeter<T> : IMeter where T : IComparable {
    #region Constants
    private const Char space = ' ';
    private const String commaSpace = ", ";
    #endregion

    #region Properties
    protected UInt64 CompareCount { get; set; } = 0;
    protected UInt64 MoveCount { get; set; } = 0;
    protected UInt64 PartCount { get; set; } = 0;
    protected String? Mode { get; init; }
    private Stopwatch Timer { get; }
    #endregion

    #region Constructors
    public SortMeter() {
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

    protected void Header(IEnumerable<T> entries, Type sortType, Boolean print = false) {
      if (print) {
        Console.WriteLine("input:");
        if (entries != null)
          Console.WriteLine(Join(commaSpace, entries));
      }

      var sb = new StringBuilder($"{DateTime.Now:HH:mm:ss.fff}");
      if (sortType != null) sb.Append(space).AppendTypeName(sortType);
      if (!IsNullOrEmpty(Mode)) sb.Append(space).Append(Mode);
      Console.WriteLine(sb.ToString());
    }

    protected void Footer(IEnumerable<T> entries, Boolean print = false, Boolean isAscending = true) {
      var sorted = entries.IsSorted(isAscending);
      Console.WriteLine($"{DateTime.Now:HH:mm:ss.fff} Finished, Sorted = {sorted}");

      //
      // There are 10,000 ticks per msec
      //
      var msec = (Double)Timer.ElapsedTicks / 10000;
      displayRate(entries.Count(), msec);

      if (print) {
        Console.WriteLine("output:");
        if (entries != null)
          Console.WriteLine(Join(commaSpace, entries));
      }
    }

    private static void displayRate(Int32 nLength, Double msec) {
      var dLength = (Double)nLength;
#if ScaleLength
      dLength *= Math.Log10(dLength);
      var sScaled = $" * log({nLength:n0}) = {dLength:0.0##}";
#else
      var sScaled = String.Empty;
#endif
      var sRate = msec == 0 ?
        String.Empty : $", rate = {dLength / msec:0.0##} KHz";
      Console.WriteLine(
        $"Sorted a total of {nLength:n0}{sScaled} entries in {msec / 1000:0.0##} sec{sRate}");
    }
    #endregion
  }
}
