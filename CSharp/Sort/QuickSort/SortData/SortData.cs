//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace Sort {
  using System;

  public class SortData {
    #region Properties
    public SortCase SortCase { get; set; }
    #endregion

    #region Constructors
    public SortData(SortCase sortCase) {
      this.SortCase = sortCase;
    }
    #endregion

    #region Methods
    public Int32[] BuildEntries(Int32 length) {
      var dtNow = DateTime.Now;
      Console.WriteLine($"{dtNow:HH:mm:ss.fff} Building {length:n0} {SortCase} Entries");

      switch (SortCase) {
      case SortCase.Ascending:
        return linearEntries(length, ascending: true);

      case SortCase.Descending:
        return linearEntries(length, ascending: false);

      case SortCase.Random:
        var seed = (Int32)dtNow.Ticks;
        return randomEntries(length, seed);

      default:
        return null;
      }
    }

    private static Int32[] linearEntries(Int32 length, Boolean ascending = true) {
      var entries = new Int32[length];
      for (var index = 0; index < length; index++)
        entries[index] = ascending ? index : length - 1 - index;

      return entries;
    }

    private static Int32[] randomEntries(Int32 length, Int32 seed) {
      const Int32 scale = 120;
      var range = length / scale;
      var r = new Random(seed);

      var entries = new Int32[length];
      for (var index = 0; index < length; index++)
        entries[index] = r.Next(range) + 1;

      return entries;
    }
    #endregion
  }
}
