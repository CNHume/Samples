//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals
//#define DenseEntries

namespace SortTest {
  using SortTest.Extensions;

  using System;

  public class SortData {
    #region Constants
    public const SortCase SORTCASE_DEFAULT = SortCase.Rand;
    #endregion

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
      Console.WriteLine($"{dtNow:HH:mm:ss.fff} Building {length:n0} {SortCase.GetDisplayName()} Entries");

      switch (SortCase) {
      case SortCase.Asc:
        return linearEntries(length, ascending: true);

      case SortCase.Desc:
        return linearEntries(length, ascending: false);

      case SortCase.Rand:
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
      const Int32 scale = 3;
#if DenseEntries
      var range = length / scale;
#else
      var range = length * scale;
#endif
      var r = new Random(seed);

      var entries = new Int32[length];
      for (var index = 0; index < length; index++)
        entries[index] = r.Next(range);

      return entries;
    }
#endregion
  }
}
