//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// Conditionals
//#define DenseEntries

namespace SortTests;

using SortTests.Extensions;

public class SortData {
  #region Constants
  public const Int32 LENGTH_MAX = (1 << 11) - 1 << 20;
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
    if (length >= LENGTH_MAX)
      throw new ArgumentOutOfRangeException(nameof(length), $"LENGTH_MAX = {LENGTH_MAX} <= {length}");

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
      throw new ArgumentException($"Unexpected SortCase: {SortCase}", nameof(SortCase));
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
