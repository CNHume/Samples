﻿//
// (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
//
// 2018-03-14 CNHume  Implemented Binary Search
//
namespace Searches;

public static partial class Extensions {
  /// <summary>Use Binary Search to find index of GLB for value</summary>
  /// <typeparam name="T">type of entries and value</typeparam>
  /// <param name="entries">array of entries</param>
  /// <param name="value">search value</param>
  /// <remarks>entries must be in ascending order</remarks>
  /// <returns>index into entries of GLB for value</returns>
  public static int BinarySearchForGLB<T>(this T[] entries, T value)
    where T : IComparable {
    return entries.BinarySearchForGLB(value, 0, entries.Length - 1);
  }

  /// <summary>Use Binary Search to find index of GLB for value</summary>
  /// <typeparam name="T">type of entries and value</typeparam>
  /// <param name="entries">array of entries</param>
  /// <param name="value">search value</param>
  /// <param name="left">leftmost index to search</param>
  /// <param name="right">rightmost index to search</param>
  /// <remarks>entries must be in ascending order</remarks>
  /// <returns>index into entries of GLB for value</returns>
  public static int BinarySearchForGLB<T>(this T[] entries, T value, int left, int right)
    where T : IComparable {
    while (left <= right) {
      var middle = left + (right - left) / 2;
      if (entries[middle].CompareTo(value) < 0)
        left = middle + 1;
      else
        right = middle - 1;
    }

    //[Assert]left == right + 1
    // GLB: entries[right] < value && value <= entries[right + 1]
    return right;
  }

  /// <summary>Use Binary Search to find index of LUB for value</summary>
  /// <typeparam name="T">type of entries and value</typeparam>
  /// <param name="entries">array of entries</param>
  /// <param name="value">search value</param>
  /// <remarks>entries must be in ascending order</remarks>
  /// <returns>index into entries of LUB for value</returns>
  public static int BinarySearchForLUB<T>(this T[] entries, T value)
    where T : IComparable {
    return entries.BinarySearchForLUB(value, 0, entries.Length - 1);
  }

  /// <summary>Use Binary Search to find index of LUB for value</summary>
  /// <typeparam name="T">type of entries and value</typeparam>
  /// <param name="entries">array of entries</param>
  /// <param name="value">search value</param>
  /// <param name="left">leftmost index to search</param>
  /// <param name="right">rightmost index to search</param>
  /// <remarks>entries must be in ascending order</remarks>
  /// <returns>index into entries of LUB for value</returns>
  public static int BinarySearchForLUB<T>(this T[] entries, T value, int left, int right)
    where T : IComparable {
    while (left <= right) {
      var middle = left + (right - left) / 2;
      if (entries[middle].CompareTo(value) <= 0)
        left = middle + 1;
      else
        right = middle - 1;
    }

    //[Assert]left == right + 1
    // LUB: entries[left - 1] <= value && value < entries[left]
    return left;
  }
}
