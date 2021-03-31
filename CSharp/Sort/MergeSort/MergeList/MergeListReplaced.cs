//
// (C) Copyright 2013, Christopher N. Hume.  All rights reserved.
//
// 2014-12-09 CNHume  Created File
//
namespace Sort {
  using System;
  using System.Collections.Generic;

  public static class MergeList<T> where T : IComparable {
    #region Constants
    private const Int32 insertionLimit = 20;
    private const Int32 mergesDefault = 6;
    #endregion

    #region Properties
    public static Int32 InsertionLimit { get; set; }
    public static Int32 Merges { get; set; }
    #endregion

    #region Constructors
    static MergeList() {
      Merges = mergesDefault;
    }
    #endregion

    #region Sort Methods
    public static List<T> Sort<T>(List<T> list) where T : IComparable {
      if (list.Count <= 1)
        return list;

      List<T> left = list.GetRange(0, list.Count / 2);
      List<T> right = list.GetRange(left.Count, list.Count - left.Count);
      return Merge(Sort(left), Sort(right));
    }

    public static List<T> Merge<T>(List<T> left, List<T> right) where T : IComparable {
      List<T> result = new List<T>();
      while (left.Count > 0 && right.Count > 0) {
        if (left[0].CompareTo(right[0]) <= 0) {
          result.Add(left[0]);
          left.RemoveAt(0);
        }
        else {
          result.Add(right[0]);
          right.RemoveAt(0);
        }
      }
      result.AddRange(left);
      result.AddRange(right);
      return result;
    }
    #endregion

    #region Test Methods
    internal static void Swap(T[] entries, Int32 index1, Int32 index2) {
      var entry = entries[index1];
      entries[index1] = entries[index2];
      entries[index2] = entry;
    }

    public static void Reverse(T[] entries) {
      for (Int32 left = 0, right = entries.Length - 1; left < right; left++, right--)
        Swap(entries, left, right);
    }
    #endregion
  }
}
