//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace HeapSort.Extensions {
  using System;
  using System.Collections.Generic;
  using System.Linq;

  public static class Extension {
    #region IEnumerable Methods
    public static Boolean IsSorted<T>(this IEnumerable<T> en, Boolean isAscending = true) where T : IComparable {
      if (en.Any()) {
        var last = en.First();
        foreach (var next in en.Skip(1)) {
          if (IsPredecessor(next, last, isAscending))
            return false;
          // Equal OK
          last = next;
        }
      }

      return true;
    }

    public static bool IsPredecessor<T>(T x, T y, Boolean isAscending = true) where T : IComparable {
      var sense = x.CompareTo(y);
      return sense < 0 && isAscending ||
             sense > 0 && !isAscending;
    }
    #endregion
  }
}
