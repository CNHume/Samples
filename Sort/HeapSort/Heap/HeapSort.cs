//
// (C) Copyright 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-03-21 CNHume  Implemented Enumerator based Sort()
// 2015-02-24 CNHume  Created class to isolate ReverseSort() and Sort()
//
#define Enumerate

namespace Sort {
  using System;

  class HeapSort<T> : Heap<T> where T : IComparable {
    #region Properties
    //public Boolean IsSorted { get; protected set; }
    #endregion

    #region Constructors
    /// <summary>HeapSort Constructor</summary>
    /// <param name="entries">Entries array</param>
    /// <param name="count"># of entries to use in Heap</param>
    /// <param name="ascending">Initial Heap sense</param>
    public HeapSort(T[] entries, Int32 count, Boolean ascending = true)
      : base(entries, count, ascending) {
    }

    /// <summary>HeapSort Constructor</summary>
    /// <param name="entries">Entries array</param>
    public HeapSort(T[] entries)
      : base(entries) {
    }

    /// <summary>HeapSort Constructor</summary>
    public HeapSort()
      : this((T[])null) {
    }
    #endregion
  }
}
