//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2010-10-31 CNHume]Created Class
//
// Conditionals:
//
//#define DebugPeak

namespace Resource {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;

  using static Logging.Logger;

  class Pool<T> where T : class, new() {
    #region Constants
    const Int32 nDefaultAllocation = 12;
    #endregion

    #region Constructors
    public Pool(String? Name = default) {
      this.Name = Name ?? typeof(T).Name;
      Inactive = new Stack<T>();
      Clear();
    }
    #endregion

    #region Methods
    public void Clear() {
      ActivePeak = ActiveCount = 0U;
    }

    private void Allocate(Int32 nAllocations) {
      for (var n = 0; n < nAllocations; n++)
        Inactive.Push(new T());         // Lazy<T> would defer invocation of initBoard()
    }

    public T Push() {
      if (Inactive == null)
        throw new ApplicationException("No Inactive Pool");

      if (Inactive.Count == 0)
        Allocate(nDefaultAllocation);

      var top = Inactive.Pop();
      Debug.Assert(top != null, "Empty Inactive Pool");
      IncActive();
      return top;
    }

    public void Pop(ref T top) {
      if (top == null)
        throw new ArgumentNullException(nameof(Pop));

      Inactive.Push(top);
      DecActive();
      top = DefaultElement;
    }

    private void IncActive() {
      if (ActivePeak < ++ActiveCount) {
        ActivePeak = ActiveCount;
#if DebugPeak
        DisplayActive();
#endif
      }
    }

    private void DecActive() {
      ActiveCount--;
    }

    public void DisplayActive() {
      LogInfo(Level.data, $"{Name} Count = {ActiveCount}, Peak = {ActivePeak}");
    }
    #endregion

    #region Static Fields
    public static readonly T DefaultElement = new T();

    private Stack<T> Inactive;
    public UInt32 ActiveCount;
    public UInt32 ActivePeak;
    public String Name;
    #endregion
  }
}
