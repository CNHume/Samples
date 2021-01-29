//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[CNH 22-Jul-14]Created ProbeCounter Class
//
namespace Cache {
  using static Logging.Logger;

  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  class SimpleCounter {
    #region Counter Fields
    public UInt64 GetReads;
    public UInt64 GetHits;
    public UInt64 Added;
    #endregion

    #region Constructors
    public SimpleCounter(String sName) {
      this.Name = sName;
    }
    #endregion

    #region Properties
    public String Name { get; protected set; }
    public virtual UInt64 Writes {
      get {
        return GetReads - GetHits;
      }
    }

    public UInt64 Replaced {
      get {
        return Writes - Added;
      }
    }
    #endregion

    #region Methods
    public virtual void Clear() {
      GetHits = GetReads = Added = 0UL;
    }

    public virtual void Display(UInt32 uCapacity, Int64 lGetReadsOffset = 0L, Int64 lGetHitsOffset = 0L) {
      displayGets(lGetReadsOffset, lGetHitsOffset);
      displayUsage(uCapacity);
    }

    protected void displayGets(Int64 lGetReadsOffset, Int64 lGetHitsOffset) {
      var lGetReads = (Int64)GetReads - lGetReadsOffset;
      var lGetHits = (Int64)GetHits - lGetHitsOffset;

      if (lGetReads == 0)
        LogInfo(Level.data, "{0} Get Hits = {1:n0}; {0} Get Reads = {2:n0}",
                       Name, lGetHits, lGetReads);
      else {
        var dGetHitsPercent = 100.0 * lGetHits / lGetReads;
        LogInfo(Level.data,
                       "{0} Get Hits = {1:n0}; {0} Get Reads = {2:n0}; {0} Get Hits/Reads = {3:n1}%",
                       Name, lGetHits, lGetReads, dGetHitsPercent);
      }
    }

    protected void displayUsage(UInt32 uCapacity) {
      var dSaturation = 100.0 * Added / uCapacity;

      if (Added == 0)
        LogInfo(Level.data, "{0} Added = {1:n0}; Saturation = {2:n1}%",
                       Name, Added, dSaturation);
      else {
        var dReplacedPercent = 100.0 * Replaced / Writes;
        LogInfo(Level.data, "{0} Added = {1:n0}; Saturation = {2:n1}%; Replaced = {3:n1}%",
                       Name, Added, dSaturation, dReplacedPercent);
      }
    }
    #endregion
  }

  class ProbeCounter : SimpleCounter {
    #region Counter Fields
    public UInt64 SetReads;
    public UInt64 SetHits;
    #endregion

    #region Constructors
    public ProbeCounter(String sName)
      : base(sName) {
    }
    #endregion

    #region Properties
    public override UInt64 Writes {
      get {
        return SetReads - SetHits;
      }
    }
    #endregion

    #region Methods
    public override void Clear() {
      base.Clear();
      SetHits = SetReads = 0UL;
    }

    public override void Display(UInt32 uCapacity, Int64 lGetReadsOffset = 0L, Int64 lGetHitsOffset = 0L) {
      displayGets(lGetReadsOffset, lGetHitsOffset);
      displaySets();
      //[Note]uCapacity > 8M thrashed in 1GB RAM
      displayUsage(uCapacity);
    }

    protected void displaySets() {
      if (SetReads == 0)
        LogInfo(Level.data, "{0} Set Hits = {1:n0}; {0} Set Reads = {2:n0}",
                       Name, SetHits, SetReads);
      else {
        var dSetHitsPercent = 100.0 * SetHits / SetReads;
        LogInfo(Level.data,
                       "{0} Set Hits = {1:n0}; {0} Set Reads = {2:n0}; {0} Set Hits/Reads = {3:n1}%",
                       Name, SetHits, SetReads, dSetHitsPercent);
      }
    }
    #endregion
  }
}
