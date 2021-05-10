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
        LogInfo(Level.data, $"{Name} Get Hits = {lGetHits:n0}; {Name} Get Reads = {lGetReads:n0}");
      else {
        var dGetHitsPercent = 100.0 * lGetHits / lGetReads;
        LogInfo(Level.data,
                $"{Name} Get Hits = {lGetHits:n0}; {Name} Get Reads = {lGetReads:n0}; {Name} Get Hits/Reads = {dGetHitsPercent:n1}%");
      }
    }

    protected void displayUsage(UInt32 uCapacity) {
      var dSaturation = 100.0 * Added / uCapacity;

      if (Added == 0)
        LogInfo(Level.data, $"{Name} Added = {Added:n0}; Saturation = {dSaturation:n1}%");
      else {
        var dReplacedPercent = 100.0 * Replaced / Writes;
        LogInfo(Level.data, $"{Name} Added = {Added:n0}; Saturation = {dSaturation:n1}%; Replaced = {dReplacedPercent:n1}%");
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
        LogInfo(Level.data, $"{Name} Set Hits = {SetHits:n0}; {Name} Set Reads = {SetReads:n0}");
      else {
        var dSetHitsPercent = 100.0 * SetHits / SetReads;
        LogInfo(Level.data,
                $"{Name} Set Hits = {SetHits:n0}; {Name} Set Reads = {SetReads:n0}; {Name} Set Hits/Reads = {dSetHitsPercent:n1}%");
      }
    }
    #endregion
  }
}
