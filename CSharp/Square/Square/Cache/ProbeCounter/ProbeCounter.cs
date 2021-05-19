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
      DisplayHits(Name, "Get",
                  (Int64)GetReads - lGetReadsOffset,
                  (Int64)GetHits - lGetHitsOffset);
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

    #region Static Methods
    public static void DisplayHits(String sName, String sAction, Int64 lReads, Int64 lHits) {
      var sRate = String.Empty;
      if (lReads != 0) {
        var dPercent = 100.0 * lHits / lReads;
        sRate = $"; {sName} {sAction} Hits/Read = {dPercent:n1}%";
      }

      LogInfo(Level.data,
              $"{sName} {sAction} Hits = {lHits:n0}; {sName} {sAction} Reads = {lReads:n0}{sRate}");
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
      DisplayHits(Name, "Set", (Int64)SetReads, (Int64)SetHits);
    }
    #endregion
  }
}
