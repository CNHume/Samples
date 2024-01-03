//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[CNH 22-Jul-14]Created ProbeCounter Class
//
namespace Cache;

using static Logging.Logger;

class SimpleCounter {
  #region Counter Fields
  public UInt64 GetReads;
  public UInt64 GetHits;
  public UInt64 Added;
  #endregion

  #region Constructors
  public SimpleCounter(String sName) {
    Name = sName;
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
    DisplayGets(lGetReadsOffset, lGetHitsOffset);
    DisplayUsage(uCapacity);
  }

  protected void DisplayGets(Int64 lGetReadsOffset, Int64 lGetHitsOffset) {
    DisplayHits(Name, "Get",
                (Int64)GetReads - lGetReadsOffset,
                (Int64)GetHits - lGetHitsOffset);
  }

  protected void DisplayUsage(UInt32 uCapacity) {
    var dSaturation = 100.0 * Added / uCapacity;

    if (Added == 0)
      LogInfo(LogLevel.data, $"{Name} Added = {Added:n0}; Saturation = {dSaturation:n1}%");
    else {
      var dReplacedPercent = 100.0 * Replaced / Writes;
      LogInfo(LogLevel.data, $"{Name} Added = {Added:n0}; Saturation = {dSaturation:n1}%; Replaced = {dReplacedPercent:n1}%");
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

    LogInfo(LogLevel.data,
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
    DisplayGets(lGetReadsOffset, lGetHitsOffset);
    DisplaySets();
    //[Note]uCapacity > 8M thrashed in 1GB RAM
    DisplayUsage(uCapacity);
  }

  protected void DisplaySets() {
    DisplayHits(Name, "Set", (Int64)SetReads, (Int64)SetHits);
  }
  #endregion
}
