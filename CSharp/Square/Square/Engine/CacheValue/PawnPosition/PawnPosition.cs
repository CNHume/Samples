//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-09-05 CNHume]Created Class
//
// Conditionals:
//
//#define PawnPositionByValue

namespace Engine.CacheValue {
  using static Position;

  using System;
  using System.Diagnostics;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;
  using FeatureCounter = System.UInt32;
  using Hashcode = System.UInt64;
  using Plane = System.UInt64;

#if PawnPositionByValue
  struct PawnPosition {
#else
  class PawnPosition {
#endif
    #region PRPFlags Enum
    [Flags]
    public enum PRPFlags : byte {
      None = 0,
      Dark = 1,                         // Bit 0 Wrong Bishop Flags
      Lite = Dark << 1,                 // Bit 1
      IsValid = Lite << 1,              // Bit 2
      Both = Lite | Dark,
    }
    #endregion

    #region Constructors
    public PawnPosition(Hashcode qHashPawn, PRPFlags fBlackPRP, PRPFlags fWhitePRP,
                        FeatureCounter uWhiteCounts, FeatureCounter uBlackCounts,
                        Plane qpWhitePassers, Plane qpBlackPassers) {
      HashPawn = qHashPawn;             // To verify Position Equality
      BlackPRP = fBlackPRP | PRPFlags.IsValid;
      WhitePRP = fWhitePRP;
      WhitePassers = qpWhitePassers;
      BlackPassers = qpBlackPassers;
      weighPawnFeatures(out Delta, out Total,
                        uWhiteCounts, uBlackCounts,
                        qpWhitePassers, qpBlackPassers);
    }
    #endregion

    #region Methods
#if !PawnPositionByValue
    // Recycle PawnPositions to reduce garbage:
    public void Recycle(Hashcode qHashPawn, PRPFlags fBlackPRP, PRPFlags fWhitePRP,
                        FeatureCounter uWhiteCounts, FeatureCounter uBlackCounts,
                        Plane qpWhitePassers, Plane qpBlackPassers) {
      HashPawn = qHashPawn;
      BlackPRP = fBlackPRP | PRPFlags.IsValid;
      WhitePRP = fWhitePRP;
      WhitePassers = qpWhitePassers;
      BlackPassers = qpBlackPassers;
      weighPawnFeatures(out Delta, out Total,
                        uWhiteCounts, uBlackCounts,
                        qpWhitePassers, qpBlackPassers);
    }
#endif
    #endregion

    #region Fields
    public Hashcode HashPawn;
    public PRPFlags BlackPRP;
    public PRPFlags WhitePRP;
    public Plane WhitePassers;
    public Plane BlackPassers;
    public Eval Delta;
    public Eval Total;
    #endregion
  }
}
