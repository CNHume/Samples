﻿//
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
                        FeatureCounter uBlackCounts, FeatureCounter uWhiteCounts,
                        Plane qpBlackPassers, Plane qpWhitePassers) {
      HashPawn = qHashPawn;             // To verify Position Equality
      BlackPRP = fBlackPRP | PRPFlags.IsValid;
      WhitePRP = fWhitePRP;
      BlackPassers = qpBlackPassers;
      WhitePassers = qpWhitePassers;
      weighPawnFeatures(out Delta, out Total,
                        uBlackCounts, uWhiteCounts,
                        qpBlackPassers, qpWhitePassers);
    }
    #endregion

    #region Methods
#if !PawnPositionByValue
    // Recycle PawnPositions to reduce garbage:
    public void Recycle(Hashcode qHashPawn, PRPFlags fBlackPRP, PRPFlags fWhitePRP,
                        FeatureCounter uBlackCounts, FeatureCounter uWhiteCounts,
                        Plane qpBlackPassers, Plane qpWhitePassers) {
      HashPawn = qHashPawn;
      BlackPRP = fBlackPRP | PRPFlags.IsValid;
      WhitePRP = fWhitePRP;
      BlackPassers = qpBlackPassers;
      WhitePassers = qpWhitePassers;
      weighPawnFeatures(out Delta, out Total,
                        uBlackCounts, uWhiteCounts,
                        qpBlackPassers, qpWhitePassers);
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
