//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2014-02-20 CNHume]Added Composition2 Class
//[2012-11-23 CNHume]Created Composition Class
//
// Conditionals:
//
#define CompositionByValue

namespace Engine.CacheValue {
  using static Board;
  using static Position;

  using System;
  using System.Diagnostics;

  //
  // Type Aliases:
  //
  using CompositionCounter = System.UInt16;
  using Eval = System.Int16;
  using Hashcode = System.UInt64;
#if CompositionByValue
  struct Composition {
#else
  class Composition {
#endif
    #region CVFlags Enum
    [Flags]
    public enum CVFlags : byte {
      //None = 0,
      IsValid = 1                               // Bit 0
    }
    #endregion

    #region Constructors
    public Composition(CompositionCounter wWhiteCounts,
                       CompositionCounter wBlackCounts,
                       HiFlags fBlackHi, HiFlags fWhiteHi) {
      //HashPiece = qHashPiece;
      WhiteCounts = wWhiteCounts;
      BlackCounts = wBlackCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsBlackHi = fBlackHi;
      FlagsWhiteHi = fWhiteHi;
      weighPieces(out Delta, out Total, wWhiteCounts, wBlackCounts, fBlackHi, fWhiteHi);
    }
    #endregion

    #region Methods
#if !CompositionByValue
    // Recycle Compositions to reduce garbage:
    public void Recycle(CompositionCounter wWhiteCounts,
                        CompositionCounter wBlackCounts,
                        HiFlags fBlackHi, HiFlags fWhiteHi) {
      //HashPiece = qHashPiece;
      Side[White].Counts = wWhiteCounts;
      Side[Black].Counts = wBlackCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsHi = fhi;
      weighPieces(out Delta, out Total, wWhiteCounts, wBlackCounts, fBlackHi, fWhiteHi);
    }
#endif
    #endregion

    #region Fields
    //public Hashcode HashPiece;
    public CompositionCounter WhiteCounts;
    public CompositionCounter BlackCounts;
    public CVFlags FlagsCV;
    public HiFlags FlagsBlackHi;
    public HiFlags FlagsWhiteHi;
    public Eval Delta;
    public Eval Total;
    #endregion
  }

#if CompositionByValue
  struct Composition2 {
#else
  class Composition2 {
#endif
    #region CVFlags Enum
    [Flags]
    public enum CVFlags : byte {
      //None = 0,
      IsValid = 1                               // Bit 0
    }
    #endregion

    #region Constructors
    public Composition2(CompositionCounter wPieceCounts,
                        HiFlags fhi) {
      PieceCounts = wPieceCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsHi = fhi;
      weighPieces(out Value, wPieceCounts, fhi);
    }
    #endregion

    #region Methods
#if !CompositionByValue
    // Recycle Compositions to reduce garbage:
    public void Recycle(CompositionCounter wPieceCounts,
                        HiFlags fhi) {
      PieceCounts = wPieceCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsBlackHi = fhi;
      weighPieces(out Value, wPieceCounts, fhi);
    }
#endif
    #endregion

    #region Fields
    //public Hashcode HashPiece;
    public CompositionCounter PieceCounts;
    public CVFlags FlagsCV;
    public HiFlags FlagsHi;
    public Eval Value;
    #endregion
  }
}
