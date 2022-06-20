//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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
  using CompositionCounter = UInt16;
  using Eval = Int16;
  using Hashcode = UInt64;
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
    public Composition(CompositionCounter wBlackCounts,
                       CompositionCounter wWhiteCounts,
                       SideFlags fBlackSide, SideFlags fWhiteSide) {
      //HashPiece = qHashPiece;
      WhiteCounts = wWhiteCounts;
      BlackCounts = wBlackCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsBlackHi = fBlackSide;
      FlagsWhiteHi = fWhiteSide;
      weighPieces(out Delta, out Total, wBlackCounts, wWhiteCounts, fBlackSide, fWhiteSide);
    }
    #endregion

    #region Methods
#if !CompositionByValue
    // Recycle Compositions to reduce garbage:
    public void Recycle(CompositionCounter wBlackCounts,
                        CompositionCounter wWhiteCounts,
                        SideFlags fBlackSide, SideFlags fWhiteSide) {
      //HashPiece = qHashPiece;
      BlackCounts = wBlackCounts;
      WhiteCounts = wWhiteCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsBlackHi = fBlackSide;
      FlagsWhiteHi = fWhiteSide;
      weighPieces(out Delta, out Total, wBlackCounts, wWhiteCounts, fBlackSide, fWhiteSide);
    }
#endif
    #endregion

    #region Fields
    //public Hashcode HashPiece;
    public CompositionCounter WhiteCounts;
    public CompositionCounter BlackCounts;
    public CVFlags FlagsCV;
    public SideFlags FlagsBlackHi;
    public SideFlags FlagsWhiteHi;
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
    public Composition2(CompositionCounter wPieceCounts, SideFlags fside) {
      PieceCounts = wPieceCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsSide = fside;
      weighPieces(out Value, wPieceCounts, fside);
    }
    #endregion

    #region Methods
#if !CompositionByValue
    // Recycle Compositions to reduce garbage:
    public void Recycle(CompositionCounter wPieceCounts, SideFlags fside) {
      PieceCounts = wPieceCounts;
      FlagsCV = CVFlags.IsValid;
      FlagsSide = fside;
      weighPieces(out Value, wPieceCounts, fside);
    }
#endif
    #endregion

    #region Fields
    //public Hashcode HashPiece;
    public CompositionCounter PieceCounts;
    public CVFlags FlagsCV;
    public SideFlags FlagsSide;
    public Eval Value;
    #endregion
  }
}
