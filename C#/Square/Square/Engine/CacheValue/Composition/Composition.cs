//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-02-20 CNHume]Added Composition2 Class
//[2012-11-23 CNHume]Created Composition Class
//
// Conditionals:
//
#define CompositionByValue

namespace CacheValue;

using static Engine.Board;
using static Engine.Position;

//
// Type Aliases:
//
using CompositionCounter = UInt16;
using Eval = Int16;
#if CompositionByValue
struct Composition {
#else
class Composition {
#endif
  #region CVFlags Enum
  [Flags]
  public enum CVFlags : byte {
    //None = 0,
    IsValid = 1                         // Bit 0
  }
  #endregion

  #region Constructors
  public Composition(
    CompositionCounter wBlackCounts,
    CompositionCounter wWhiteCounts,
    SideFlags fBlackSide,
    SideFlags fWhiteSide) {
    //HashPiece = qHashPiece;
    BlackCounts = wBlackCounts;
    WhiteCounts = wWhiteCounts;
    BlackFlagsSide = fBlackSide;
    WhiteFlagsSide = fWhiteSide;
    FlagsCV = CVFlags.IsValid;

    BlackValue = weighPieces(BlackCounts, BlackFlagsSide);
    WhiteValue = weighPieces(WhiteCounts, WhiteFlagsSide);
  }
  #endregion

  #region Methods
#if !CompositionByValue
  // Recycle Compositions to reduce garbage:
  public void Recycle(
    CompositionCounter wBlackCounts,
    CompositionCounter wWhiteCounts,
    SideFlags fBlackSide,
    SideFlags fWhiteSide) {
    //HashPiece = qHashPiece;
    BlackCounts = wBlackCounts;
    WhiteCounts = wWhiteCounts;
    FlagsCV = CVFlags.IsValid;
    BlackFlagsSide = fBlackSide;
    WhiteFlagsSide = fWhiteSide;

    BlackValue = weighPieces(BlackCounts, BlackFlagsSide);
    WhiteValue = weighPieces(WhiteCounts, WhiteFlagsSide);
  }
#endif
  #endregion

  #region Fields
  //public Hashcode HashPiece;
  public CompositionCounter BlackCounts;
  public CompositionCounter WhiteCounts;
  public CVFlags FlagsCV;
  public SideFlags BlackFlagsSide;
  public SideFlags WhiteFlagsSide;
  public Eval BlackValue;
  public Eval WhiteValue;
  #endregion

  #region Properties
  public Eval Delta => (Eval)(WhiteValue - BlackValue);
  public Eval Total => (Eval)(WhiteValue + BlackValue);
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
    IsValid = 1                         // Bit 0
  }
  #endregion

  #region Constructors
  public Composition2(CompositionCounter wPieceCounts, SideFlags fside) {
    PieceCounts = wPieceCounts;
    FlagsCV = CVFlags.IsValid;
    FlagsSide = fside;
    Value = weighPieces(wPieceCounts, fside);
  }
  #endregion

  #region Methods
#if !CompositionByValue
  // Recycle Compositions to reduce garbage:
  public void Recycle(CompositionCounter wPieceCounts, SideFlags fside) {
    PieceCounts = wPieceCounts;
    FlagsCV = CVFlags.IsValid;
    FlagsSide = fside;
    Value = weighPieces(wPieceCounts, fside);
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
