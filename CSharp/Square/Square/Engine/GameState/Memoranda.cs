﻿//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-07-04 CNHume]Created File
//
// Conditionals:
//
#define CompositionByValue
//#define PawnPositionByValue
#define InitFree                      //[Default]
//#define InitHelp                      //[Test]
//#define DebugHashPieces
//#define MaterialBalance

namespace Engine {
  using CacheValue;

  using System;
  using System.Text;

  using static Board;
  using static CacheValue.PawnPosition;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using CompositionCounter = UInt16;
  using Eval = Int16;
  using Hashcode = UInt64;
  using MemoHashcode = UInt32;
  using PieceHashcode = UInt16;         // 10 bits
  using Plane = UInt64;

  partial class GameState {
    #region Evaluation Methods
    //
    // The MaterialBalance Conditional makes a simplifying assumption that
    // material value can be assessed independently for the two sides.
    //
    // However, research has shown that material evaluation depends on which
    // pieces the are possessed by the two sides.  There are conquest cycles:
    // A Bishop Pair is more effective against an opposing Rook, for example.
    //
    // See "The Evaluation of Material Imbalances" by IM Larry Kaufman
    // https://www.chess.com/article/view/the-evaluation-of-material-imbalances-by-im-larry-kaufman
    //
#if MaterialBalance
    public Composition2 GetCX2(
      Position position,
      PieceHashcode wMemoHash,
      CompositionCounter wPieceCounts,
      SideFlags fside) {
      CXPMemo.Counts.GetReads++;
      var found = CXPMemo[wMemoHash];
      fside &= SideFlags.Pair;          //[Note]Only SideFlags.Pair are cached for use by weighPieces()
      var fsideFound = found.FlagsSide & SideFlags.Pair;
#if CompositionByValue
      var bDefault = (found.FlagsCV & Composition2.CVFlags.IsValid) == 0;
#else
      var bDefault = found == default(PawnPosition);
#endif
      if (!bDefault &&
          found.PieceCounts == wPieceCounts &&
          fside == fsideFound) {
        CXPMemo.Counts.GetHits++;       // Match, a.k.a. Get Hit
        return found;
      }

#if CompositionByValue
      if (bDefault) {
        CXPMemo.Counts.Added++;         // Non-Match Case: Add new Composition
      }
#if DebugHashPieces
      else {
        var sb = new StringBuilder();
        //DisplayCurrent("GetCX2()");
        sb.AppendLine();
        sb.Append("Old ");
        sb.AppendPieceCounts(White, wPieceCounts);
        sb.AppendLine();
        sb.Append("New ");
        sb.AppendPieceCounts(White, found.PieceCounts);
        sb.AppendLine();
        sb.AppendFormat($"Index = {CXPMemo.index(wMemoHash)}");
        sb.AppendLine();
        sb.FlushLine();
      }
#endif
      found = new Composition2(wPieceCounts, fside);
      CXPMemo[wMemoHash] = found;
      return found;
#else                                   // CompositionByValue
      if (bDefault) {
        CXPMemo.Counts.Added++;         // Non-Match Case: Add new Composition
        found = new Composition2(wPieceCounts, fBlackSide, fWhiteSide);
        CXPMemo[wMemoHash] = found;
        return found;
      }
      else {
        found.Recycle(wPieceCounts, fBlackSide, fWhiteSide);
        return found;
      }
#endif
    }
#else                                   // MaterialBalance
    public Composition GetCXP(
      Position position,
      MemoHashcode uMemoHash,
      CompositionCounter wBlackCounts,
      CompositionCounter wWhiteCounts,
      SideFlags fBlackSide,
      SideFlags fWhiteSide) {
      CXPMemo.Counts.GetReads++;
      var found = CXPMemo[uMemoHash];
      fBlackSide &= SideFlags.Pair;     //[Note]Only SideFlags.Pair are cached for use by weighPieces()
      fWhiteSide &= SideFlags.Pair;
#if CompositionByValue
      var bDefault = (found.FlagsCV & Composition.CVFlags.IsValid) == 0;
#else
      var bDefault = found == default(Composition);
#endif
      if (!bDefault) {
        var fBlackSideFound = found.BlackFlagsSide & SideFlags.Pair;
        var fWhiteSideFound = found.WhiteFlagsSide & SideFlags.Pair;
        if (found.BlackCounts == wBlackCounts &&
            found.WhiteCounts == wWhiteCounts &&
            fBlackSide == fBlackSideFound &&
            fWhiteSide == fWhiteSideFound) {
          CXPMemo.Counts.GetHits++;     // Match, a.k.a. Get Hit
          return found;
        }
      }
#if CompositionByValue
      if (bDefault) {
        CXPMemo.Counts.Added++;         // Non-Match Case: Add new Composition
      }
#if DebugHashPieces
      else {
        var sb = new StringBuilder();
        //DisplayCurrent("GetCXP()");
        sb.AppendLine();
        sb.Append("Old ");
        sb.AppendPieceCounts(Side[Black], Side[White]);
        sb.AppendLine();
        sb.Append("New ");
        sb.AppendPieceCounts(found.Side[Black], found.Side[White]);
        sb.AppendLine();
        sb.AppendFormat($"Index = {memo.index(uMemoHash)}");
        sb.AppendLine();
        sb.FlushLine();
      }
#endif
      found = new Composition(wBlackCounts, wWhiteCounts, fBlackSide, fWhiteSide);
      CXPMemo[uMemoHash] = found;
      return found;
#else                                   // CompositionByValue
      if (bDefault) {
        CXPMemo.Counts.Added++;         // Non-Match Case: Add new Composition
        found = new Composition(wBlackCounts, wWhiteCounts, fBlackSide, fWhiteSide);
        CXPMemo[uMemoHash] = found;
        return found;
      }
      else {
        found.Recycle(wBlackCounts, wWhiteCounts, fBlackSide, fWhiteSide);
        return found;
      }
#endif
    }
#endif                                  // MaterialBalance
    public PawnPosition GetPXP(Position position) {
      PXPMemo.Counts.GetReads++;
      var qHashPawn = position.HashPawn;
      var found = PXPMemo[qHashPawn];
#if PawnPositionByValue
      var bDefault = (found.BlackPRP & PRPFlags.IsValid) == 0;
#else
      var bDefault = found == default(PawnPosition);
#endif
      if (!bDefault && found.HashPawn == qHashPawn) {
        PXPMemo.Counts.GetHits++;       // Match, a.k.a. Get Hit
        return found;
      }

      //
      // Position instance contains the specific Pawn configuration.
      //
      // Passer weight could be optimized out where PawnFeature Delta
      // is much less than the StaticDelta returned by staticEval().
      //
      // Wrong Bishops will be determined by Passed Rook Pawns.
      //
      var uBlackCount = position.CountPawnFeatures(Black, out Plane qpBlackPassers, out PRPFlags fBlackPRP);
      var uWhiteCount = position.CountPawnFeatures(White, out Plane qpWhitePassers, out PRPFlags fWhitePRP);
#if PawnPositionByValue
      if (bDefault)
        PXPMemo.Counts.Added++;         // Non-Match Case: Add new PawnPosition

      found = new PawnPosition(qHashPawn, fBlackPRP, fWhitePRP,
                               uBlackCount, uWhiteCount,
                               qpBlackPassers, qpWhitePassers);
      PXPMemo[qHashPawn] = found;
      return found;
#else                                   // PawnPositionByValue
      if (bDefault) {
        PXPMemo.Counts.Added++;         // Non-Match Case: Add new PawnPosition
        found = new PawnPosition(qHashPawn, fBlackPRP, fWhitePRP,
                                 uBlackCount, uWhiteCount,
                                 qpBlackPassers, qpWhitePassers);
        PXPMemo[qHashPawn] = found;
        return found;
      }
      else {
        found.Recycle(qHashPawn, fBlackPRP, fWhitePRP,
                      uBlackCount, uWhiteCount,
                      qpBlackPassers, qpWhitePassers);
        return found;
      }
#endif
    }
    #endregion
  }
}
