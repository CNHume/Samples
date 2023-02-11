//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2012-07-04 CNHume]Created File
//
// Conditionals:
//
//#define DebugHashPieces
#define CompositionByValue
//#define PawnPositionByValue
#define InitFree                      //[Default]
//#define InitHelp                      //[Test]
//#define MaterialBalance
//#define TestInsufficient

using System.Text;

namespace Engine {
  using CacheValue;

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
      BoardSide side) {
      side.TestInsufficient();

      CXPMemo.Counts.GetReads++;
      var found = CXPMemo[wMemoHash];

      //[Note]SideFlags.Weight are used to determine a Match, i.e., Get Hit
      var fsideWeight = side.FlagsSide & SideFlags.Weight;
      var fsideFoundWeight = found.FlagsSide & SideFlags.Weight;
#if CompositionByValue
      var bDefault = (found.FlagsCV & Composition2.CVFlags.IsValid) == 0;
#else
      var bDefault = found == default(Composition2);
#endif
      if (!bDefault &&
          found.PieceCounts == wPieceCounts &&
          fsideWeight == fsideFoundWeight) {
        CXPMemo.Counts.GetHits++;       // Match. i.e., Get Hit
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
      found = new Composition2(wPieceCounts, fsideWeight);
      CXPMemo[wMemoHash] = found;
      return found;
#else                                   // CompositionByValue
      if (bDefault) {
        CXPMemo.Counts.Added++;         // Non-Match Case: Add new Composition
        found = new Composition2(wPieceCounts, fsideWeight);
        CXPMemo[wMemoHash] = found;
        return found;
      }
      else {
        found.Recycle(wPieceCounts, fsideWeight);
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
      BoardSide blackSide,
      BoardSide whiteSide) {
      blackSide.TestInsufficient();
      whiteSide.TestInsufficient();

      CXPMemo.Counts.GetReads++;
      var found = CXPMemo[uMemoHash];

      //[Note]SideFlags.Weight are used to determine a Match, i.e., Get Hit
      var fBlackSideWeight = blackSide.FlagsSide & SideFlags.Weight;
      var fWhiteSideWeight = whiteSide.FlagsSide & SideFlags.Weight;
#if CompositionByValue
      var bDefault = (found.FlagsCV & Composition.CVFlags.IsValid) == 0;
#else
      var bDefault = found == default(Composition);
#endif
      if (!bDefault) {
        var fBlackSideFoundWeight = found.BlackFlagsSide & SideFlags.Weight;
        var fWhiteSideFoundWeight = found.WhiteFlagsSide & SideFlags.Weight;
        if (found.BlackCounts == wBlackCounts &&
            found.WhiteCounts == wWhiteCounts &&
            fBlackSideWeight == fBlackSideFoundWeight &&
            fWhiteSideWeight == fWhiteSideFoundWeight) {
          CXPMemo.Counts.GetHits++;     // Match. i.e., Get Hit
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
        sb.AppendPieceCounts(blackSide, whiteSide, blackSide.Counts, whiteSide.Counts);
        sb.AppendLine();
        sb.Append("New ");
        sb.AppendPieceCounts(blackSide, whiteSide, found.BlackCounts, found.WhiteCounts);
        sb.AppendLine();
        sb.AppendFormat($"Index = {uMemoHash}");
        sb.AppendLine();
        sb.FlushLine();
      }
#endif
      found = new Composition(
        wBlackCounts, wWhiteCounts,
        fBlackSideWeight, fWhiteSideWeight);
      CXPMemo[uMemoHash] = found;
      return found;
#else                                   // CompositionByValue
      if (bDefault) {
        CXPMemo.Counts.Added++;         // Non-Match Case: Add new Composition
        found = new Composition(
          wBlackCounts, wWhiteCounts,
          fBlackSideWeight, fWhiteSideWeight);
        CXPMemo[uMemoHash] = found;
        return found;
      }
      else {
        found.Recycle(
          wBlackCounts, wWhiteCounts,
          fBlackSideWeight, fWhiteSideWeight);
        return found;
      }
#endif
    }
#endif                                  // MaterialBalance
    public PawnPosition? GetPXP(Position position) {
      PXPMemo.Counts.GetReads++;
      var qHashPawn = position.HashPawn;
      var found = PXPMemo[qHashPawn];
#if PawnPositionByValue
      var bDefault = (found.BlackPRP & PRPFlags.IsValid) == 0;
#else
      var bDefault = found == default(PawnPosition);
#endif
      if (!bDefault && found?.HashPawn == qHashPawn) {
        PXPMemo.Counts.GetHits++;       // Match. i.e., Get Hit
        return found;
      }

      //
      // Position instance contains the specific Pawn configuration.
      //
      // Passer weight could be optimized out where PawnFeature Delta
      // is much less than the staticDelta returned by staticEval().
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
        found = new PawnPosition(
          qHashPawn, fBlackPRP, fWhitePRP,
          uBlackCount, uWhiteCount,
          qpBlackPassers, qpWhitePassers);
        PXPMemo[qHashPawn] = found;
        return found;
      }
      else {
        found?.Recycle(
          qHashPawn, fBlackPRP, fWhitePRP,
          uBlackCount, uWhiteCount,
          qpBlackPassers, qpWhitePassers);
        return found;
      }
#endif
    }
    #endregion
  }
}
