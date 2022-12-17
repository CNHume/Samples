//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-10-29 CNHume]Added file
//
// Conditionals:
//
//#define TestPawnFeatures

namespace Engine {
  using System;
  using System.Diagnostics;

  using static CacheValue.PawnPosition;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Eval = Int16;
  using FeatureCounter = UInt32;
  using Plane = UInt64;

  partial class Position : Board {
    #region Constants
    protected static Plane[] File =
      { qpFileA, qpFileB, qpFileC, qpFileD, qpFileE, qpFileF, qpFileG, qpFileH };

    public enum PawnFeature : byte { Pawns, Passers, Divides, Isolani, Doubled, Awkward };
    protected static Eval[] PawnFeatureWeight =
    { mPawnWeight, mQuarterWeight, -mTenthWeight, -mTenthWeight, -mQuarterWeight, -mFifthWeight };

    const Byte vPawns = (Byte)PawnFeature.Pawns;
    const Byte vPassers = (Byte)PawnFeature.Passers;
    const Byte vDivides = (Byte)PawnFeature.Divides;
    const Byte vIsolani = (Byte)PawnFeature.Isolani;
    const Byte vDoubled = (Byte)PawnFeature.Doubled;
    const Byte vAwkward = (Byte)PawnFeature.Awkward;
    #endregion

    #region Pawn Feature Methods
    private void countPawn(
      Int32 nSide, Int32 nFile, Int32 nPawn, Plane qpFound, Plane qpFoePawn, Plane qpFoePawnAtx, Plane qpFriendPawnAtx,
      ref PRPFlags fprp, ref Plane qpPassers, ref Plane qpAwkward, ref UInt32 uPassers, ref UInt32 uAwkward) {
      var (qpFree, qpHelp) = getFreeHelp(nSide, nPawn);

      if ((qpFree & qpFoePawn) == 0 &&
          (qpFree & qpFoePawnAtx) == 0) {
        uPassers++;
        qpPassers |= qpFound;

        //
        // Identify which Bishops would protect the queening square of a Passed Rook Pawn:
        //
        if (nFile == 0)
          fprp |= nSide == White ? PRPFlags.Lite : PRPFlags.Dark;
        else if (nFile == nFiles - 1)
          fprp |= nSide == White ? PRPFlags.Dark : PRPFlags.Lite;
      }

      //
      // Awkward Pawn has no Helper, nor possibility of a Helper,
      // and its next square is stopped or guarded by a Foe Pawn.
      //
      if ((qpHelp & qpFriendPawnAtx) == 0) {
        var nStop = nPawn + Parameter[nSide].PawnStep;
        var qpStop = bit(nStop);

        if ((qpStop & qpFoePawn) != 0 ||
            (qpStop & qpFoePawnAtx) != 0) {
          uAwkward++;
          qpAwkward |= qpFound;
        }
      }
    }

    private UInt32 countFile(
      Int32 nSide, Int32 nFile, Plane qpFile, Plane qpFilePawn, Plane qpFoePawn, Plane qpFoePawnAtx, Plane qpFriendPawnAtx,
      ref PRPFlags fprp, ref Plane qpPassers, ref Plane qpIsolani, ref Plane qpDoubled, ref Plane qpAwkward,
      ref UInt32 uPassers, ref UInt32 uIsolani, ref UInt32 uDoubled, ref UInt32 uAwkward) {
      var uFilePawns = 0U;
      for (var qpPawn = qpFilePawn; qpPawn != 0; uFilePawns++) {
        var nPawn = RemoveLo(ref qpPawn, out Plane qpFound);
        countPawn(
          nSide, nFile, nPawn, qpFound, qpFoePawn, qpFoePawnAtx, qpFriendPawnAtx,
          ref fprp, ref qpPassers, ref qpAwkward, ref uPassers, ref uAwkward);
      }

      if ((qpFile & qpFriendPawnAtx) == 0) {
        uIsolani += uFilePawns;
        qpIsolani |= qpFilePawn;
      }

      if (uFilePawns > 1) {
        uDoubled += uFilePawns - 1;     // Avoid double counting the first Doubled Pawn
        qpDoubled |= qpFilePawn;
      }

      return uFilePawns;
    }

    //
    // Isolated Files have no intersection with FriendPawnAtx.
    // Pawns are not Free iff Free[n] & (FoePawnAtx | FoePawn).
    // Pawns are Backward when their Stop Squares are blocked, i.e.,
    // when their Stop Square is either attacked or occupied by a Foe,
    // unless Help[n] & FriendPawnAtx indicates that they can be helped.
    //
    public FeatureCounter CountPawnFeatures(Int32 nSide, out Plane qpPassers, out PRPFlags fprp) {
      fprp = default;                   //[Init]
      bool bWhiteAttacker = nSide == White;
      var (friend, foe) = GetSides(bWhiteAttacker);
      var qpFriendPawnAtx = friend.PawnA1H8Atx | friend.PawnA8H1Atx;
      var qpFoePawnAtx = foe.PawnA1H8Atx | foe.PawnA8H1Atx;
      var qpFriend = friend.Piece;
      var qpFoe = foe.Piece;
      var qpFriendPawn = Pawn & qpFriend;
      var qpFoePawn = Pawn & qpFoe;
#if TestPawnFeatures
      Display();
#endif
      //
      // Outputs:
      //
      var uFriendPawns = 0U;
      var uPassers = 0U;
      var uIslands = 0U;
      var uIsolani = 0U;
      var uDoubled = 0U;
      var uAwkward = 0U;

      var qpPawns = qpFriendPawn;
      qpPassers = 0UL;
      Byte vOccupied = 0;
      var qpIsolani = 0UL;
      var qpDoubled = 0UL;
      var qpAwkward = 0UL;

      while (qpFriendPawn != 0) {
        var nFound = TZC64(qpFriendPawn);
        var nFile = x(nFound);
        Debug.Assert((vOccupied & (Byte)bit(nFile)) == 0, "File Previously Visited", $"File = {nFile}");
        vOccupied |= (Byte)bit(nFile);
        var qpFile = File[nFile];
        var qpFilePawn = qpFile & qpFriendPawn;
        qpFriendPawn &= ~qpFilePawn;    // Visit each occupied file only once

        UInt32 uFilePawns = countFile(
          nSide, nFile, qpFile, qpFilePawn, qpFoePawn, qpFoePawnAtx, qpFriendPawnAtx,
          ref fprp, ref qpPassers, ref qpIsolani, ref qpDoubled, ref qpAwkward,
          ref uPassers, ref uIsolani, ref uDoubled, ref uAwkward);
        uFriendPawns += uFilePawns;
      }

      Int32? nPrevFile = default;
      var vFile = vOccupied;
      while (vFile != 0) {
        var nFile = RemoveLo(ref vFile);
        if (!nPrevFile.HasValue || nFile > nPrevFile + 1)
          uIslands++;
        nPrevFile = nFile;
      }

      var uDivides = uIslands > 0 ? uIslands - 1 : uIslands;

      return featureCount(
        nSide, uFriendPawns, uPassers, uDivides, uIsolani, uDoubled, uAwkward,
        qpPawns, qpPassers, vOccupied, qpIsolani, qpDoubled, qpAwkward);
    }

    private FeatureCounter featureCount(
      Int32 nSide, UInt32 uFriendPawns, UInt32 uPassers, UInt32 uDivides, UInt32 uIsolani, UInt32 uDoubled, UInt32 uAwkward,
      Plane qpPawns, Plane qpPassers, byte vOccupied, Plane qpIsolani, Plane qpDoubled, Plane qpAwkward) {
      FeatureCounter uFeatureCounts = 0;
      uFeatureCounts += uFriendPawns << vPawns * nPerNibble;
      uFeatureCounts += uPassers << vPassers * nPerNibble;
      uFeatureCounts += uDivides << vDivides * nPerNibble;
      uFeatureCounts += uIsolani << vIsolani * nPerNibble;
      uFeatureCounts += uDoubled << vDoubled * nPerNibble;
      uFeatureCounts += uAwkward << vAwkward * nPerNibble;
#if TestPawnFeatures
      var nOffset = nSide == White ? 0 : PawnFeatures.Length;

      FeatureOrth[vPawns + nOffset] = qpPawns;
      FeatureOrth[vPassers + nOffset] = qpPassers;
      FeatureOrth[vDivides + nOffset] = vOccupied;
      FeatureOrth[vIsolani + nOffset] = qpIsolani;
      FeatureOrth[vDoubled + nOffset] = qpDoubled;
      FeatureOrth[vAwkward + nOffset] = qpAwkward;

      var uCounter = uFeatureCounts;
      var sideName = Side[nSide].Parameter.SideName;
      for (var n = 0; n < PawnFeatures.Length; n++, uCounter >>= nPerNibble) {
        var uCount = nibble(uCounter);
        LogLine($"{sideName} {(PawnFeature)n} = {uCount}");
        LogLine();
        writeOrth(FeatureOrth[n + nOffset]);
        LogLine();
      }
#endif
      return uFeatureCounts;
    }
    #endregion

    #region Pawn Evaluation
    private static Eval weighWhitePassers(Plane qpPassers) {
      var nValue = 0;
      while (qpPassers != 0) {
        var n = RemoveLo(ref qpPassers);
        // White Pawns advance over 5 ranks, from the 2nd up to the 7th, then promote:
        nValue += (y(n) - 1) * mPassedPawnPushWeight;
      }

      return (Eval)nValue;
    }

    private static Eval weighBlackPassers(Plane qpPassers) {
      var nValue = 0;
      while (qpPassers != 0) {
        var n = RemoveLo(ref qpPassers);
        // Black Pawns advance over 5 ranks, from the 7th down to the 2nd, then promote:
        nValue += (InvertRank(y(n)) - 1) * mPassedPawnPushWeight;
      }

      return (Eval)nValue;
    }

    internal static (Eval delta, Eval total) weighPawnFeatures(
      FeatureCounter uBlackCounts,
      FeatureCounter uWhiteCounts,
      Plane qpBlackPassers,
      Plane qpWhitePassers) {
      var nValueDelta = 0;
      var nValueTotal = 0;

      for (var nFeature = 0; nFeature < PawnFeatures.Length; nFeature++,
           uBlackCounts >>= nPerNibble,
           uWhiteCounts >>= nPerNibble) {
        var nBlack = (Int32)Nibble(uBlackCounts);
        var nWhite = (Int32)Nibble(uWhiteCounts);

        var nDelta = nWhite - nBlack;
        var nTotal = nWhite + nBlack;

        nValueDelta += nDelta * PawnFeatureWeight[nFeature];
        if (nFeature == vPawns)
          nValueTotal += nTotal * PawnFeatureWeight[nFeature];
      }

      var mBlackPasserWeight = weighBlackPassers(qpBlackPassers);
      var mWhitePasserWeight = weighWhitePassers(qpWhitePassers);
      nValueDelta += mWhitePasserWeight - mBlackPasserWeight;

      return ((Eval)nValueDelta, (Eval)nValueTotal);
    }
    #endregion
  }
}
