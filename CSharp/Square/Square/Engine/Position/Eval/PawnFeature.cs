//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-10-29 CNHume]Added file
//
// Conditionals:
//
//#define TestInitFree
//#define TestInitHelp
#define InitFree                        //[Default]
//#define InitHelp                        //[Test]
//#define TestPawnFeatures

namespace Engine {
  using static Logging.Logger;
  using static CacheValue.PawnPosition;

  using System;
  using System.Diagnostics;
  using System.Text;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;
  using FeatureCounter = System.UInt32;
  using Plane = System.UInt64;

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
#if TestInitFree || TestInitHelp
    static sq[] testSquares = { sq.a1, sq.a8, sq.c2, sq.c5, sq.d6, sq.e4, sq.f1, sq.g7, sq.h8 };
#endif
    #endregion

    #region Pawn Feature Help & Free
#if !InitHelp
    //
    // Return the Pawn Stop square in front of each square;
    // and all help squares prior to that:
    //
    protected static Plane whiteHelp(Int32 n) {
      var qpHelp = BlackFree[n] << nFiles * 2;
#if TestInvalidPositions
      qpHelp |= BIT0 << nFiles + n % nFiles;
#endif
      return qpHelp;
    }

    protected static Plane blackHelp(Int32 n) {
      var qpHelp = WhiteFree[n] >> nFiles * 2;
#if TestInvalidPositions
      qpHelp |= BIT0 << nFiles * (nRanks - 2) + n % nFiles;
#endif
      return qpHelp;
    }
#endif
    private static void newOutsideSquare() {
      WhiteKingToMoveLoss = new Plane[nSquares];
      BlackKingToMoveLoss = new Plane[nSquares];
      WhitePawnToMoveWins = new Plane[nSquares];
      BlackPawnToMoveWins = new Plane[nSquares];
    }

    //
    // White/BlackKingToMoveLoss[] and White/BlackPawnToMoveWins[] 
    // return a Plane for each King Position which, when intersected
    // with Pawns for the side to move, return any Pawns that cannot
    // be caught by the King before they queen.
    //
    protected static void loadOutsideSquare() {
      //
      // Pawns cannot appear on the first or last ranks;
      // and start on the second or second-to-last rank:
      //
      var qpBlack = BIT0 << nFiles;
      var qpWhite = BITHI >> nFiles;

      for (var nPawnY = 1; nPawnY < nRanks - 1; nPawnY++) {
        // Size of the Square depends on how close the Black Pawn is to queening
        var nBase1 = nPawnY > 1 ? nPawnY : nPawnY + 1;
        var nSize1 = nRanks - nBase1;

        for (var nPawnX = 0; nPawnX < nFiles; nPawnX++, qpBlack <<= 1, qpWhite >>= 1) {
          for (var nBlackKingY = 0; nBlackKingY < nRanks; nBlackKingY++) {
            var nWhiteKingY = nRanks - 1 - nBlackKingY;

            for (var nBlackKingX = 0; nBlackKingX < nFiles; nBlackKingX++) {
              var nWhiteKingX = nFiles - 1 - nBlackKingX;
              // Perform King to Move 48 * 64 = 3,072 times

              var nBlack = sqr(nBlackKingX, nBlackKingY);
              var nWhite = sqr(nWhiteKingX, nWhiteKingY);

              //[Debug]
              var sqBlack = (sq)nBlack;
              var sqWhite = (sq)nWhite;

              //
              // bTake accounts for the KingToMove case when a King on the first rank
              // can capture a Pawn on the second rank before it can move two squares.
              //
              var nDeltaX = Math.Abs(nPawnX - nWhiteKingX);
              var bTake = nDeltaX < 2;
              var nBase2 = bTake || nPawnY > 1 ? nPawnY : nPawnY + 1;
              var nSize2 = nRanks - nBase2;

              var bOutsideKingToMove = nSize2 < nDeltaX || nSize2 < nWhiteKingY;
              var bOutsidePawnToMove = nSize1 < nDeltaX + 1 || nSize1 < nWhiteKingY + 1;

              //
              // Sum Pawn positions where the King is outside the Square of the Pawn:
              //
              if (bOutsideKingToMove) {
                WhiteKingToMoveLoss[nWhite] |= qpBlack;
                BlackKingToMoveLoss[nBlack] |= qpWhite;
              }

              if (bOutsidePawnToMove) {
                WhitePawnToMoveWins[nBlack] |= qpWhite;
                BlackPawnToMoveWins[nWhite] |= qpBlack;
              }
            }                           // nKingX
          }                             // nKingY
        }                               // nPawnX
      }                                 // nPawnY
    }
#if TestInitHelp || InitFree || !InitHelp
    private static void newFree() {
      WhiteFree = new Plane[nSquares];
      BlackFree = new Plane[nSquares];
    }

    //
    // Mark squares that remain in front of each square,
    // as potential Pawn Advancements:
    //
    protected static void loadFree() {
      //
      // Advance File masks forward by one Rank:
      //
      var qpWhite = qpFileA << nFiles;
      var qpBlack = qpFileH >> nFiles;

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);

        //
        // Advance masks to the right one File at a time
        // until they rotate back to their leftmost File,
        // whereupon they will have advanced by one Rank:
        //
        for (var x = 0; x < nFiles; x++, qpWhite <<= 1, qpBlack >>= 1) {
          var xInverse = invertFile(x);
          WhiteFree[sqr(x, y)] = qpWhite;
          BlackFree[sqr(xInverse, yInverse)] = qpBlack;
        }
      }
    }
#endif
#if !InitFree
    //
    // Return squares that remain in front of each square,
    // as potential Pawn Advancements:
    //
    protected static Plane whiteFree(Int32 n) {
      var qpFree = BlackHelp[n] << nFiles * 2;
#if TestInvalidPositions
      if (n < nFiles)
        qpFree |= BIT0 << nFiles + n;
#endif
      return qpFree;
    }

    protected static Plane blackFree(Int32 n) {
      var qpFree = WhiteHelp[n] >> nFiles * 2;
#if TestInvalidPositions
      if (n >= nFiles * (nRanks - 1))
        qpFree |= BIT0 << nRankLast + n;
#endif
      return qpFree;
    }
#endif
#if TestInitFree || InitHelp || !InitFree
    private static void newHelp() {
      WhiteHelp = new Plane[nSquares];
      BlackHelp = new Plane[nSquares];
    }

    //
    // Mark the Pawn Stop square in front of each square;
    // and all help squares prior to that:
    //
    protected static void loadHelp() {
      var qpWhite = 0UL;
      var qpBlack = 0UL;

      for (var y = 0; y < nRanks; y++) {
        var yInverse = yInverse(y);
        qpWhite |= BIT0 << nFiles;
        qpBlack |= BIT0 << nRankLast - 1;

        for (var x = 0; x < nFiles; x++, qpWhite <<= 1, qpBlack >>= 1) {
          var xInverse = invertFile(x);
          WhiteHelp[sqr(x, y)] = qpWhite;
          BlackHelp[sqr(xInverse, yInverse)] = qpBlack;
        }
      }
    }
#endif
    private void countPawn(
      Boolean bWhiteCount, Int32 nFile, Int32 nPawn, Plane qpFound, Plane qpFoePawn, Plane qpFoePawnAtx, Plane qpFriendPawnAtx,
      ref PRPFlags fprp, ref Plane qpPassers, ref Plane qpAwkward, ref UInt32 uPassers, ref UInt32 uAwkward) {
#if InitFree
      var qpFreeSquares = bWhiteCount ? WhiteFree[nPawn] : BlackFree[nPawn];
#else
      var qpFreeSquares = bWhiteCount ? whiteFree(nPawn) : blackFree(nPawn);
#endif
#if InitHelp
      var qpHelpSquares = bWhiteCount ? WhiteHelp[nPawn] : BlackHelp[nPawn];
#else
      var qpHelpSquares = bWhiteCount ? whiteHelp(nPawn) : blackHelp(nPawn);
#endif
      if ((qpFreeSquares & qpFoePawn) == 0 &&
          (qpFreeSquares & qpFoePawnAtx) == 0) {
        uPassers++;
        qpPassers |= qpFound;

        //
        // Identify which Bishops would protect the queening square of a Passed Rook Pawn:
        //
        if (nFile == 0)
          fprp |= bWhiteCount ? PRPFlags.Lite : PRPFlags.Dark;
        else if (nFile == nFiles - 1)
          fprp |= bWhiteCount ? PRPFlags.Dark : PRPFlags.Lite;
      }

      //
      // Awkward Pawn has no Helper, nor possibility of a Helper,
      // and its next square is stopped or guarded by a Foe Pawn.
      //
      if ((qpHelpSquares & qpFriendPawnAtx) == 0) {
        var side = getSide(bWhiteCount);
        var nStop = nPawn + side.Rank;
        var qpStop = BIT0 << nStop;

        if ((qpStop & qpFoePawn) != 0 ||
            (qpStop & qpFoePawnAtx) != 0) {
          uAwkward++;
          qpAwkward |= qpFound;
        }
      }
    }

    private UInt32 countFile(
      Boolean bWhiteCount, Int32 nFile, Plane qpFile, Plane qpFilePawn, Plane qpFoePawn, Plane qpFoePawnAtx, Plane qpFriendPawnAtx,
      ref PRPFlags fprp, ref Plane qpPassers, ref Plane qpIsolani, ref Plane qpDoubled, ref Plane qpAwkward,
      ref UInt32 uPassers, ref UInt32 uIsolani, ref UInt32 uDoubled, ref UInt32 uAwkward) {
      var uFilePawns = 0U;
      for (var qpPawn = qpFilePawn; qpPawn != 0; uFilePawns++) {
        var nPawn = RemoveLo(ref qpPawn, out Plane qpFound);
        countPawn(
          bWhiteCount, nFile, nPawn, qpFound, qpFoePawn, qpFoePawnAtx, qpFriendPawnAtx,
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
    // Pawns are not Free iff Free[sq] & (FoePawnAtx | FoePawn).
    // Pawns are Backward when their Stop Squares are blocked, i.e.,
    // when their Stop Square is either attacked or occupied by a Foe,
    // unless Help[sq] & FriendPawnAtx indicates that they can be helped.
    //
    public FeatureCounter CountPawnFeatures(Boolean bWhiteCount, out Plane qpPassers, out PRPFlags fprp) {
      fprp = PRPFlags.None;//[Init]
      var side = getSide(bWhiteCount);
      var foe = getSide(!bWhiteCount);
      var qpFriendPawnAtx = side.PawnA1H8Atx | side.PawnA8H1Atx;
      var qpFoePawnAtx = foe.PawnA1H8Atx | foe.PawnA8H1Atx;
      var qpFriend = side.Piece;
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
      var vOccupied = (Byte)0;
      var qpIsolani = 0UL;
      var qpDoubled = 0UL;
      var qpAwkward = 0UL;

      while (qpFriendPawn != 0) {
        var nFound = FindLo(qpFriendPawn);
        var nFile = nFound % nFiles;
        Debug.Assert((vOccupied & (Byte)(BIT0 << nFile)) == 0, "File Previously Visited", "File = {0}", nFile);
        vOccupied |= (Byte)(BIT0 << nFile);
        var qpFile = File[nFile];
        var qpFilePawn = qpFile & qpFriendPawn;
        qpFriendPawn &= ~qpFilePawn;    // Visiting each occupied file once

        UInt32 uFilePawns = countFile(
          bWhiteCount, nFile, qpFile, qpFilePawn, qpFoePawn, qpFoePawnAtx, qpFriendPawnAtx,
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
        bWhiteCount, uFriendPawns, uPassers, uDivides, uIsolani, uDoubled, uAwkward,
        qpPawns, qpPassers, vOccupied, qpIsolani, qpDoubled, qpAwkward);
    }

    private FeatureCounter featureCount(
      Boolean bWhiteCount, UInt32 uFriendPawns, UInt32 uPassers, UInt32 uDivides, UInt32 uIsolani, UInt32 uDoubled, UInt32 uAwkward,
      Plane qpPawns, Plane qpPassers, byte vOccupied, Plane qpIsolani, Plane qpDoubled, Plane qpAwkward) {
      var uFeatureCounts = (FeatureCounter)0;
      uFeatureCounts += uFriendPawns << vPawns * nPerNibble;
      uFeatureCounts += uPassers << vPassers * nPerNibble;
      uFeatureCounts += uDivides << vDivides * nPerNibble;
      uFeatureCounts += uIsolani << vIsolani * nPerNibble;
      uFeatureCounts += uDoubled << vDoubled * nPerNibble;
      uFeatureCounts += uAwkward << vAwkward * nPerNibble;
#if TestPawnFeatures
      var nOffset = bWhiteCount ? 0 : PawnFeatures.Length;

      FeatureRect[vPawns + nOffset] = qpPawns;
      FeatureRect[vPassers + nOffset] = qpPassers;
      FeatureRect[vDivides + nOffset] = vOccupied;
      FeatureRect[vIsolani + nOffset] = qpIsolani;
      FeatureRect[vDoubled + nOffset] = qpDoubled;
      FeatureRect[vAwkward + nOffset] = qpAwkward;

      var sColor = bWhiteCount ? "White" : "Black";
      var uCount = uFeatureCounts;
      for (var n = 0; n < PawnFeatures.Length; n++, uCount >>= nPerNibble) {
        var nFeature = (Byte)uCount & vNibble;
        LogLine("{0} {1} = {2}", sColor, (PawnFeature)n, nFeature);
        LogLine();
        writeRect(FeatureRect[n + nOffset]);
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
        var y1 = n / nFiles - 1;
        nValue += y1 * mPassedPawnPushWeight;
      }

      return (Eval)nValue;
    }

    private static Eval weighBlackPassers(Plane qpPassers) {
      var nValue = 0;
      while (qpPassers != 0) {
        var n = RemoveLo(ref qpPassers);
        var y = n / nFiles;
        // Black Pawns advance over 5 ranks, from the 7th down to the 2nd, then promote:
        var yInverse1 = nRanks - (y + 2);
        nValue += yInverse1 * mPassedPawnPushWeight;
      }

      return (Eval)nValue;
    }

    internal static void weighPawnFeatures(
      out Eval Delta,
      out Eval Total,
      FeatureCounter uWhiteCounts,
      FeatureCounter uBlackCounts,
      Plane qpWhitePassers,
      Plane qpBlackPassers) {
      var nValueDelta = 0;
      var nValueTotal = 0;

      for (var nFeature = 0; nFeature < PawnFeatures.Length; nFeature++,
           uWhiteCounts >>= nPerNibble,
           uBlackCounts >>= nPerNibble) {
        var nWhite = (Byte)uWhiteCounts & vNibble;
        var nBlack = (Byte)uBlackCounts & vNibble;

        var nDelta = nWhite - nBlack;
        var nTotal = nWhite + nBlack;

        nValueDelta += nDelta * PawnFeatureWeight[nFeature];
        if (nFeature == vPawns)
          nValueTotal += nTotal * PawnFeatureWeight[nFeature];
      }

      var mWhitePasserWeight = weighWhitePassers(qpWhitePassers);
      var mBlackPasserWeight = weighBlackPassers(qpBlackPassers);
      nValueDelta += mWhitePasserWeight - mBlackPasserWeight;

      Delta = (Eval)nValueDelta;
      Total = (Eval)nValueTotal;
    }
    #endregion
  }
}
