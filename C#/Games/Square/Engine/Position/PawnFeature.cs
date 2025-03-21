﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2021-04-21 CNHume]Created File
//
// Conditionals:
//
//#define TestPawnFeatures
#define InitFree                        //[Default]
//#define InitHelp                        //[Test]
//#define TestInitFree
//#define TestInitHelp
//#define TestInvalidPawnPositions

namespace Engine;

using static Logging.Logger;

//
// Type Aliases:
//
using Plane = UInt64;

partial class Position {
  #region King Outside Square Load Methods
  //
  // KingToMoveLoss[] and PawnToMoveWins[] return Planes for each King Position which
  // when intersected with Pawns for the side to move return any Pawns that cannot be
  // caught by the King - before they queen.
  //
  private static void loadOutsideSquare() {
    var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

    //
    // Pawns cannot appear on the first or last ranks;
    // and start on the second or second-to-last rank:
    //
    var n = sqr(0, whiteParameter.PawnRank);
    var qpWhite = bit(n);
    var qpBlack = bit(InvertSquare(n));

    for (var nPawnY = 1; nPawnY < nRanks - 1; nPawnY++) {
      // Size of the Games depends on how close the Black Pawn is to queening
      var nBase1 = nPawnY > 1 ? nPawnY : nPawnY + 1;  // Beyond vs On 2nd Rank
      var nSize1 = nRanks - nBase1;

      for (var nPawnX = 0; nPawnX < nFiles; nPawnX++, qpWhite <<= 1, qpBlack >>= 1) {
        for (Int32 nWhite = 0, nWhiteKingY = 0; nWhiteKingY < nRanks; nWhiteKingY++) {
          //var nBlackKingY = InvertRank(nWhiteKingY);

          for (var nWhiteKingX = 0; nWhiteKingX < nFiles; nWhiteKingX++, nWhite++) {
            var nBlack = InvertSquare(nWhite);
            var nBlackKingX = InvertFile(nWhiteKingX);
            // Calculate King to Move 48 * 64 = 3,072 times

            //[Note]Calculate lateral distance with Pawn and King of opposite colors:
            var nDeltaX = Math.Abs(nPawnX - nBlackKingX);

            //
            // In the KingToMove case: bTake prevents a Pawn from moving two squares
            // when a King on the first rank can capture the Pawn on the second rank.
            //
            var bTake = nDeltaX < 2;
            var nBase2 = bTake || nPawnY > 1 ? nPawnY : nPawnY + 1;
            var nSize2 = nRanks - nBase2;

            var bOutsideKingToMove = nSize2 < nDeltaX || nSize2 < nWhiteKingY;
            var bOutsidePawnToMove = nSize1 < nDeltaX + 1 || nSize1 < nWhiteKingY + 1;

            //[Debug]King Positions
            //var sqBlack = (Sq)nBlack;
            //var sqWhite = (Sq)nWhite;

            //
            // Sum Pawn positions with King outside the Games of the Pawn:
            //
            if (bOutsideKingToMove) {
              blackParameter.KingToMoveLoss[nBlack] |= qpWhite;
              whiteParameter.KingToMoveLoss[nWhite] |= qpBlack;
            }

            if (bOutsidePawnToMove) {
              blackParameter.PawnToMoveWins[nWhite] |= qpBlack;
              whiteParameter.PawnToMoveWins[nBlack] |= qpWhite;
            }
          }                             // nKingX
        }                               // nKingY
      }                                 // nPawnX
    }                                   // nPawnY
  }
  #endregion                            // King Outside Games Load Methods

  #region Free & Help Load Methods
#if TestInitHelp || InitFree || !InitHelp
  //
  // Mark squares that remain in front of each square,
  // as potential Pawn Advancements:
  //
  private static void loadFree() {
    var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

    //
    // Advance File masks forward by one Rank:
    //
    var qpWhite = qpFileA << nFiles;
    var qpBlack = qpFileH >> nFiles;

    for (Int32 nWhite = 0, y = 0; y < nRanks; y++) {
      //
      // Advance masks to the right one File at a time
      // until they rotate back to their leftmost File,
      // whereupon they will have advanced by one Rank:
      //
      for (var x = 0; x < nFiles; x++, nWhite++, qpWhite <<= 1, qpBlack >>= 1) {
        var nBlack = InvertSquare(nWhite);
        blackParameter.Free[nBlack] = qpBlack;
        whiteParameter.Free[nWhite] = qpWhite;
      }
    }
  }
#endif
#if TestInitFree || InitHelp || !InitFree
  //
  // Mark the Pawn Stop square in front of each square;
  // and all help squares prior to that:
  //
  private static void loadHelp() {
    var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

    var n = sqr(0, whiteParameter.PawnRank);
    var qpWhite = 0UL;
    var qpBlack = 0UL;

    for (Int32 nWhite = 0, y = 0; y < nRanks; y++) {
      qpWhite |= bit(n);
      qpBlack |= bit(InvertSquare(n));

      for (var x = 0; x < nFiles; x++, nWhite++, qpWhite <<= 1, qpBlack >>= 1) {
        var nBlack = InvertSquare(nWhite);
        blackParameter.Help[nBlack] = qpBlack;
        whiteParameter.Help[nWhite] = qpWhite;
      }
    }
  }
#endif
  private static void loadFreeHelp() {
#if TestInitHelp || InitFree || !InitHelp
    loadFree();
#endif
#if TestInitFree || InitHelp || !InitFree
    loadHelp();
#endif
#if TestInitFree || TestInitHelp
    testFreeHelp(testSquares);
#endif
  }

  //
  // Return squares that remain in front of each square,
  // as potential Pawn Advancements:
  //
  private static Plane free(Int32 nSide, Int32 nPawn) {
#if InitFree
    var qpFree = Parameter[nSide].Free[nPawn];
#else
    var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

    Plane qpFree = default;
    switch (nSide) {
    case Black:
      qpFree = whiteParameter.Help[nPawn] >> nFiles * 2;
#if TestInvalidPawnPositions
      if (y(nPawn) > blackParameter.PawnRank) {
        var nFree = sqr(x(nPawn), blackParameter.PawnRank);
        qpFree |= bit(nFree);
      }
#endif
      break;

    case White:
      qpFree = blackParameter.Help[nPawn] << nFiles * 2;
#if TestInvalidPawnPositions
      if (y(nPawn) < whiteParameter.PawnRank) {
        var nFree = sqr(x(nPawn), whiteParameter.PawnRank);
        qpFree |= bit(nFree);
      }
#endif
      break;
    }
#endif
    return qpFree;
  }

  //
  // Return the Pawn Stop square in front of each square;
  // and all help squares prior to that:
  //
  private static Plane help(Int32 nSide, Int32 nPawn) {
#if InitHelp
    var qpHelp = Parameter[nSide].Help[nPawn];
#else
    var (blackParameter, whiteParameter) = Parameter.GetBothParameters();

    Plane qpHelp = default;
    switch (nSide) {
    case Black:
      qpHelp = whiteParameter.Free[nPawn] >> nFiles * 2;
#if TestInvalidPawnPositions
      var nHelpBlack = sqr(x(nPawn), blackParameter.PawnRank);
      qpHelp |= bit(nHelpBlack);
#endif
      break;

    case White:
      qpHelp = blackParameter.Free[nPawn] << nFiles * 2;
#if TestInvalidPawnPositions
      var nHelpWhite = sqr(x(nPawn), whiteParameter.PawnRank);
      qpHelp |= bit(nHelpWhite);
#endif
      break;
    }
#endif
    return qpHelp;
  }

  private static (Plane qpFree, Plane qpHelp) getFreeHelp(Int32 nSide, Int32 nPawn) {
    var qpFree = free(nSide, nPawn);
    var qpHelp = help(nSide, nPawn);
    return (qpFree, qpHelp);
  }

  private static void testFreeHelp(Sq[] squares) {
    foreach (var sq in squares) {
      var n = (Int32)sq;
      foreach (var parameter in Parameter) {
        var nSide = (Int32)parameter.SideName;
        var (qpFree, qpHelp) = getFreeHelp(nSide, n);
#if TestInitFree
        LogLine($"free({parameter.SideName}, {sq})\n");
        WriteOrth(qpFree);
        LogLine();
#endif
#if TestInitHelp
        LogLine($"help({parameter.SideName}, {sq})\n");
        WriteOrth(qpHelp);
        LogLine();
#endif
      }
    }
  }
  #endregion                            // Free & Help Load Methods
}
