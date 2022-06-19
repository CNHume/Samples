//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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

namespace Engine {
  using System;

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
    protected static void loadOutsideSquare() {
      //
      // Pawns cannot appear on the first or last ranks;
      // and start on the second or second-to-last rank:
      //
      var qpWhite = BIT0 << nFiles;
      var qpBlack = BITHI >> nFiles;

      for (var nPawnY = 1; nPawnY < nRanks - 1; nPawnY++) {
        // Size of the Square depends on how close the Black Pawn is to queening
        var nBase1 = nPawnY > 1 ? nPawnY : nPawnY + 1;  // Beyond vs On 2nd Rank
        var nSize1 = nRanks - nBase1;

        for (var nPawnX = 0; nPawnX < nFiles; nPawnX++, qpWhite <<= 1, qpBlack >>= 1) {
          for (Int32 nWhite = 0, nWhiteKingY = 0; nWhiteKingY < nRanks; nWhiteKingY++) {
            var nBlackKingY = nRanks - 1 - nWhiteKingY;

            for (var nWhiteKingX = 0; nWhiteKingX < nFiles; nWhiteKingX++, nWhite++) {
              var nBlack = nSquares - 1 - nWhite;
              var nBlackKingX = nFiles - 1 - nWhiteKingX;
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
              //var sqBlack = (sq)nBlack;
              //var sqWhite = (sq)nWhite;

              //
              // Sum Pawn positions with King outside the Square of the Pawn:
              //
              if (bOutsideKingToMove) {
                Parameter[Black].KingToMoveLoss[nBlack] |= qpWhite;
                Parameter[White].KingToMoveLoss[nWhite] |= qpBlack;
              }

              if (bOutsidePawnToMove) {
                Parameter[Black].PawnToMoveWins[nWhite] |= qpBlack;
                Parameter[White].PawnToMoveWins[nBlack] |= qpWhite;
              }
            }                           // nKingX
          }                             // nKingY
        }                               // nPawnX
      }                                 // nPawnY
    }
    #endregion

    #region Free & Help Load Methods
#if TestInitHelp || InitFree || !InitHelp
    //
    // Mark squares that remain in front of each square,
    // as potential Pawn Advancements:
    //
    private static void loadFree() {
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
          var nBlack = nSquares - 1 - nWhite;
          Parameter[Black].Free[nBlack] = qpBlack;
          Parameter[White].Free[nWhite] = qpWhite;
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
      var qpWhite = 0UL;
      var qpBlack = 0UL;

      for (Int32 nWhite = 0, y = 0; y < nRanks; y++) {
        qpWhite |= BIT0 << nFiles;
        qpBlack |= BIT0 << nRankLast - 1;

        for (var x = 0; x < nFiles; x++, nWhite++, qpWhite <<= 1, qpBlack >>= 1) {
          var nBlack = nSquares - 1 - nWhite;
          Parameter[Black].Help[nBlack] = qpBlack;
          Parameter[White].Help[nWhite] = qpWhite;
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
    protected static Plane free(Int32 nSide, Int32 nPawn) {
#if InitFree
      var qpFree = Parameter[nSide].Free[nPawn];
#else
      Plane qpFree = default;
      switch (nSide) {
      case Black:
        qpFree = Parameter[White].Help[nPawn] >> nFiles * 2;
#if TestInvalidPawnPositions
        if (nPawn >= nFiles * (nRanks - 1))
          qpFree |= BIT0 << nRankLast + nPawn;
#endif
        break;

      case White:
        qpFree = Parameter[Black].Help[nPawn] << nFiles * 2;
#if TestInvalidPawnPositions
        if (nPawn < nFiles)
          qpFree |= BIT0 << nFiles + nPawn;
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
    protected static Plane help(Int32 nSide, Int32 nPawn) {
#if InitHelp
      var qpHelp = Parameter[nSide].Help[nPawn];
#else
      Plane qpHelp = default;
      switch (nSide) {
      case Black:
        qpHelp = Parameter[White].Free[nPawn] >> nFiles * 2;
#if TestInvalidPawnPositions
        qpHelp |= BIT0 << nFiles * (nRanks - 2) + x(nPawn);
#endif
        break;

      case White:
        qpHelp = Parameter[Black].Free[nPawn] << nFiles * 2;
#if TestInvalidPawnPositions
        qpHelp |= BIT0 << nFiles + x(nPawn);
#endif
        break;
      }
#endif
      return qpHelp;
    }

    protected static (ulong qpFree, ulong qpHelp) GetFreeHelp(Int32 nSide, Int32 nPawn) {
      var qpFree = free(nSide, nPawn);
      var qpHelp = help(nSide, nPawn);
      return (qpFree, qpHelp);
    }

    private static void testFreeHelp(sq[] squares) {
      foreach (var sq in squares) {
        var n = (Int32)sq;
        foreach (var parameter in Parameter) {
          var nSide = (Int32)parameter.SideName;
          var (qpFree, qpHelp) = GetFreeHelp(nSide, n);
#if TestInitFree
          LogLine($"free({parameter.SideName}, {sq})\n");
          writeRect(qpFree);
          LogLine();
#endif
#if TestInitHelp
          LogLine($"help({parameter.SideName}, {sq})\n");
          writeRect(qpHelp);
          LogLine();
#endif
        }
      }
    }
    #endregion
  }
}
