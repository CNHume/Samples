//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2021-04-21 CNHume]Created File
//
// Conditionals:
//
#define InitFree                        //[Default]
//#define InitHelp                        //[Test]
//#define TestInitFree
//#define TestInitHelp
//#define TestPawnFeatures
//#define TestInvalidPawnPositions

namespace Engine {
  using System;

  using static Board.BoardSide;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class Position {
    #region Pawn Feature New Methods
    private static void newPawnFeature() {
      foreach (var sideName in (SideName[])Enum.GetValues(typeof(SideName))) {
        var hashcode = sideName == SideName.Black ? ZobristBlack : ZobristWhite;
        var nSide = (Int32)sideName;
        KingToMoveLoss[nSide] = new Plane[nSquares];
        PawnToMoveWins[nSide] = new Plane[nSquares];
#if TestInitHelp || InitFree || !InitHelp
        Free[nSide] = new Plane[nSquares];
#endif
#if TestInitFree || InitHelp || !InitFree
        Help[nSide] = new Plane[nSquares];
#endif
      }
    }
    #endregion

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
          for (int nWhite = 0, nWhiteKingY = 0; nWhiteKingY < nRanks; nWhiteKingY++) {
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
                KingToMoveLoss[Black][nBlack] |= qpWhite;
                KingToMoveLoss[White][nWhite] |= qpBlack;
              }

              if (bOutsidePawnToMove) {
                PawnToMoveWins[Black][nWhite] |= qpBlack;
                PawnToMoveWins[White][nBlack] |= qpWhite;
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

      for (int nWhite = 0, y = 0; y < nRanks; y++) {
        //
        // Advance masks to the right one File at a time
        // until they rotate back to their leftmost File,
        // whereupon they will have advanced by one Rank:
        //
        for (var x = 0; x < nFiles; x++, nWhite++, qpWhite <<= 1, qpBlack >>= 1) {
          var nBlack = nSquares - 1 - nWhite;
          Free[Black][nBlack] = qpBlack;
          Free[White][nWhite] = qpWhite;
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

      for (int nWhite = 0, y = 0; y < nRanks; y++) {
        qpWhite |= BIT0 << nFiles;
        qpBlack |= BIT0 << nRankLast - 1;

        for (var x = 0; x < nFiles; x++, nWhite++, qpWhite <<= 1, qpBlack >>= 1) {
          var nBlack = nSquares - 1 - nWhite;
          Help[Black][nBlack] = qpBlack;
          Help[White][nWhite] = qpWhite;
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
#if !InitFree
    private static Plane free(Int32 nSide, Int32 nPawn) {
      //
      // Return squares that remain in front of each square,
      // as potential Pawn Advancements:
      //
      Plane qpFree = default;
      switch (nSide) {
      case Black:
        qpFree = Help[White][nPawn] >> nFiles * 2;
#if TestInvalidPawnPositions
        if (nPawn >= nFiles * (nRanks - 1))
          qpFree |= BIT0 << nRankLast + nPawn;
#endif
        break;

      case White:
        qpFree = Help[Black][nPawn] << nFiles * 2;
#if TestInvalidPawnPositions
        if (nPawn < nFiles)
          qpFree |= BIT0 << nFiles + nPawn;
#endif
        break;
      }
      return qpFree;
    }
#endif
#if !InitHelp
    private static Plane help(Int32 nSide, Int32 nPawn) {
      //
      // Return the Pawn Stop square in front of each square;
      // and all help squares prior to that:
      //
      Plane qpHelp = default;
      switch (nSide) {
      case Black:
        qpHelp = Free[White][nPawn] >> nFiles * 2;
#if TestInvalidPawnPositions
        qpHelp |= BIT0 << nFiles * (nRanks - 2) + nPawn % nFiles;
#endif
        break;

      case White:
        qpHelp = Free[Black][nPawn] << nFiles * 2;
#if TestInvalidPawnPositions
        qpHelp |= BIT0 << nFiles + nPawn % nFiles;
#endif
        break;
      }
      return qpHelp;
    }
#endif
    protected static (ulong qpFree, ulong qpHelp) GetFreeHelp(Int32 nSide, Int32 nPawn) {
#if InitFree
      var qpFree = Free[nSide][nPawn];
#else
      var qpFree = free(nSide, nPawn);
#endif
#if InitHelp
      var qpHelp = Help[nSide][nPawn];
#else
      var qpHelp = help(nSide, nPawn);
#endif
      return (qpFree, qpHelp);
    }

    private static void testFreeHelp(sq[] squares) {
      foreach (var sq in squares) {
        var n = (Int32)sq;
        foreach (var sideName in (SideName[])Enum.GetValues(typeof(SideName))) {
          var nSide = (Int32)sideName;
          var (qpFree, qpHelp) = GetFreeHelp(nSide, n);
#if TestInitFree
          LogLine($"Free[{sideName}][{sq}]\n");
          writeRect(qpFree);
          LogLine();
#endif
#if TestInitHelp
          LogLine($"Help[{sideName}][{sq}]\n");
          writeRect(qpHelp);
          LogLine();
#endif
        }
      }
    }
    #endregion
  }
}
