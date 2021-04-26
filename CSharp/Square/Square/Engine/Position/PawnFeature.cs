//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2021-04-21 CNHume]Created File
//
// Conditionals:
//
#define TestInitFree
//#define TestInitHelp
#define InitFree                        //[Default]
//#define InitHelp                        //[Test]
//#define TestPawnFeatures

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
                KingToMoveLoss[White][nWhite] |= qpBlack;
                KingToMoveLoss[Black][nBlack] |= qpWhite;
              }

              if (bOutsidePawnToMove) {
                PawnToMoveWins[White][nBlack] |= qpWhite;
                PawnToMoveWins[Black][nWhite] |= qpBlack;
              }
            }                           // nKingX
          }                             // nKingY
        }                               // nPawnX
      }                                 // nPawnY
    }
    #endregion

    #region Free & Help Load Methods
    private static void loadFreeHelp() {
#if TestInitHelp || InitFree || !InitHelp
      loadFree();
#endif
#if TestInitFree || InitHelp || !InitFree
      loadHelp();
#endif
#if TestInitFree || TestInitHelp
      foreach (var sq in testSquares) {
        var n = (Int32)sq;
        LogLine("whiteHelp({0})\n", sq);
        writeRect(whiteHelp(n));
        //LogLine("WhiteFree[{0}]\n", sq);
        //writeRect(WhiteFree[n]);
        LogLine();
#if InitHelp
        LogLine("WhiteHelp[{0}]\n", sq);
        writeRect(WhiteHelp[n]);
        LogLine();
#endif
      }
#endif
    }

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

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);

        //
        // Advance masks to the right one File at a time
        // until they rotate back to their leftmost File,
        // whereupon they will have advanced by one Rank:
        //
        for (var x = 0; x < nFiles; x++, qpWhite <<= 1, qpBlack >>= 1) {
          var xInverse = invertFile(x);
          Free[White][sqr(x, y)] = qpWhite;
          Free[Black][sqr(xInverse, yInverse)] = qpBlack;
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

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);
        qpWhite |= BIT0 << nFiles;
        qpBlack |= BIT0 << nRankLast - 1;

        for (var x = 0; x < nFiles; x++, qpWhite <<= 1, qpBlack >>= 1) {
          var xInverse = invertFile(x);
          Help[White][sqr(x, y)] = qpWhite;
          Help[Black][sqr(xInverse, yInverse)] = qpBlack;
        }
      }
    }
#endif
#if !InitFree
    //
    // Return squares that remain in front of each square,
    // as potential Pawn Advancements:
    //
    protected Plane whiteFree(Int32 n) {
      var qpFree = Help[Black][n] << nFiles * 2;
#if TestInvalidPositions
      if (n < nFiles)
        qpFree |= BIT0 << nFiles + n;
#endif
      return qpFree;
    }

    protected Plane blackFree(Int32 n) {
      var qpFree = Help[White][n] >> nFiles * 2;
#if TestInvalidPositions
      if (n >= nFiles * (nRanks - 1))
        qpFree |= BIT0 << nRankLast + n;
#endif
      return qpFree;
    }
#endif
#if !InitHelp
    //
    // Return the Pawn Stop square in front of each square;
    // and all help squares prior to that:
    //
    private static Plane whiteHelp(Int32 n) {
      var qpHelp = Free[Black][n] << nFiles * 2;
#if TestInvalidPositions
      qpHelp |= BIT0 << nFiles + n % nFiles;
#endif
      return qpHelp;
    }

    private static Plane blackHelp(Int32 n) {
      var qpHelp = Free[White][n] >> nFiles * 2;
#if TestInvalidPositions
      qpHelp |= BIT0 << nFiles * (nRanks - 2) + n % nFiles;
#endif
      return qpHelp;
    }
#endif
    protected static (ulong qpFree, ulong qpHelp) getFreeHelp(bool bWhiteCount, int nPawn) {
#if InitFree
      var qpFree = bWhiteCount ? Free[White][nPawn] : Free[Black][nPawn];
#else
      var qpFree = bWhiteCount ? whiteFree(nPawn) : blackFree(nPawn);
#endif
#if InitHelp
      var qpHelp = bWhiteCount ? Help[White][nPawn] : Help[Black][nPawn];
#else
      var qpHelp = bWhiteCount ? whiteHelp(nPawn) : blackHelp(nPawn);
#endif
      return (qpFree, qpHelp);
    }
    #endregion
  }
}
