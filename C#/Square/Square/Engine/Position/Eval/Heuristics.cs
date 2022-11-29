//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2022-08-11 CNHume]Split Heuristic Methods into their own file
//
// Conditionals:
//
//#define BuildAtxTo
//#define Controlled
//#define ShowCornerCP
//#define TestOutsideSquare
#define Mobility

namespace Engine {
  using System;
  using static System.Math;
  using static CacheValue.PawnPosition;

  //
  // Type Aliases:
  //
  using Eval = Int16;
  using Plane = UInt64;

  partial class Position : Board {
    #region Methods
    //
    //[ToDo]Add evaluation of practical Draws where helpmate is possible:
    //
    // KKNN or KNKNN
    // KBKB opposite color
    // KNKBN, KBKBN. KBKBB pair [Note: KNKBB pair can be won; but perhaps not in 50 moves]
    //
    // More Complex Draws, assuming weaker side in time to defend:
    //
    // KKP if weaker side maintains opposition
    // Distant Opposition vs. Key/Critical Squares vs Corresponding or Relative Squares!?
    //
    // KBKBP of opposite color
    // KPKQ if P to queen on ACFH-file and sronger K too far to help
    //

    //
    // Development [initial move per piece, delaying heavy pieces]
    // Control of the Center by Pawns
    // Control of Squares around King
    // Castling and King Safety [Corner Squares] vs King Activity in the Endgame
    //
    // Rooks on Open Files
    // Rook or Queen on 7th [or 2nd] Rank
    // Protected Piece and Connected Rook Bonus
    // Knight Scope and Outpost [free of Pawn harrassment]
    // Bishop Scope [and Bad Bishop Detection]
    //
    // Bonus for Bishop with color of Promotion Square for any Passed Pawns, especially
    // Rook Pawns.  Applies to defense and Passed Opposing Pawns, as well as to offense.
    //
    // Attack Pawn Chains at the base
    // Bonus for Passed Pawn Couples
    //
    // Piece Values depending on Opening, Middle, End Game Phase and Relative Advantage:
    // Stronger side favors [vice versa Weaker side is averse to]
    // Bishops of Opposite Colors in Middle Game; and Bishops of Same Color in Endgame.
    //

    //
    // Technical and Tablebase Draws:
    // KRKRP if weaker side can attain Philidor Position
    //
    // EGTB Alternatives: Syzygy by Ronald de Man, Gaviota EGTB by Miguel A. Ballicora,
    // Nalimov by Eugene Nalimov.  Syzygy is preferred by Houdini.  It is compact; but
    // does not provide Distance to Mate (DTM).  They provide Wind-Draw-Loss (WDL) and
    // Distance to Zero (DTZ).
    //
    // The Syzygy 6-man EGTB is available at http://tablebase.sesse.net/syzygy, where
    // the (290) 3-4-5-men files require 938 MB; and (730) 6-men files require 149 GB.
    //
    #region End Game Detection
    private Boolean isKQvKPEndgame2(BoardSide attacker, BoardSide defender) {
      var qpAttackerPawn = attacker.Piece & Pawn;
      if (qpAttackerPawn != 0)
        return false;

      var qpDefenderQueen = defender.Piece & Queen;
      if (qpDefenderQueen != 0)
        return false;

      var qpDefenderPawn = defender.Piece & Pawn;
      if (qpDefenderPawn == 0)          // Defender must have at least one Pawn
        return false;

      var qpAttackerQueen = attacker.Piece & Queen;
      if (qpAttackerQueen == 0)         // Attacker must have at least one Queen
        return false;

      var bEndGame =
        IsOneOrNone(qpDefenderPawn) &&  // Defender has at most one Pawn
        IsOneOrNone(qpAttackerQueen);   // Attacker has at most one Queen

      return bEndGame;
    }

    private Boolean isKQvKPEndgame() {
      if ((Knight | Bishop | Rook) != 0)
        return false;

      var blackSide = Side[Black];
      var whiteSide = Side[White];

      return
        isKQvKPEndgame2(blackSide, whiteSide) ||
        isKQvKPEndgame2(whiteSide, blackSide);
    }

    private Boolean isKBNvKEndgame(SideFlags fside) {
      //[Assume]KingAlone and No Rooks or Queens
      if ((Bishop | Knight) == 0)       // At least one Bishop and one Knight
        return false;

      var bEndgame =
        IsOneOrNone(Knight) &&          // At most one Knight
        !hasBishopPair(fside);          // No Bishop Pair

      return bEndgame;
    }

    private GameFlags getEndGameFlags() {
      GameFlags fgame = default;
      if (Side[Black].IsAlone()) fgame |= GameFlags.BlackAlone;
      if (Side[White].IsAlone()) fgame |= GameFlags.WhiteAlone;

      if (!fgame.Has(GameFlags.KingAlone)) {
        if (isKQvKPEndgame()) fgame |= GameFlags.KQvKP;
      }
      else if (OrthPiece == 0) {        // No Rooks or Queens
        fgame |= GameFlags.OutsideSquare;
        var bWhiteAttacker = fgame.Has(GameFlags.BlackAlone);
        var attacker = getSide(bWhiteAttacker);
        if (isKBNvKEndgame(attacker.FlagsSide)) fgame |= GameFlags.KBNvK;
      }

      return fgame;
    }

    private void setEndGameFlags() {
      FlagsGame &= ~GameFlags.EndGame;
      FlagsGame |= getEndGameFlags();
    }
    #endregion                          // End Game Detection

    #region King Outside Square of the Pawn
    private Eval punishOutsideSquare() {
      var bWhiteAlone = FlagsGame.Has(GameFlags.WhiteAlone);
      var bWTM = WTM();
      var parameter = Parameter[bWTM ? White : Black];
      var bKingToMoveLoss = bWhiteAlone == bWTM;
      var qpArray = bKingToMoveLoss ? parameter.KingToMoveLoss : parameter.PawnToMoveWins;

      var side = getSide(bWhiteAlone);
      var vDefendingKingPos = side.GetKingPos();
      var bOutside = (qpArray[vDefendingKingPos] & Pawn) != 0;
      var nReward = bOutside ? (Int32)mOutsideSquareWeight : 0;

      if (bWhiteAlone) nReward = -nReward;
#if TestOutsideSquare
      if (bOutside) {
        var sideName = parameter.SideName;
        var sOutcome = bKingToMoveLoss ? "KingToMoveLoss" : "PawnToMoveWins";
        var sq = (sq)vDefendingKingPos;
        testOrth($"{sideName}{sOutcome}[{sq}]", qpArray[vDefendingKingPos]);
        testOrth("Pawns", Pawn);
        DisplayCurrent("OutsideSquare");
      }
#endif
      return (Eval)nReward;
    }
    #endregion                          // King Outside Square of the Pawn

    #region KBN Endgame
    private static Int32 edgeDistance(Int32 n) {
      var dx = Min(x(n), invertFile(x(n)));
      var dy = Min(y(n), invertRank(y(n)));
      return Min(dx, dy);
    }

    private static Int32 distance(Int32 m, Int32 n) {
      var dx = Abs(x(n) - x(m));
      var dy = Abs(y(n) - y(m));
      return Max(dx, dy);
    }

    private static Int32 liteCornerDistance(Int32 n) {
      var distA8 = distance((Int32)sq.a8, n);
      var distH1 = distance((Int32)sq.h1, n);
      return Min(distA8, distH1);
    }

    private static Int32 darkCornerDistance(Int32 n) {
      var distA1 = distance((Int32)sq.a1, n);
      var distH8 = distance((Int32)sq.h8, n);
      return Min(distA1, distH8);
    }

    private static Int32 liteCornerDefender(Int32 n) {
      return liteCornerDistance(n) + edgeDistance(n);
    }

    private static Int32 darkCornerDefender(Int32 n) {
      return darkCornerDistance(n) + edgeDistance(n);
    }

    private static Int32 liteCornerReward(Int32 n) {
      var defence = liteCornerDefender(n);
      var offence = nFiles - defence;
      return mKBNvKMateCornerWeight * offence / nFiles;
    }

    private static Int32 darkCornerReward(Int32 n) {
      var defence = darkCornerDefender(n);
      var offence = nFiles - defence;
      return mKBNvKMateCornerWeight * offence / nFiles;
    }
#if ShowCornerCP
    private static Int32 liteCornerCP(Int32 n) {
      var nReward = liteCornerReward(n);
      return Round(100 * nReward, mUnitWeight);
    }

    private static Int32 darkCornerCP(Int32 n) {
      var nReward = darkCornerReward(n);
      return Round(100 * nReward, mUnitWeight);
    }
#endif
    private Eval rewardKBNvKMateCorner() {
      var bWhiteAttacker = FlagsGame.Has(GameFlags.BlackAlone);
      var (attacker, defender) = GetSides(bWhiteAttacker);
      var vDefenderKingPos = defender.GetKingPos();

      // Bishop color determines the mating corner
      var bLite = attacker.FlagsSide.Has(SideFlags.Lite);
      var nReward = bLite ?
        liteCornerReward(vDefenderKingPos) :
        darkCornerReward(vDefenderKingPos);
      return (Eval)(bWhiteAttacker ? nReward : -nReward);
    }

    private Eval rewardKQvKPProximity() {
      const Int32 nMaxPawnDistance = nFiles - 2;
      var bWhiteAttacker = (Side[Black].Piece & Pawn) != 0;
      var attacker = getSide(bWhiteAttacker);
      var vAttackerKingPos = attacker.GetKingPos();
      var qp = Pawn;
      var nPawnPos = RemoveLo(ref qp);
      var nDistance = distance(vAttackerKingPos, nPawnPos);
      var nProximity = nMaxPawnDistance + 1 - nDistance;
      var nReward = mKQvKPProximityWeight * nProximity / nMaxPawnDistance;
      return (Eval)(bWhiteAttacker ? nReward : -nReward);
    }
    #endregion                          // KBN Endgame

    #region Wrong Bishop
    //
    // To be considered "Wrong" a given side must have a Bishop to begin with.
    // Wrong Bishops DO NOT protect the queening square of a Passed Rook Pawn:
    //
    private static Boolean punishWrongBishop(PRPFlags fprp, SideFlags fside) {
      var bWrong =
        fside.Has(SideFlags.Pair) &&
        fprp.Has(PRPFlags.Both) &&
        (fprp.Has(PRPFlags.Lite) && !fside.Has(SideFlags.Lite) ||
         fprp.Has(PRPFlags.Dark) && !fside.Has(SideFlags.Dark));

      return bWrong;
    }
    #endregion                          // Wrong Bishop

    #region Rook Behind Passer
    //
    // Rooks Belong Behind Passed Pawns, whether they are on offence or defence:
    //
    private Eval rookBehindPasser(Boolean bWhiteRook, Plane qpPassers) {
      Eval mBehind = 0;

      var (attacker, defender) = GetSides(bWhiteRook);
      var qpAttacker = attacker.Piece & Rook;
      var qpDefender = defender.Piece & Rook;

      while (qpPassers != 0) {
        var nPasser = RemoveLo(ref qpPassers, out Plane qpPasser);
        //[Speed]Omit fileAtx() lookup unless Pawn and Rook are on the same file
        var nPasserFile = x(nPasser);
        var qpBehind = bWhiteRook ? qpPasser - 1 : MASK64 << nPasser + 1;

        var qpAttackerBehind = qpAttacker & qpBehind;
        while (qpAttackerBehind != 0) {
          var nRook = RemoveLo(ref qpAttackerBehind);
          if (x(nRook) == nPasserFile && (fileAtx(nRook) & qpPasser) != 0)
            mBehind += mRookBehindPasserAttacker;
        }

        var qpDefenderBehind = qpDefender & qpBehind;
        while (qpDefenderBehind != 0) {
          var nRook = RemoveLo(ref qpDefenderBehind);
          if (x(nRook) == nPasserFile && (fileAtx(nRook) & qpPasser) != 0)
            mBehind -= mRookBehindPasserDefender;
        }
      }

      return mBehind;
    }
    #endregion                          // Rook Behind Passer

    #region Mobility and Square Control
    //
    // Pseudo Attacks are counted for both sides.
    // Pawn Advances and Castling are not included.
    //
    private Eval mobility() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      var nControlValue = 0;
      var nMobileValue = 0;
#if Controlled
      var nControlTotal = 0;
      var nControlDelta = 0;

      // The following bit planes are only used to determine which side controls a given square:
      AttackedSum =
        BlackControlled =
        WhiteControlled = 0UL;

      Array.Clear(ControlTo, 0, ControlTo.Length);
#endif
      var nBlackAtx = blackSide.AtxCount();
      var nWhiteAtx = whiteSide.AtxCount();
#if Controlled
      var qpAtx = AttackedSum;
      while (qpAtx != 0) {
        Plane qpTo;
        var n = RemoveLo(ref qpAtx, out qpTo);
        var z = ControlTo[n];
        if (z > 0) {
          WhiteControlled |= qpTo;
          nControlDelta += Importance[n];
          nControlTotal += Importance[n];
        }
        else if (z < 0) {
          BlackControlled |= qpTo;
          nControlDelta -= Importance[n];
          nControlTotal += Importance[n];
        }
      }
#if TestControlled
      DisplayCurrent("mobility()");

      LogLine("WhiteControlled\n");
      writeOrth(WhiteControlled);
      LogLine();

      LogLine("BlackControlled\n");
      writeOrth(BlackControlled);
      LogLine();

      var qpNeutral = AttackedSum & ~(WhiteControlled | BlackControlled);
      LogLine("Neutral\n");
      writeOrth(qpNeutral);
      LogLine();
#endif
      nControlValue = 2 * mPawnWeight * nControlDelta / nControlTotal;
#endif
      var nMobileTotal = nWhiteAtx + nBlackAtx;
      if (nMobileTotal != 0) {
        var nMobileDelta = nWhiteAtx - nBlackAtx;
        nMobileValue = 2 * mPawnWeight * nMobileDelta / nMobileTotal;
      }

      return (Eval)(nControlValue + nMobileValue);
    }
    #endregion                          // Mobility and Square Control
    #endregion                          // Methods
  }
}
