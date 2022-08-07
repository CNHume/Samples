//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-06-09 CNHume]Created File
//
// Conditionals:
//
//#define BuildAtxTo
//#define PawnPositionByValue
//#define DebugComposition
//#define NoPieceHash
//#define ShowCornerCP
#define EvalBishopPair
#define EvalWrongBishop
#define EvalOutsideSquare
#define EvalKQvKPDistance
#define EvalKBNvKMateCorner
#define EvalRookBehindPasser
#define EvalInsufficient
//#define TestOutsideSquare
//#define TestRookBehindPasser
#define Mobility
//#define MaterialBalance
//#define TradePieces
//#define TraceVal
//#define FailHard
#define TraceInfinity
//#define VerifyIBV

namespace Engine {
  using CacheValue;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Text;

  using static CacheValue.PawnPosition;
  using static Logging.Logger;

  using static System.Math;

  //
  // Type Aliases:
  //
  using Bval = Int16;
  using CompositionCounter = UInt16;
  using Depth = UInt16;
  using Draft = UInt16;
  using Eval = Int16;
  using Hashcode = UInt64;
  using MemoHashcode = UInt32;
  using Plane = UInt64;
  using Ply = UInt16;

  partial class Position : Board {
    #region Constants
    public const Eval EvalMask = (1 << 14) - 1;
    public const Eval PlusInfinity = EvalMask >> 1;
    public const Eval MinusInfinity = -PlusInfinity;
    //[Old]public const Eval EvalUndefined = MinusInfinity - 1;
    public const Eval EvalUndefined = Bval.MinValue;

    //
    //[Analysis]Is headroom needed to guard against MateRange Overflow or Underflow,
    // due to Search Move adjustments?  This came up when testing Aspiration Search.
    //
    public const Eval MateRange = 256;  // Max # of Moves to Mate
    public const Eval MateMax = PlusInfinity - 1;
    public const Eval MateMin = MateMax - MateRange;
    public const Eval EvalMax = MateMin - 1;

    //
    // Unit Weight supports 14-bit evaluations, as follows:
    // EvalMax = 8192 - 2 - 256 = 7934
    // Max Score = EvalMax / Unit Weight = 7934 / 60 = 132
    // Max Heavy Piece Score = 9 Queens + 2 Rooks = 100
    // Max Light Piece Score = 2 Bishops + 2 Knights = 28
    // Positional Rewards = Max Score - Max Piece Score = 4
    //
    internal const Eval mUnitWeight = 60;
    protected const Eval mHalfWeight = mUnitWeight / 2;
    protected const Eval mThirdWeight = mUnitWeight / 3;
    protected const Eval mQuarterWeight = mHalfWeight / 2;
    protected const Eval mFifthWeight = mUnitWeight / 5;
    protected const Eval mTenthWeight = mFifthWeight / 2;

    protected const Eval mPawnWeight = mUnitWeight;
    protected const Eval mRookWeight = 5 * mUnitWeight; // + mQuarterWeight
    protected const Eval mKnightWeight = 3 * mUnitWeight + mQuarterWeight;
    protected const Eval mBishopWeight = 3 * mUnitWeight + mQuarterWeight;
    protected const Eval mQueenWeight = 2 * mBishopWeight + mKnightWeight;

    protected const Eval mInsufficientWeight = mHalfWeight;
    protected const Eval mThreatWeight = mKnightWeight;
    protected const Eval mStandPatWeight = mPawnWeight + mHalfWeight;//[Unused]
    protected const Eval mDeltaBaseWeight = 2 * mPawnWeight;
    protected const Eval mOccamBaseWeight = 4 * mPawnWeight;
    protected const Eval mSingularWeight = 3 * mQuarterWeight;
    protected const Eval mPassedPawnPushWeight = mFifthWeight / 2;
    protected const Eval mKBNvKMateCornerWeight = 2 * mPawnWeight;// Divisible by nFiles
    protected const Eval mKQvKPProximityWeight = mPawnWeight;
    protected const Eval mOutsideSquareWeight = mRookWeight;
    protected const Eval mWrongBishopWeight = mBishopWeight;      //[Note]Wrong Bishop test fails at large depth if this is small
    protected const Eval mBishopPairWeight = 3 * mFifthWeight;
    protected const Eval mRookBehindPasserAttacker = mQuarterWeight;
    protected const Eval mRookBehindPasserDefender = mThirdWeight;

    //
    //[Assume]P, N, B, R, Q Piece Order
    //
    public static readonly Eval[] PieceWeight =
    { mPawnWeight, mKnightWeight, mBishopWeight, mRookWeight, mQueenWeight };
    #endregion

    #region EGFlags Methods
    public Boolean isKQvKPEndgame2(BoardSide attacker, BoardSide defender) {
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

    public Boolean isKBNvKEndgame(SideFlags fside) {
      //[Assume]KingAlone and No Rooks or Queens
      if ((Bishop | Knight) == 0)       // At least one Bishop and one Knight
        return false;

      var bEndgame =
        IsOneOrNone(Knight) &&          // At most one Knight
        !hasBishopPair(fside);          // No Bishop Pair

      return bEndgame;
    }

    public GameFlags getEndGameFlags() {
      GameFlags fgame = default;
      if (Side[Black].IsAlone()) fgame |= GameFlags.BlackAlone;
      if (Side[White].IsAlone()) fgame |= GameFlags.WhiteAlone;

      if (!fgame.Has(GameFlags.KingAlone)) {
        if (isKQvKPEndgame()) fgame |= GameFlags.KQvKP;
      }
      else if (RectPiece == 0) {        // No Rooks or Queens
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
    #endregion

    #region King Outside Square of the Pawn
    protected Eval punishOutsideSquare() {
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
        testRect($"{sideName}{sOutcome}[{sq}]", qpArray[vDefendingKingPos]);
        testRect("Pawns", Pawn);
        DisplayCurrent("OutsideSquare");
      }
#endif
      return (Eval)nReward;
    }
    #endregion

    #region KBN Endgame
    protected static Int32 edgeDistance(Int32 n) {
      var dx = Min(x(n), invertFile(x(n)));
      var dy = Min(y(n), invertRank(y(n)));
      return Min(dx, dy);
    }

    protected static Int32 distance(Int32 m, Int32 n) {
      var dx = Abs(x(n) - x(m));
      var dy = Abs(y(n) - y(m));
      return Max(dx, dy);
    }

    protected static Int32 liteCornerDistance(Int32 n) {
      var distA8 = distance((Int32)sq.a8, n);
      var distH1 = distance((Int32)sq.h1, n);
      return Min(distA8, distH1);
    }

    protected static Int32 darkCornerDistance(Int32 n) {
      var distA1 = distance((Int32)sq.a1, n);
      var distH8 = distance((Int32)sq.h8, n);
      return Min(distA1, distH8);
    }

    protected static Int32 liteCornerDefender(Int32 n) {
      return liteCornerDistance(n) + edgeDistance(n);
    }

    protected static Int32 darkCornerDefender(Int32 n) {
      return darkCornerDistance(n) + edgeDistance(n);
    }

    protected static Int32 liteCornerReward(Int32 n) {
      var defence = liteCornerDefender(n);
      var offence = nFiles - defence;
      return mKBNvKMateCornerWeight * offence / nFiles;
    }

    protected static Int32 darkCornerReward(Int32 n) {
      var defence = darkCornerDefender(n);
      var offence = nFiles - defence;
      return mKBNvKMateCornerWeight * offence / nFiles;
    }
#if ShowCornerCP
    protected static Int32 liteCornerCP(Int32 n) {
      var nReward = liteCornerReward(n);
      return Round(100 * nReward, mUnitWeight);
    }

    protected static Int32 darkCornerCP(Int32 n) {
      var nReward = darkCornerReward(n);
      return Round(100 * nReward, mUnitWeight);
    }
#endif
    protected Eval rewardKBNvKMateCorner() {
      var bWhiteAttacker = FlagsGame.Has(GameFlags.BlackAlone);
      var (attacker, defender) = getSides(bWhiteAttacker);
      var vDefenderKingPos = defender.GetKingPos();

      // Bishop color determines the mating corner
      var bLite = attacker.FlagsSide.Has(SideFlags.Lite);
      var nReward = bLite ?
        liteCornerReward(vDefenderKingPos) :
        darkCornerReward(vDefenderKingPos);
      return (Eval)(bWhiteAttacker ? nReward : -nReward);
    }

    protected Eval rewardKQvKPProximity() {
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
    #endregion

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
    #endregion

    #region Rook Behind Passer
    //
    // Rooks Belong Behind Passed Pawns, whether they are on offence or defence:
    //
    protected Eval rookBehindPasser(Boolean bWhiteRook, Plane qpPassers) {
      Eval mBehind = 0;

      var (attacker, defender) = getSides(bWhiteRook);
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
    #endregion

    #region Evaluation Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Boolean isEndgame(Eval mStaticTotal) {
      return mStaticTotal <= State.EndgameValue;        // 22.25
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Boolean isEndgame() {
      return isEndgame(StaticTotal);
    }

    protected static Eval weight(Byte vPiece) {
      return PieceWeight[vPiece];
    }

    protected static Eval weightP(Byte vPromotion) {
      return (Eval)(PieceWeight[vPromotion] - mPawnWeight);
    }

    private static Eval occamDelta(Depth wDepth) {
      var mDepthWeight = (Eval)(wDepth * mPawnWeight);
      var mValue = (Eval)(mOccamBaseWeight + mDepthWeight);
      return mValue;
    }

    //
    // Determine whether a Draw should be sought or avoided, by adding
    // the estimated handicap to an equal evaluation of Zero depending
    // on the game phase, and the relative strength of the two players.
    //
    protected Eval contempt() {
      GameState.AtomicIncrement(ref State.DrawTotal);
      var mDrawValue = (Eval)(-State.Contempt);         // Strength advantage of White over Black
      return mDrawValue;
    }
#if MaterialBalance
    protected (Eval mDelta, Eval mTotal) getValue() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      //
      // Piece Counts ignore the number of Pawns
      //
      var wBlackCounts = (CompositionCounter)(blackSide.Counts >> nCompositionOffsetBit);
      var wWhiteCounts = (CompositionCounter)(whiteSide.Counts >> nCompositionOffsetBit);
#if DebugComposition
      var sb = new StringBuilder();
      foreach (var side in Side)
        sb.AppendPieceCounts(side).AppendLine();

      sb.AppendPieceHash(blackSide, whiteSide);
      var sComposition = sb.ToString();
      LogLine(sComposition);
#endif
      var compBlack = State.GetCX2(this, blackSide.PieceHash, wBlackCounts, blackSide);
      var compWhite = State.GetCX2(this, whiteSide.PieceHash, wWhiteCounts, whiteSide);

      setEndGameFlags();                // Composition2 values do not depend on EGFlags.EndGame

      var mValueBlack = compBlack.Value;
      var mValueWhite = compWhite.Value;

      var mDelta = (Eval)(mValueWhite - mValueBlack);
      var mTotal = (Eval)(mValueWhite + mValueBlack);

      return (mDelta, mTotal);
    }
#else                                   // MaterialBalance
#if NoPieceHash
    private static Composition comp = new Composition();
#endif
    protected (Eval mDelta, Eval mTotal) getValue() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      var wBlackCounts = (CompositionCounter)(blackSide.Counts >> nCompositionOffsetBit);
      var wWhiteCounts = (CompositionCounter)(whiteSide.Counts >> nCompositionOffsetBit);
#if NoPieceHash
      var fBlackSide = blackSide.FlagsSide;
      var fWhiteSide = whiteSide.FlagsSide;

      comp.Recycle(wBlackCounts, wWhiteCounts, fBlackSide, fWhiteSide);
#else
      var uMemoHash = compositionHash(true);
#if DebugComposition
      var sb = new StringBuilder()
        .AppendPieceCounts(blackSide, whiteSide).AppendLine()
        .AppendPieceHash(blackSide, whiteSide);
      var sComposition = sb.ToString();
      LogLine(sComposition);
#endif
      var comp = State.GetCXP(this, uMemoHash, wBlackCounts, wWhiteCounts, blackSide, whiteSide);
#endif
      return ((Eval)comp.Delta, (Eval)comp.Total);
    }

    protected MemoHashcode compositionHash(Boolean bWhiteHash) {
      if (bWhiteHash)
        return (MemoHashcode)Side[Black].PieceHash << nHashPieceBits | (MemoHashcode)Side[White].PieceHash;
      else
        return (MemoHashcode)Side[White].PieceHash << nHashPieceBits | (MemoHashcode)Side[Black].PieceHash;
    }
#endif                                  // MaterialBalance
    private void clrEval() {
      StaticDelta = Position.EvalUndefined;
      StaticTotal = Position.EvalUndefined;
    }

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
    protected Eval staticEval(out PawnPosition? pp) {    //[New]~9.666 MHz vs ~13.333 MHz w/o EvalRookBehindPasser
      pp = default;
      if (IsInsufficient())
        return contempt();

      GameState.AtomicIncrement(ref State.TotalEvals);  // vs. FullEvaluations

      setEndGameFlags();

      if (EvalUndefined < StaticDelta)
        return StaticDelta;

      //
      // Retrieve material balance from the Composition:
      //
      (Eval mDelta, Eval mTotal) = getValue();

      if (Pawn != 0) {                  // Else PawnHash == default(Hashcode)
        pp = State.GetPXP(this);

        mDelta += pp.Delta;
        mTotal += pp.Total;
#if EvalWrongBishop
        var fBlackSide = Side[Black].FlagsSide;
        var fWhiteSide = Side[White].FlagsSide;

        if (punishWrongBishop(pp.BlackPRP & PRPFlags.Both, fBlackSide))
          mDelta += mWrongBishopWeight; // Black has Wrong Bishop

        if (punishWrongBishop(pp.WhitePRP & PRPFlags.Both, fWhiteSide))
          mDelta -= mWrongBishopWeight; // White has Wrong Bishop
#endif
      }
#if TradePieces
      if (mTotal > 0) {
        //
        // The following provides an incentive for the stronger
        // side to exchange material and for the weaker side to
        // avoid such exchanges.  The value is Zero in an equal
        // position, and grows to a maximum of one centipawn if
        // the weaker side has been stripped of all material.
        //
        // Intuitively: Exchanges reduce the Total material but
        // leave the Delta unaffected; and Delta can range from
        // Zero to the Total.  Thus, their quotient ranges from
        // Zero to One.
        //
        //[Note]A refinement is needed to prefer trading Pieces
        // over Pawns in the endgame.
        //
        var mIncentive = (Eval)(mPawnWeight * mDelta / mTotal);
        mDelta += mIncentive;
      }
#endif
      //
      //[Note]staticEval() prepares StaticTotal for any isEndgame() tests
      //
      StaticTotal = mTotal;             // Update for isEndgame()
      StaticDelta = mDelta;
      return mDelta;
    }

    private Eval fullEval() {
      //
      //[Note]StaticEvaluations = TotalEvaluations - FullEvaluations
      // Draws are included; because they exit early.
      //
      GameState.AtomicIncrement(ref State.FullEvals);

      var mValue = staticEval(out PawnPosition? pp);

      //
      // Load PawnFeature Deltas from the PawnPositions hash table,
      // based on HashPawn, the Zobrist Hashcode summed over Pawns.
      //
      if (Pawn == 0) {                  // PawnHash == default(Hashcode)}
#if EvalKBNvKMateCorner
        if (FlagsGame.Has(GameFlags.KBNvK)) {
          var mReward = rewardKBNvKMateCorner();
          mValue += mReward;
        }
#endif
      }
      else {
#if EvalOutsideSquare
        if (FlagsGame.Has(GameFlags.OutsideSquare)) {
          var mReward = punishOutsideSquare();
          mValue += mReward;
        }
#endif
#if EvalKQvKPDistance
        if (FlagsGame.Has(GameFlags.KQvKP)) {
          var mReward = rewardKQvKPProximity();
          mValue += mReward;
        }
#endif
#if EvalRookBehindPasser
        if (Rook != 0) {
#if PawnPositionByValue
          var bDefault = !pp.BlackPRP.Has(PRPFlags.IsValid);
#else
          var bDefault = pp == default(PawnPosition);
#endif
          if (bDefault) pp = State.GetPXP(this);
          const Boolean bWhiteRook = true;
          var mRooksBehindBlack = rookBehindPasser(!bWhiteRook, pp.BlackPassers);
          var mRooksBehindWhite = rookBehindPasser(bWhiteRook, pp.WhitePassers);
#if TestRookBehindPasser
          if (mRooksBehindBlack != 0 ||
              mRooksBehindWhite != 0) {
            DisplayCurrent("pieceval()");
          }
#endif
          mValue += mRooksBehindWhite;
          mValue -= mRooksBehindBlack;
        }
#endif
      }

      var mAbs = Abs(mValue);
      Debug.Assert(mAbs < MateMin, "Mate value returned by staticEval()");
      if (mAbs <= mDeltaBaseWeight) {   //[ToDo]Define a new threshold
#if BuildAtxTo
        buildAtxTo(RankPiece);
#endif
#if Mobility
        mValue += mobility();
#endif
      }

      //
      // The following helps find Draw3
      //
      if (IsDraw2() && mAbs < MateMin)
        mValue /= 4;                    //[IBV]Care will be needed to protect the EvalType here

      return mValue;
    }

    //
    // Dynamic Evaluation is invoked at the end of
    // a Search, e.g., when the Depth reaches Zero.
    //
    protected Eval eval() {
      var bWTM = WTM();
      var mValue = IsDraw() || IsStalemate() ? contempt() : fullEval();
      return reflectValue(bWTM, mValue);
    }

    protected Eval final() {
      //
      // The game is over if the side to move has no move.
      // If the King is in Check, this is a Checkmate and
      // the lowest possible evaluation is given.
      //
      if (InCheck()) {
        GameState.AtomicIncrement(ref State.MateTotal);
        return debitMate(-MateMax, SearchPly);
      }

      //
      // If the King is not in Check, this is a Stalemate
      // and an equal evaluation is returned.
      //
      var bWTM = WTM();
      var mValue = contempt();
      return reflectValue(bWTM, mValue);
    }

    //
    // The following boundValue() overload is primarily for use
    // when the TransposeQuiet conditional is not asserted.
    //
    private static Eval boundValue(Eval mValue, Eval mAlpha, Eval mBeta) {
#if FailHard
      if (mBeta < mValue)
        mValue = mBeta;
      else if (mValue < mAlpha)
        mValue = mAlpha;
#endif
      return mValue;
    }

    private static Eval boundValue(Eval mValue, Eval mValueFound, EvalType etFound) {
      if (EvalUndefined < mValueFound) {
        //
        // Apply Upper or Lower Bound to mValue
        //
        switch (etFound) {
        case EvalType.Upper:            // LUB
          if (mValueFound < mValue)
            mValue = mValueFound;
          break;
        case EvalType.Lower:            // GLB
          if (mValue < mValueFound)
            mValue = mValueFound;
          break;
        default:
          break;
        }
      }

      return mValue;
    }

    protected Eval standpatval(Eval mValueFound, EvalType etFound) {
      if (EvalUndefined < mValueFound) {
        switch (etFound) {
        case EvalType.Exact:
        case EvalType.Undefined:
          return mValueFound;
        default:
          break;
        }
      }

      var mValue = fullEval();

      var bWTM = WTM();
      mValue = reflectValue(bWTM, mValue);
      return boundValue(mValue, mValueFound, etFound);
    }

    protected Eval pruneval(Draft wPruneDraft, Eval mAlpha, Eval mBeta, Eval mValueFound, EvalType etFound) {
      if (EvalUndefined < mValueFound) {
        switch (etFound) {
        case EvalType.Exact:
        case EvalType.Undefined:
          return mValueFound;
        default:
          break;
        }
      }

      //var mValue = standpatval(mValueFound, etFound);
      var mValue = clonedSearch(wPruneDraft, mAlpha, mBeta);
      traceVal("pruneval()", mValue, etFound);          //[Conditional]

      return boundValue(mValue, mValueFound, etFound);
    }

    public static Int32 Round(Int32 nNumerator, Int32 nDenominator) {
      // Round Away From Zero
      var nRound = nNumerator < 0 ? -nDenominator : nDenominator;
      return (2 * nNumerator + nRound) / (2 * nDenominator);
    }
    #endregion

    #region IBV
    //
    // The following approach to Integrated Bounds and Values (IBV) is due to Don Beal
    // See https://www.chessprogramming.org/Integrated_Bounds_and_Values
    //
    // Representation:
    //
    // Exact numbers(n) are represented as 4n
    // Upper bounds(<= n) are represented as 4n-1
    // Lower bounds(>= n) are represented as 4n+1
    //
    // with following properties:
    //
    // 1. negating a bound yields in the corresponding bound from opponent's point of view (Negamax)
    // 2. a lower bound at n(>= n) is greater than an exact n
    // 3. an exact value(n) is greater than an upper bound
    //
    // Thus, a 16-bit Bval is formed from 14-bit Eval by shifting the Eval left by 2-bits and adding mIBV
    //
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Bval IBV(Eval mValue, EvalType et) {
      if (mValue == EvalUndefined)
        return (Bval)EvalUndefined;
      else {
        var nIBV = (mValue << nPerTwoBits) + twoBits((Int32)et);
        return (Bval)nIBV;
      }
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static EvalType IBType(Bval mIBV) {
      return mIBV == EvalUndefined ? EvalType.Undefined : (EvalType)twoBits(mIBV & vTwoBits);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Eval IBEval(Bval mIBV) {
      return mIBV == EvalUndefined ? EvalUndefined : (Eval)(mIBV >> nPerTwoBits);
    }
#if UsedIBV
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Bval IBLower(Bval mIBV) {
      return (Bval)(mIBV == EvalUndefined ? EvalUndefined : IBExact(mIBV) + 1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Bval IBExact(Bval mIBV) {
      return (Bval)(mIBV == EvalUndefined ? EvalUndefined : (mIBV + 1) & ~vTwoBits);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Bval IBUpper(Bval mIBV) {
      return (Bval)(mIBV == EvalUndefined ? EvalUndefined : IBExact(mIBV) - 1);
    }
#endif
    //[Note]"Side relative" values reflect to "White relative" values; and vice versa
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Eval reflectValue(Boolean bWTM, Eval mValue) {
      //
      // Note: In the NegaMax evaluation to be returned, a positive sign
      // will indicate that the Side to Move is winning.
      //
      // During the intermediate stages above (and also when alternative
      // variations are displayed by the GUI) positive signs may be used
      // to indicate that White is winning.
      //
      //[Safe]EvalUndefined == -EvalUndefined
      return bWTM || mValue == EvalUndefined ? mValue : (Eval)(-mValue);
    }
    #endregion

    #region Linear Interpolation
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 Interpolate(Int32 x, Int32 x0, Int32 y0, Int32 x1, Int32 y1) {
      if (x <= x0) return y0;
      else if (x1 <= x) return y1;
      return Extrapolate(x, x0, y0, x1, y1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 Extrapolate(Int32 x, Int32 x0, Int32 y0, Int32 x1, Int32 y1) {
      return y0 + (x - x0) * (y1 - y0) / (x1 - x0);
    }

    // See https://en.wikipedia.org/wiki/Linear_interpolation
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Double Interpolate(Double x, Double x0, Double y0, Double x1, Double y1) {
      if (x <= x0) return y0;
      else if (x1 <= x) return y1;
      return Extrapolate(x, x0, y0, x1, y1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Double Extrapolate(Double x, Double x0, Double y0, Double x1, Double y1) {
      var s = (x - x0) / (x1 - x0);
      return Lerp(s, y0, y1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Double Lerp(Double s, Double y0, Double y1) {
      return y0 + s * (y1 - y0);
    }
    #endregion

    #region Piece Evaluation
    internal static Eval weighPieces(
      CompositionCounter wPieceCounts, SideFlags fside) {
#if EvalInsufficient
      if (IsInsufficient(fside))
        return mInsufficientWeight;
#endif
      // 218.34 MHz with both EvalInsufficient and EvalBishopPair;
      // 231.5 MHz for PieceWeight sum.
      var nSum = 0;
      for (var vPiece = vCompositionOffset; vPiece < vK6; vPiece++,
           wPieceCounts >>= nPerNibble) {
        nSum += nibble(wPieceCounts) * PieceWeight[vPiece];
      }
#if EvalBishopPair
      if (hasBishopPair(fside)) {
        nSum += mBishopPairWeight;
      }
#endif
      return (Eval)nSum;
    }

    // Side has Insufficient Material to Force Mate
    protected static bool IsInsufficient(SideFlags fside) {
      return fside.Has(SideFlags.Insufficient);
    }
    #endregion

    #region Search vs Position Relative Mate Evaluation
    //
    // When checkmates are discovered, values are set to prefer
    // the shortest path to mate over longer mates.
    //
    // An immediate mate is assigned the maximum magnitude; and
    // mates requiring one or more moves have their evaluations
    // reduced in magnitude according to how many moves it will
    // take to reach the final Mate Position.
    //
    // wSearchMoves is the # of moves required to reach the Mate
    // Position from the Game Position from which the search was
    // initiated.
    //
    // When storing the Mate Value for a position which has been
    // given a Search-relative Mate Value, wSearchMoves is credited
    // so the Mate Value will be neutral with respect to however
    // many moves it may have taken to find the position.
    //
    // When a position is loaded from the Transposition Table or
    // when mates are being searched, wSearchMoves is debited to
    // convert a Position-relative Mate Value to a Search-relative
    // Mate Value.
    //
    // This allows transpositions to be used by searches of varying
    // lengths while returning values that prefer the shortest mate.
    //
    internal static Eval creditMate(Eval mValue, Ply wSearchPlies) {
#if VerifyIBV
      var mIBV = IBV(mValue, EvalType.Exact);
      var mVerify = IBEval(mIBV);
      if (mVerify != mValue) {
        Trace.Assert(mVerify == mValue, "mVerify != mValue");
      }
#endif
      var nAdjusted = (Int32)mValue;
      if (EvalUndefined < mValue) {     //[Safe]
        if (MinusInfinity < mValue && mValue < PlusInfinity) {
          var wSearchMoves = moveDelta(wSearchPlies);
          // Credit wSearchMoves when storing a Search-relative Mate Value as a Position-relative Mate Value:
          if (MateMin <= mValue) {      // Mate [Plus]
            Debug.Assert(nAdjusted <= MateMax, "Position-relative MateRange Overflow [Plus]");
            nAdjusted += wSearchMoves;

            if (PlusInfinity <= nAdjusted) {
              Debug.Assert(nAdjusted < PlusInfinity, "Position-relative Value too Large");
              nAdjusted = PlusInfinity; //[Safe]Apply Ceiling
            }
          }
          else if (mValue <= -MateMin) {// Mate [Minus]
            Debug.Assert(-MateMax <= nAdjusted, "Position-relative MateRange Overflow [Minus]");
            nAdjusted -= wSearchMoves;

            if (nAdjusted <= MinusInfinity) {
              Debug.Assert(MinusInfinity < nAdjusted, "Position-relative Value too Small");
              nAdjusted = MinusInfinity;//[Safe]Apply Ceiling
            }
          }
        }
#if TraceInfinity
        else {
          Trace.Assert(MinusInfinity < mValue, "Position-relative Value = MinusInfinity");
          Trace.Assert(mValue < PlusInfinity, "Position-relative Value = PlusInfinity");
        }
#endif
      }

      return (Eval)nAdjusted;
    }

    protected static Eval debitMate(Eval mValue, Ply wSearchPlies) {
      var nAdjusted = (Int32)mValue;
      if (EvalUndefined < mValue) {     //[Safe]
        if (MinusInfinity < mValue && mValue < PlusInfinity) {
          var wSearchMoves = moveDelta(wSearchPlies);
          // Debit wSearchMoves when loading a Position-relative Mate Value as a Search-relative Mate Value:
          if (mValue >= MateMin) {      // Mate [Plus]
            Debug.Assert(mValue < PlusInfinity, "Search-relative Value too Large");
            nAdjusted -= wSearchMoves;

            if (nAdjusted < MateMin) {
              Debug.Assert(MateMin <= nAdjusted, "Search-relative MateRange Underflow [Plus]");
              nAdjusted = MateMin;      //[Safe]Floor ensures a Mate value
            }
          }
          else if (mValue <= -MateMin) {// Mate [Minus]
            Debug.Assert(MinusInfinity < mValue, "Search-relative Value too Small");
            nAdjusted += wSearchMoves;

            if (-MateMin < nAdjusted) {
              Debug.Assert(nAdjusted <= -MateMin, "Search-relative MateRange Underflow [Minus]");
              nAdjusted = -MateMin;     //[Safe]Floor ensures a Mate value
            }
          }
        }
#if TraceInfinity
        else {
          Trace.Assert(MinusInfinity < mValue, "Search-relative Value = MinusInfinity");
          Trace.Assert(mValue < PlusInfinity, "Search-relative Value = PlusInfinity");
        }
#endif
      }

      return (Eval)nAdjusted;
    }
    #endregion
  }
}
