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
#define EvalBishopPair
#define EvalWrongBishop
#define EvalOutsideSquare
#define EvalKQvKPDistance
#define EvalKBNvKMateCorner
#define EvalRookBehindPasser
#define EvalInsufficient
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

    #region Evaluation Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Boolean isEndgame(Eval mStaticTotal) {
      return mStaticTotal <= State!.EndgameValue;       // 22.25
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
      GameState.AtomicIncrement(ref State!.DrawTotal);
      var mDrawValue = (Eval)(-State!.Contempt);        // Strength advantage of White over Black
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
      var compBlack = State!.GetCX2(this, blackSide.PieceHash, wBlackCounts, blackSide);
      var compWhite = State!.GetCX2(this, whiteSide.PieceHash, wWhiteCounts, whiteSide);

      setEndGameFlags();                // Composition2 values do not depend on GameFlags.EndGame

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
      var comp = State!.GetCXP(this, uMemoHash, wBlackCounts, wWhiteCounts, blackSide, whiteSide);
#endif
      return ((Eval)comp.Delta, (Eval)comp.Total);
    }

    protected MemoHashcode compositionHash(Boolean bWhiteHash) {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      if (bWhiteHash)
        return (MemoHashcode)blackSide.PieceHash << nHashPieceBits | (MemoHashcode)whiteSide.PieceHash;
      else
        return (MemoHashcode)whiteSide.PieceHash << nHashPieceBits | (MemoHashcode)blackSide.PieceHash;
    }
#endif                                  // MaterialBalance
    private void clrEval() {
      StaticDelta = Position.EvalUndefined;
      StaticTotal = Position.EvalUndefined;
    }

    protected Eval staticEval(out PawnPosition? pp) {   //[New]~9.666 MHz vs ~13.333 MHz w/o EvalRookBehindPasser
      pp = default;
      if (IsInsufficient())
        return contempt();

      GameState.AtomicIncrement(ref State!.TotalEvals); // vs. FullEvaluations

      setEndGameFlags();

      if (EvalUndefined < StaticDelta)
        return StaticDelta;

      //
      // Retrieve material balance from the Composition:
      //
      (Eval mDelta, Eval mTotal) = getValue();

      if (Pawn != 0) {                  // Else PawnHash == default(Hashcode)
        pp = State!.GetPXP(this);

        mDelta += pp.Delta;
        mTotal += pp.Total;
#if EvalWrongBishop
        var blackSide = Side[Black];
        var whiteSide = Side[White];

        var fBlackSide = blackSide.FlagsSide;
        var fWhiteSide = whiteSide.FlagsSide;

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
      GameState.AtomicIncrement(ref State!.FullEvals);

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
          if (bDefault) pp = State!.GetPXP(this);
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
        BuildAtxTo(RankPiece);
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
    private Eval eval() {
      var bWTM = WTM();
      var mValue = IsDraw() || IsStalemate() ? contempt() : fullEval();
      return ReflectValue(bWTM, mValue);
    }

    private Eval final() {
      //
      // The game is over if the side to move has no move.
      // If the King is in Check, this is a Checkmate and
      // the lowest possible evaluation is given.
      //
      if (InCheck()) {
        GameState.AtomicIncrement(ref State!.MateTotal);
        return debitMate(-MateMax, SearchPly);
      }

      //
      // If the King is not in Check, this is a Stalemate
      // and an equal evaluation is returned.
      //
      var bWTM = WTM();
      var mValue = contempt();
      return ReflectValue(bWTM, mValue);
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

    private Eval standpatval(Eval mValueFound, EvalType etFound) {
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
      mValue = ReflectValue(bWTM, mValue);
      return boundValue(mValue, mValueFound, etFound);
    }

    private Eval pruneval(Draft wPruneDraft, Eval mAlpha, Eval mBeta, Eval mValueFound, EvalType etFound) {
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
    #endregion                          // Evaluation Methods

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
    public static Eval ReflectValue(Boolean bWTM, Eval mValue) {
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
    #endregion                          // IBV

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
    #endregion                          // Linear Interpolation

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
    #endregion                          // Piece Evaluation

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
    private static Eval creditMate(Eval mValue, Ply wSearchPlies) {
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
          var wSearchMoves = MoveDelta(wSearchPlies);
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

    private static Eval debitMate(Eval mValue, Ply wSearchPlies) {
      var nAdjusted = (Int32)mValue;
      if (EvalUndefined < mValue) {     //[Safe]
        if (MinusInfinity < mValue && mValue < PlusInfinity) {
          var wSearchMoves = MoveDelta(wSearchPlies);
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
    #endregion                          // Search vs Position Relative Mate Evaluation
  }
}
