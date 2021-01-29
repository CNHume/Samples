//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-06-24 CNHume]Created File
//
// Conditionals:
//
//#define DualModel
//#define TestRegression
//#define AtomicMethods                 // Costs ~1.5%
#define QuiescentTryXP
//#define DisplayActivePositions
#define GraphEarlyMove
#define GraphPVDouble
//#define MaterialBalance
//#define CountCapturedPiece

namespace Engine {
  using Cache;                          // For ProbeCounter
  using static Logging.Logger;
  using Exceptions;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using static System.Math;
  using static System.String;
  using System.Threading;

  //
  // Type Aliases:
  //
  using Depth = System.UInt16;
  using PlyDepth = System.Byte;
  using Ply = System.UInt16;

  partial class GameState {
    #region Atomic Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int64 AtomicIncrement(ref Int64 location) {
#if AtomicMethods
      return Interlocked.Increment(ref location);
#else
      return ++location;
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int64 AtomicAdd(ref Int64 location, Int64 value) {
#if AtomicMethods
      return Interlocked.Add(ref location, value);
#else
      return location += value;
#endif
    }

    // Increment # of Pseudo Moves by # Generated
    public Int32 IncPseudoMoveTotal(Int32 nCount) {
      AtomicAdd(ref PseudoMoveTotal, nCount);
      return nCount;
    }

    public void AddEarlyTotal(Boolean bWTM, Int32 nEarly) {
      if (bWTM) {
        AtomicIncrement(ref WhiteSearchedPositionCount);
        AtomicAdd(ref WhiteEarlyMoveTotal, nEarly);
      }
      else {
        AtomicIncrement(ref BlackSearchedPositionCount);
        AtomicAdd(ref BlackEarlyMoveTotal, nEarly);
      }
    }
    #endregion

    #region Index Methods
    public Ply modPly(Ply wPly) {
      var nPly = (MovePly + wPly) % wPlyHistory;

      if (nPly < 0) nPly += wPlyHistory;

      return (Ply)nPly;
    }
    #endregion

    #region Methods
    [Conditional("CountEarlyMoves")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void IncEarlyMoveCount(Ply wSearchPlies) {
      var wCountPly = modPly(wSearchPlies);
      AtomicIncrement(ref EarlyMoveCount[wCountPly]);

      //
      //[ToDo]The following Min and Max Ply updates are not thread safe:
      //
      if (wSearchPlies < EarlyMoveMinPly)
        EarlyMoveMinPly = wSearchPlies;

      if (wSearchPlies > EarlyMoveMaxPly)
        EarlyMoveMaxPly = wSearchPlies;
    }

    [Conditional("CountPVDoubles")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void IncPVDoubleCount(Ply wSearchPlies) {
      var wCountPly = modPly(wSearchPlies);
      AtomicIncrement(ref PVDoubleCount[wCountPly]);

      //
      //[ToDo]The following Min and Max Ply updates are not thread safe:
      //
      if (wSearchPlies < PVDoubleMinPly)
        PVDoubleMinPly = wSearchPlies;

      if (wSearchPlies > PVDoubleMaxPly)
        PVDoubleMaxPly = wSearchPlies;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void IncMove(Boolean bLegal, Boolean bQxnt) {
      if (bLegal) {
        if (bQxnt)
          AtomicIncrement(ref LegalMovesQxnt);
        else
          AtomicIncrement(ref LegalMoves);
      }
      else if (bQxnt)
        AtomicIncrement(ref IllegalMovesQxnt);
      else
        AtomicIncrement(ref IllegalMoves);
    }

    [Conditional("CountEvalTypes")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void IncEvalType(EvalType et) {
      switch (et) {
      case EvalType.Upper:
        AtomicIncrement(ref UpperCount);
        break;

      case EvalType.Exact:
        AtomicIncrement(ref ExactCount);
        break;

      case EvalType.Lower:
        AtomicIncrement(ref LowerCount);
        break;

      default:
        throw new PositionException("Unexpected EvalType");
      }
    }

    [Conditional("QuiescentTryXP")]
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void IncQxnt(Boolean bFoundValue) {
      //
      // Maintain separate accounting for Quiescent XP Probes,
      // i.e., for Qxnt XPGet[Reads|Hits]:
      //
      if (bFoundValue) AtomicIncrement(ref XPGetHitsQxnt);
      AtomicIncrement(ref XPGetReadsQxnt);
    }

    [Conditional("CountNodes")]
    protected void displayNodeCounts(Double dElapsedMS) {
      Debug.Assert(NodeTotal == MoveTotal + NullMoveTotal, "Inconsistent Node Total");

      if (dElapsedMS == 0)
        LogInfo(Level.data, "Searched a total of {0:n0} nodes in {1:0.0##} sec",
                NodeTotal, dElapsedMS / 1000);
      else {
        var dRate = NodeTotal / dElapsedMS;
        LogInfo(Level.data,
                "Searched a total of {0:n0} nodes in {1:0.0##} sec, {2:0.0##} KHz",
                NodeTotal, dElapsedMS / 1000, dRate);
      }
#if DisplayActivePositions
      LogInfo(Level.data, "Peak Positions = {0}, Currently Active = {1}",
              ActivePositionsPeak, ActivePositions);
#endif
    }

    [Conditional("TotalMoves")]
    protected void displayPseudoMoveTotals() {
      LogInfoNewLine(Level.data);
      LogInfo(Level.data, "Pseudo Moves = {0:n0}; Pins Skipped = {1:n0}",
              PseudoMoveTotal, PinSkipTotal);

      if (MoveTotal != 0) {
        if (MoveTotalQxnt != 0) {       // Qxnt Moves do not occur in SearchMode.Perft
          var dQxntPercent = 100.0 * MoveTotalQxnt / MoveTotal;
          LogInfo(Level.data,
                  "Qxnt Moves = {0:n0}; Move Total = {1:n0}; Qxnt Moves/Move Total = {2:n1}%",
                  MoveTotalQxnt, MoveTotal, dQxntPercent);
        }

        var dIllegalPercent = 100.0 * IllegalMoveTotal / MoveTotal;
        LogInfo(Level.data, "Illegal Moves = {0:n0}; Illegal Moves/Move Total = {1:n1}%",
                IllegalMoveTotal, dIllegalPercent);
      }
#if CountCapturedPiece
      if (PseudoMoveTotal == 0)
        LogInfo(Level.data, "getPiece() Calls = {0:n0}", CapturedPieceTotal);
      else {
        var dCapturedPiecePercent = 100.0 * CapturedPieceTotal / PseudoMoveTotal;
        LogInfo(Level.data, "getPiece() Calls = {0:n0}; getPiece() Calls/Pseudo Moves = {1:n1}%",
                CapturedPieceTotal, dCapturedPiecePercent);
      }
#endif
    }

    protected static String bar(Double percent, Int32 scale = 100) {
      var v2 = (Byte)Round(2 * scale * percent / 100.0);
      var s = Empty.PadRight(v2 / 2, '@');
      var bHalf = (v2 & 1) != 0;
      return bHalf ? s + "&" : s;
    }

    protected void displayPVDoubleHistogram() {
      if (PVDoubleTotal <= 0) return;

      LogInfoNewLine(Level.data);
      for (var wSearchPlies = PVDoubleMinPly; wSearchPlies <= PVDoubleMaxPly; wSearchPlies++) {
        var wCountPly = modPly(wSearchPlies);
        var qResearches = PVDoubleCount[wCountPly];
        var dPercent = 100.0 * qResearches / PVDoubleTotal;
#if GraphPVDouble
        LogInfo(Level.data, "Doubles[{0,2:n0}] ={1,7:n3}% {2}",
                wSearchPlies, dPercent, bar(dPercent));
#else
        LogInfo(Level.data, "Doubles[{0,2:n0}] ={1,7:n3}% = {2:n0}",
                wSearchPlies, dPercent, qResearches);
#endif
      }
    }

    [Conditional("TotalPVS")]
    protected void displayPVSTotals() {
      LogInfo(Level.data, "ZW Simple = {0:n0}; PV Simple = {1:n0}",
              ZWSimpleTotal, PVSimpleTotal);
      LogInfo(Level.data, "PV Singles = {0:n0}; PV Doubles = {1:n0}",
              PVSingleTotal, PVDoubleTotal);

      displayPVDoubleHistogram();
    }

    protected void displayEarlyMoveHistogram() {
      var lEarlyMoveTotal = WhiteEarlyMoveTotal + BlackEarlyMoveTotal;
      if (lEarlyMoveTotal <= 0) return;

      LogInfoNewLine(Level.data);
      for (var wSearchPlies = EarlyMoveMinPly; wSearchPlies <= EarlyMoveMaxPly; wSearchPlies++) {
        var wCountPly = modPly(wSearchPlies);
        var qResearches = EarlyMoveCount[wCountPly];
        var dPercent = 100.0 * qResearches / lEarlyMoveTotal;
#if GraphEarlyMove
        LogInfo(Level.data, "EarlyMoves[{0,2:n0}] ={1,7:n3}% {2}",
                wSearchPlies, dPercent, bar(dPercent));
#else
        LogInfo(Level.data, "EarlyMoves[{0,2:n0}] ={1,7:n3}% = {2:n0}",
                wSearchPlies, dPercent, qResearches);
#endif
      }
    }

    private static void displayEarlyMoveCounts(String sColor, Int64 lEarlyMoveTotal, Int64 lSearchedPositionCount) {
      //[Note]Move Ordering is applied in a "Searched Position", not during a quiescence search.
      if (lSearchedPositionCount == 0)
        LogInfo(Level.data, "{0} Early Moves = {1:n0}", sColor, lEarlyMoveTotal);
      else {
        var dEarlyMovesPerPosition = (Double)lEarlyMoveTotal / lSearchedPositionCount;
        LogInfo(Level.data,
                "{0} Early Moves = {1:n0}; Searched Positions = {2:n0}; {0} Early Moves/Searched Position = {3:n2}",
                sColor, lEarlyMoveTotal, lSearchedPositionCount, dEarlyMovesPerPosition);
      }
    }

    [Conditional("TotalEarlyMoves")]
    protected void displayEarlyMoveTotals() {
      LogInfoNewLine(Level.data);
      displayEarlyMoveCounts("White", WhiteEarlyMoveTotal, WhiteSearchedPositionCount);
      displayEarlyMoveCounts("Black", BlackEarlyMoveTotal, BlackSearchedPositionCount);

      displayEarlyMoveHistogram();
    }

    [Conditional("ShowDetails")]
    protected void displayDetails() {
      LogInfoNewLine(Level.data);
      if (RepetitionSearches > 0) {
        var dMovesPerRepetition = (Double)RepetitionPlies / 2 / RepetitionSearches;
        LogInfo(Level.data, "Moves/Repetition = {0:n2}", dMovesPerRepetition);
      }

      LogInfo(Level.data, "Draws Found = {0:n0}; Mates Found = {1:n0}",
              DrawTotal, MateTotal);
      LogInfo(Level.data, "Extensions: Check = {0:n0}; Threat = {1:n0}; Singular = {2:n0}",
              CheckExtCount, ThreatExtCount, SingularExtCount);
      LogInfo(Level.data, "Reduced = {0:n0}; Quiet Skipped = {1:n0}",
              ReducedTotal, QuietSkipTotal);
      LogInfo(Level.data, "Pruning: Delta = {0:n0}; Futile = {1:n0}; Occam = {2:n0}",
              DeltaPruneTotal, FutilePruneTotal, OccamPruneTotal);

      if (NullMoveTotal != 0) {
        var dNullMovePrunePercent = 100.0 * NullMovePruneTotal / NullMoveTotal;
        LogInfo(Level.data, "Pruned {0:n0} [{1:n1}%] of {2:n0} Null Moves",
                NullMovePruneTotal, dNullMovePrunePercent, NullMoveTotal);
      }
    }

    [Conditional("ShowEvals")]
    private void displayEvals() {
      LogInfoNewLine(Level.data);

      if (LegalMoveTotal == 0) {
        var qStaticEvals = TotalEvals - FullEvals;
        LogInfo(Level.data, "Full Evals {0:n0}; Legal Moves = {1:n0}; Stat Evals = {2:n0}",
                FullEvals, LegalMoveTotal, qStaticEvals);
      }
      else {
        var dFullPercent = 100.0 * FullEvals / LegalMoveTotal;
        LogInfo(Level.data,
                "Full Evals = {0:n0}; Legal Moves = {1:n0}; Full Evals/Legal Moves = {2:n1}%",
                FullEvals, LegalMoveTotal, dFullPercent);

        var qStaticEvals = TotalEvals - FullEvals;
        var dFastPercent = 100.0 * qStaticEvals / LegalMoveTotal;
        LogInfo(Level.data, "Stat Evals = {0:n0}; Stat Evals/Legal Moves = {1:n1}%",
                qStaticEvals, dFastPercent);
      }
    }

    [Conditional("CountCXP")]
    protected void displayCXP() {
      if (CXPMemo is null)
        return;

      var counts = CXPMemo.Counts;
      if (counts != default(SimpleCounter)) {
        LogInfoNewLine(Level.data);
        var uCapacity = CXPMemo.LookupLength;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountPXP")]
    protected void displayPXP() {
      if (PXPMemo is null)
        return;

      var counts = PXPMemo.Counts;
      if (counts != default(SimpleCounter)) {
        LogInfoNewLine(Level.data);
        var uCapacity = PXPMemo.LookupLength;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountQXP")]
    protected void displayQXP() {
      if (QXPTank is null)
        return;                         //[Safe]

      var counts = QXPTank.Counts;
      LogInfoNewLine(Level.data);
      if (counts != default(ProbeCounter)) {
        var uCapacity = QXPTank.LookupLength * QXPTank.LookupBuckets;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountXP")]
    protected void displayXP() {
      if (XPTank is null)
        return;                         //[Safe]

      var counts = XPTank.Counts;
      LogInfoNewLine(Level.data);

      //
      //[Note]Qxnt counts track Quiescent XP Probes made by Quiet().
      // A special Display() overload subtracts these from the true
      // XPTank.Counts reported below.
      //
      displayQxntGets();                //[Conditional]See below

      if (counts != default(ProbeCounter)) {
        var uCapacity = XPTank.LookupLength * XPTank.LookupBuckets;
        counts.Display(uCapacity, XPGetReadsQxnt, XPGetHitsQxnt);
      }
    }

    [Conditional("QuiescentTryXP")]
    private void displayQxntGets() {
      if (XPGetReadsQxnt == 0)
        LogInfo(Level.data, "Qxnt XP Get Hits = {0:n0}; Qxnt XP Get Reads = {1:n0}",
                XPGetHitsQxnt, XPGetReadsQxnt);
      else {
        var dXPGetHitsQxntPercent = 100.0 * XPGetHitsQxnt / XPGetReadsQxnt;
        LogInfo(Level.data,
                "Qxnt XP Get Hits = {0:n0}; Qxnt XP Get Reads = {1:n0}; Qxnt XP Get Hits/Reads = {2:n1}%",
                XPGetHitsQxnt, XPGetReadsQxnt, dXPGetHitsQxntPercent);
      }
    }

    [Conditional("CountXPM")]
    protected void displayXPM() {
      if (XPMTank is null)
        return;                         //[Safe]

      var counts = XPMTank.Counts;
      LogInfoNewLine(Level.data);
      if (counts != default(ProbeCounter)) {
        var uCapacity = XPMTank.LookupLength * XPMTank.LookupBuckets;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountEvalTypes")]
    protected void displayEvalTypeCounts() {
      //[Note]The EvalType Counts should add up to SetReads
      LogInfo(Level.data,
              "{0} Counts: {1} = {2:n0}; {3} = {4:n0}; {5} = {6:n0}",
              typeof(EvalType).Name,
              EvalType.Upper, UpperCount,
              EvalType.Exact, ExactCount,
              EvalType.Lower, LowerCount);
    }

    [Conditional("DisplayPositionPool")]
    protected void displayPositionPool() {
      PositionPool.DisplayActive();
    }

    [Conditional("DisplayRate")]
    public static void displayRate(Double dElapsedMS, UInt64 qNodeDelta) {
      if (dElapsedMS == 0) {
        LogInfo(Level.data, "Searched {0:n0} nodes in {1:0.0##} sec",
                qNodeDelta, dElapsedMS / 1000);
      }
      else {
        var dRate = qNodeDelta / dElapsedMS;
        LogInfo(Level.data,
                "Searched {0:n0} nodes in {1:0.0##} sec, {2:0.0##} KHz",
                qNodeDelta, dElapsedMS / 1000, dRate);
      }
    }

    [Conditional("DisplayPrediction")]
    public static void DisplayPrediction(Double dElapsedMS,
                                         UInt64 qNodeDelta,
                                         UInt64 qPredicted1,
                                         UInt64 qPredicted2) {
      if (dElapsedMS == 0) {
        // Current Iteration
        if (qPredicted1 > 0) {
          var dError = (Double)qPredicted1 / qNodeDelta - 1;
          LogInfo(Level.data, "Predicted {0:n0} moves, Relative Error = {1:n1}%",
                  qPredicted1, 100 * dError);
        }

        // Next Iteration
        if (qPredicted2 > 0) {
          LogInfo(Level.data, "Predicting {0:n0} moves", qPredicted2);
        }
      }
      else {
        // Current Iteration
        if (qPredicted1 > 0) {
          var dError = (Double)qPredicted1 / qNodeDelta - 1;
          LogInfo(Level.data, "Predicted {0:n0} moves, Relative Error = {1:n1}%",
                  qPredicted1, 100 * dError);
        }

        // Next Iteration
        var dRate = qNodeDelta / dElapsedMS;
        if (qPredicted2 > 0) {
          var dPredictedSec = qPredicted2 / dRate;
          LogInfo(Level.data, "Predicting {0:n0} moves in {1:0.0##} sec",
                  qPredicted2, dPredictedSec / 1000);
        }
      }
    }

    public UInt64 Predict(PlyDepth vPliesStart, PlyDepth vPlies, UInt64 qNodes) {
      const Int32 nModelSize = 3;
      var qPredicted = 0UL;             // Return Value

      //
      // Maintain the total # of moves searched at each ply: starting
      // with vPliesStart and continuing through the current vPlies.
      //
      NodeDelta[vPlies] = qNodes;       //[Test]
      NodeDeltaLog[vPlies] = Log((Double)qNodes);
#if DualModel
      var nPliesPrior = vPlies - 2 * (nModelSize - 1);
#else
      var nPliesPrior = vPlies - (nModelSize - 1);
#endif
      var nPliesModel = nPliesPrior;
      if (nPliesModel < vPliesStart) {
#if DualModel
        var nSamples2 = vPlies - vPliesStart & -2;
        nPliesModel = vPlies - nSamples2;
#else
        nPliesModel = vPliesStart;
#endif
      }
#if DualModel
      var nCount = (vPlies - nPliesModel) / 2 + 1;
#else
      var nCount = vPlies - nPliesModel + 1;
#endif
      if (nCount > 1) {
        var nCount1 = nCount - 1;

        //
        // n = # of terms
        // start = 1st term 
        // delta = diff between each term
        //
        // Sum = n * (2 * start + delta * (n - 1)) / 2
        // Avg = Sum / n = (2 * start + delta * (n - 1)) / 2
        //
#if DualModel
        var dXMean = (Double)(nPliesModel + nCount1);
#else
        var dXMean = (Double)(2 * nPliesModel + nCount1) / 2;
#endif
        var dYSum = 0.0;
        for (var n = 0; n < nCount; n++) {
#if DualModel
          var nPlies = 2 * n + nPliesModel;
#else
          var nPlies = n + nPliesModel;
#endif
          dYSum += NodeDeltaLog[nPlies];
        }

        var dYMean = dYSum / nCount;
        var dXSquareSum = 0.0;
        var dYSquareSum = 0.0;
        var dProductSum = 0.0;

        for (var n = 0; n < nCount; n++) {
#if DualModel
          var nPlies = 2 * n + nPliesModel;
#else
          var nPlies = n + nPliesModel;
#endif
          var dXDelta = nPlies - dXMean;
          var dYDelta = NodeDeltaLog[nPlies] - dYMean;

          dXSquareSum += dXDelta * dXDelta;
          dYSquareSum += dYDelta * dYDelta;
          dProductSum += dXDelta * dYDelta;
        }

        var dXVariance = dXSquareSum / nCount1;
        var dYVariance = dYSquareSum / nCount1;
        var dXDeviation = Sqrt(dXVariance);
        var dYDeviation = Sqrt(dYVariance);

        var dDenominator = nCount1 * dXDeviation * dYDeviation;
        var dCorrelation = dProductSum / dDenominator;
        var dSlope = dCorrelation * dYDeviation / dXDeviation;

        var dPredictedLog = dYMean + dSlope * (vPlies + 1 - dXMean);
        var dPredicted = Exp(dPredictedLog);
        qPredicted = (UInt64)(dPredicted + 0.5);
#if TestRegression
        for (var n = 0; n < nCount; n++) {
#if DualModel
          var nPlies = 2 * n + nPliesModel;
#else
          var nPlies = n + nPliesModel;
#endif
          var dModelLog = dSlope * (nPlies - dXMean) + dYMean;
          var dModel = Exp(dModelLog);
          var qModel = (UInt64)(dModel + 0.5);
          var qActual = NodeDelta[nPlies];
          var dError = dModel / qActual - 1;

          LogInfo(Level.data, "Modelled {0:n0} for {1:n0} Actual Moves, Relative Error = {2:n1}%",
                  qModel, qActual, 100 * dError);
        }
#endif
      }

      return qPredicted;
    }
    #endregion
  }
}
