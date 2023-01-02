//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2013-06-24 CNHume]Created File
//
// Conditionals:
//
//#define DualModel
//#define TestRegression
//#define AtomicMethods                 // Costs ~1.5%
#define QuiescentTryXP
#define GraphEarlyMove
#define GraphPVDouble
//#define MaterialBalance
//#define CountCapturedPiece

namespace Engine {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;               // For AtomicMethods

  using Cache;                          // For ProbeCounter

  using Exceptions;

  using static System.Math;
  using static System.String;
  using static Board;
  using static Cache.SimpleCounter;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Ply = UInt16;
  using PlyDepth = Byte;

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
    private void displayNodeCounts(Double dElapsedMS) {
      Debug.Assert(NodeTotal == MoveTotal + NullMoveTotal, "Inconsistent Node Total");

      if (dElapsedMS == 0)
        LogInfo(Level.data, $"Searched a total of {NodeTotal:n0} nodes in {dElapsedMS / 1000:0.0##} sec");
      else {
        var dRate = NodeTotal / dElapsedMS;
        LogInfo(Level.data,
                $"Searched a total of {NodeTotal:n0} nodes in {dElapsedMS / 1000:0.0##} sec, {dRate:0.0##} KHz");
      }
    }

    [Conditional("TotalMoves")]
    private void displayPseudoMoveTotals() {
      LogInfoNewLine(Level.data);
      LogInfo(Level.data, $"Pseudo Moves = {PseudoMoveTotal:n0}; Pins Skipped = {PinSkipTotal:n0}");

      if (MoveTotal != 0) {
        if (MoveTotalQxnt != 0) {       // Qxnt Moves do not occur in SearchMode.Perft
          var dQxntPercent = 100.0 * MoveTotalQxnt / MoveTotal;
          LogInfo(Level.data,
                  $"Qxnt Moves = {MoveTotalQxnt:n0}; Move Total = {MoveTotal:n0}; Qxnt Moves/Move Total = {dQxntPercent:n2}%");
        }

        var dIllegalPercent = 100.0 * IllegalMoveTotal / MoveTotal;
        LogInfo(Level.data,
                $"Illegal Moves = {IllegalMoveTotal:n0}; Illegal Moves/Move Total = {dIllegalPercent:n2}%");
      }
#if CountCapturedPiece
      if (PseudoMoveTotal == 0)
        LogInfo(Level.data, $"getPieceIndex() Calls = {CapturedPieceTotal:n0}");
      else {
        var dCapturedPiecePercent = 100.0 * CapturedPieceTotal / PseudoMoveTotal;
        LogInfo(Level.data,
                $"getPieceIndex() Calls = {CapturedPieceTotal:n0}; getPieceIndex() Calls/Pseudo Moves = {dCapturedPiecePercent:n2}%");
      }
#endif
    }

    private static String bar(Double percent, Int32 scale = 100) {
      var v2 = (Byte)Round(2 * scale * percent / 100.0);
      var s = Empty.PadRight(v2 / 2, '@');
      var bHalf = Board.IsOdd(v2);
      return bHalf ? s + "&" : s;
    }

    private void displayPVDoubleHistogram() {
      if (PVDoubleTotal <= 0) return;

      LogInfoNewLine(Level.data);
      for (var wSearchPlies = PVDoubleMinPly; wSearchPlies <= PVDoubleMaxPly; wSearchPlies++) {
        var wCountPly = modPly(wSearchPlies);
        var qResearches = PVDoubleCount[wCountPly];
        var dPercent = 100.0 * qResearches / PVDoubleTotal;
#if GraphPVDouble
        LogInfo(Level.data, $"Doubles[{wSearchPlies,2:n0}] ={dPercent,7:n3}% {bar(dPercent)}");
#else
        LogInfo(Level.data, $"Doubles[{wSearchPlies,2:n0}] ={dPercent,7:n3}% = {qResearches:n0}");
#endif
      }
    }

    [Conditional("TotalPVS")]
    private void displayPVSTotals() {
      LogInfo(Level.data, $"ZW Simple = {ZWSimpleTotal:n0}; PV Simple = {PVSimpleTotal:n0}");
      LogInfo(Level.data, $"PV Singles = {PVSingleTotal:n0}; PV Doubles = {PVDoubleTotal:n0}");

      displayPVDoubleHistogram();
    }

    private void displayEarlyMoveHistogram() {
      var lEarlyMoveTotal = WhiteEarlyMoveTotal + BlackEarlyMoveTotal;
      if (lEarlyMoveTotal <= 0) return;

      LogInfoNewLine(Level.data);
      for (var wSearchPlies = EarlyMoveMinPly; wSearchPlies <= EarlyMoveMaxPly; wSearchPlies++) {
        var wCountPly = modPly(wSearchPlies);
        var qResearches = EarlyMoveCount[wCountPly];
        var dPercent = 100.0 * qResearches / lEarlyMoveTotal;
#if GraphEarlyMove
        LogInfo(Level.data, $"EarlyMoves[{wSearchPlies,2:n0}] ={dPercent,7:n3}% {bar(dPercent)}");
#else
        LogInfo(Level.data, $"EarlyMoves[{wSearchPlies,2:n0}] ={dPercent,7:n3}% = {qResearches:n0}");
#endif
      }
    }

    private static void displayEarlyMoveCounts(SideName sideName, Int64 lEarlyMoveTotal, Int64 lSearchedPositionCount) {
      //[Note]Move Ordering is applied in a "Searched Position", not during a quiescence search.
      if (lSearchedPositionCount == 0)
        LogInfo(Level.data, $"{sideName} Early Moves = {lEarlyMoveTotal:n0}");
      else {
        var dEarlyMovesPerPosition = (Double)lEarlyMoveTotal / lSearchedPositionCount;
        LogInfo(Level.data,
                $"{sideName} Early Moves = {lEarlyMoveTotal:n0}; Searched Positions = {lSearchedPositionCount:n0}");
        LogInfo(Level.data, $"{sideName} Early Moves/Searched Position = {dEarlyMovesPerPosition:n2}");
      }
    }

    [Conditional("TotalEarlyMoves")]
    private void displayEarlyMoveTotals() {
      LogInfoNewLine(Level.data);
      displayEarlyMoveCounts(SideName.White, WhiteEarlyMoveTotal, WhiteSearchedPositionCount);
      displayEarlyMoveCounts(SideName.Black, BlackEarlyMoveTotal, BlackSearchedPositionCount);

      displayEarlyMoveHistogram();
    }

    [Conditional("ShowDetails")]
    private void displayDetails() {
      LogInfoNewLine(Level.data);
      if (RepetitionSearches > 0) {
        var dMovesPerRepetition = (Double)RepetitionPlies / 2 / RepetitionSearches;
        LogInfo(Level.data, $"Moves/Repetition = {dMovesPerRepetition:n2}");
      }

      LogInfo(Level.data, $"Draws Found = {DrawTotal:n0}; Mates Found = {MateTotal:n0}");
      LogInfo(Level.data,
              $"Extensions: Check = {CheckExtCount:n0}; Threat = {ThreatExtCount:n0}; Singular = {SingularExtCount:n0}");
      LogInfo(Level.data, $"Reduced = {ReducedTotal:n0}; Quiet Skipped = {QuietSkipTotal:n0}");
      LogInfo(Level.data,
              $"Pruning: Delta = {DeltaPruneTotal:n0}; Futile = {FutilePruneTotal:n0}; Occam = {OccamPruneTotal:n0}");

      if (NullMoveTotal != 0) {
        var dNullMovePrunePercent = 100.0 * NullMovePruneTotal / NullMoveTotal;
        LogInfo(Level.data,
                $"Pruned {NullMovePruneTotal:n0} [{dNullMovePrunePercent:n1}%] of {NullMoveTotal:n0} Null Moves");
      }
    }

    [Conditional("ShowEvals")]
    private void displayEvals() {
      LogInfoNewLine(Level.data);

      if (LegalMoveTotal == 0) {
        var qStaticEvals = TotalEvals - FullEvals;
        LogInfo(Level.data,
                $"Full Evals {FullEvals:n0}; Legal Moves = {LegalMoveTotal:n0}; Stat Evals = {qStaticEvals:n0}");
      }
      else {
        var dFullPercent = 100.0 * FullEvals / LegalMoveTotal;
        LogInfo(Level.data,
                $"Full Evals = {FullEvals:n0}; Legal Moves = {LegalMoveTotal:n0}; Full Evals/Legal Moves = {dFullPercent:n1}%");

        var qStaticEvals = TotalEvals - FullEvals;
        var dFastPercent = 100.0 * qStaticEvals / LegalMoveTotal;
        LogInfo(Level.data,
                $"Stat Evals = {qStaticEvals:n0}; Stat Evals/Legal Moves = {dFastPercent:n1}%");
      }
    }

    [Conditional("CountCXP")]
    private void displayCXP() {
      if (CXPMemo == null)
        return;

      var counts = CXPMemo.Counts;
      if (counts != default(SimpleCounter)) {
        LogInfoNewLine(Level.data);
        var uCapacity = CXPMemo.LookupLength;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountPXP")]
    private void displayPXP() {
      if (PXPMemo == null)
        return;

      var counts = PXPMemo.Counts;
      if (counts != default(SimpleCounter)) {
        LogInfoNewLine(Level.data);
        var uCapacity = PXPMemo.LookupLength;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountQXP")]
    private void displayQXP() {
      if (QXPTank == null)
        return;                         //[Safe]

      var counts = QXPTank.Counts;
      LogInfoNewLine(Level.data);
      if (counts != default(ProbeCounter)) {
        var uCapacity = QXPTank.LookupLength * QXPTank.LookupBuckets;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountXP")]
    private void displayXP() {
      if (XPTank == null)
        return;                         //[Safe]

      var counts = XPTank.Counts;
      LogInfoNewLine(Level.data);
#if QuiescentTryXP
      //
      //[Note]Qxnt counts track Quiescent XP Probes made by Quiet().
      // A special Display() overload subtracts these from the true
      // XPTank.Counts reported below.
      //
      DisplayHits("Qxnt XP", "Get", XPGetReadsQxnt, XPGetHitsQxnt);
#endif
      if (counts != default(ProbeCounter)) {
        var uCapacity = XPTank.LookupLength * XPTank.LookupBuckets;
        counts.Display(uCapacity, XPGetReadsQxnt, XPGetHitsQxnt);
      }
    }

    [Conditional("CountXPM")]
    private void displayXPM() {
      if (XPMTank == null)
        return;                         //[Safe]

      var counts = XPMTank.Counts;
      LogInfoNewLine(Level.data);
      if (counts != default(ProbeCounter)) {
        var uCapacity = XPMTank.LookupLength * XPMTank.LookupBuckets;
        counts.Display(uCapacity);
      }
    }

    [Conditional("CountEvalTypes")]
    private void displayEvalTypeCounts() {
      //[Note]The EvalType Counts should add up to SetReads
      LogInfo(Level.data,
              $"{EvalType.Upper} = {UpperCount:n0}; {EvalType.Exact} = {ExactCount:n0}; {EvalType.Lower} = {LowerCount:n0}");
    }

    [Conditional("DisplayPositionPool")]
    private void displayPositionPool() {
      PositionPool.DisplayActive();
    }

    [Conditional("DisplayRate")]
    public static void displayRate(Double dElapsedMS, UInt64 qNodeDelta) {
      if (dElapsedMS == 0) {
        LogInfo(Level.data, $"Searched {qNodeDelta:n0} nodes in {dElapsedMS / 1000:0.0##} sec");
      }
      else {
        var dRate = qNodeDelta / dElapsedMS;
        LogInfo(Level.data,
                $"Searched {qNodeDelta:n0} nodes in {dElapsedMS / 1000:0.0##} sec, {dRate:0.0##} KHz");
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
          LogInfo(Level.data, $"Predicted {qPredicted1:n0} moves, Relative Error = {100 * dError:n1}%");
        }

        // Next Iteration
        if (qPredicted2 > 0) {
          LogInfo(Level.data, $"Predicting {qPredicted2:n0} moves");
        }
      }
      else {
        // Current Iteration
        if (qPredicted1 > 0) {
          var dError = (Double)qPredicted1 / qNodeDelta - 1;
          LogInfo(Level.data, $"Predicted {qPredicted1:n0} moves, Relative Error = {100 * dError:n1}%");
        }

        // Next Iteration
        var dRate = qNodeDelta / dElapsedMS;
        if (qPredicted2 > 0) {
          var dPredictedSec = qPredicted2 / dRate;
          LogInfo(Level.data, $"Predicting {qPredicted2:n0} moves in {dPredictedSec / 1000:0.0##} sec");
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

          LogInfo(Level.data,
                  $"Modelled {qModel:n0} for {qActual:n0} Actual Moves, Relative Error = {100 * dError:n1}%");
        }
#endif
      }

      return qPredicted;
    }
    #endregion
  }
}
