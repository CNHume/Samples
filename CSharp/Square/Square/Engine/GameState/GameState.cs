﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-06-22 CNHume]Created Class
//
// Conditionals:
//
#define QuiescentTryXP
#define AddBestMoves
#define HalfDeBruijn
//#define Magic
//#define LateMoveReduction
//#define KillerCompositionHash
#define MateThreat
#define SingularExtension
#define TotalMoves
#define TotalPVS
#define TotalEarlyMoves
#define ShowDetails
#define ShowEvals
#define CountNodes
#define CountCXP
#define CountPXP
#define CountQXP
#define CountXP
#define CountXPM
#define CountEvalTypes                  // for displayEvalTypeCounts()
//#define CountCapturedPiece
#define CountEarlyMoves
#define CountPVDoubles
#define DisplayRate                     //[Test]heartbeat()
//#define Controlled
#define Mobility
//#define SwapOn
#define DisplayOptions
#define DisplayPosition
//#define DisplayPositionPool
#define GetSmart
#define UseKillers
#define UseMoveSort
#define LazyMoveSort
//#define QuietCheck
//#define QuietMate
//#define TestCornerCP
//#define ThreadSafeTank
//#define TestLerp
//#define MaterialBalance
//#define XPHash128
//#define QXPHash128

namespace Engine {
  using Command;                        // For Scanner

  using Exceptions;

  using MoveOrder;                      // For MoveBottle

  using Resource;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Text;

  using Test;

  using static Board;
  using static Position;
  using static System.String;

  //
  // Type Aliases:
  //
  using Depth = System.UInt16;
  using Ply = System.UInt16;

  partial class GameState : IDisposable {
    #region Constants
    public const Depth wDepthMax = 48;  // Used by Predict()
    internal const Ply wPlyHistory = 64;
    #endregion

    #region Enumerations
    public enum SearchMode : byte { BestMove, Perft };
    #endregion

    #region Constructors
    public GameState(ICommand Command) {
      if (Command is null)
        throw new ArgumentNullException("Command");

      this.Command = Command;

      //
      //[ToDo]Allocate a separate Pool of positions for each thread:
      //
      PositionPool = new Pool<Position>();

      Bound = new SearchBound();
      Rule = new CastleRule();
      Case = new PerfCase();

      newBestMoves(wDepthMax);
      newTimers();
      SeededRandom = new Random();      // Variable seed based on Environment.TickCount

      loadEndgameValue();
      loadExtensionLimit();
#if KillerCompositionHash
      var uBottleLength = 8191U;
#else
      var uBottleLength = wPlyHistory;
#endif
#if UseKillers
      Bottle = new MoveBottle(uBottleLength);
#endif                                  // UseKillers
      wireControls();

      newNodeDelta(wDepthMax);
      newEarlyMoveCounts(wPlyHistory);
      newPVDoubleCounts(wPlyHistory);
#if MaterialBalance
      newCXPMemo(uDefaultComposition2);
#else
      newCXPMemo(uDefaultCompositions);
#endif
      newPXPMemo(uDefaultPawnPositions);

      //clear();
    }

    static GameState() {
      Extensions = (SearchExtensions[])Enum.GetValues(typeof(SearchExtensions));
    }

    ~GameState() {
      Dispose(false);
    }
    #endregion

    #region Init Methods
    protected void wireControls() {
      Opponent = default;

      //
      // Wire up the Event Handlers to listen for UCI Option changes:
      //
      wireClearHash();
      wireMultiPV();                    // Step 6/6: Wireup
      wireQXP();
      wireXP();
      wireXPM();
      wireExpectedMovesToGo();
      wireContempt();
      wireLate();
      wireChecks();
      wireThreat();
      wireSingular();
      wireAspiration();
      wireFlip();
      wireFutility();
      wireNullPrune();
      wireOccam();
      wirePure();
      wireHeartbeat();
      wireHeartbeatMS();
      wirePonder();
      wireAnalyseMode();
      wireShowingLine();
      wireOpponent();
      wireLogLevel();
      wireLogPath();
      wireLanguage();
    }

    private void newBestMoves(Int32 nCapacity) {
      BestMoves = new List<Move>(nCapacity);
    }

    private void newTimers() {
      SearchTimer = new Stopwatch();
      IterationTimer = new Stopwatch();
    }

    public void ClearSearchCounts() {
#if UseKillers
      Bottle.Clear();
#endif                                  // UseKillers
      XPTank.Counts.Clear();
      QXPTank.Counts.Clear();
      PXPMemo.Counts.Clear();
      CXPMemo.Counts.Clear();

      NodeTotal = PseudoMoveTotal =
        RepetitionPlies = RepetitionSearches =
        PinSkipTotal = QuietSkipTotal =
#if CountCapturedPiece
        CapturedPieceTotal =
#endif
        NullMoveTotal = NullMovePruneTotal =
        DeltaPruneTotal = FutilePruneTotal = OccamPruneTotal =
        CheckExtCount = ThreatExtCount = SingularExtCount =
        DrawTotal = MateTotal =
        WhiteSearchedPositionCount = WhiteEarlyMoveTotal =
        BlackSearchedPositionCount = BlackEarlyMoveTotal =
        PVDoubleTotal = PVSimpleTotal = PVSingleTotal = ZWSimpleTotal =
        ReducedTotal = TotalEvals = FullEvals =
        LowerCount = UpperCount = ExactCount =
#if QuiescentTryXP
        XPGetHitsQxnt = XPGetReadsQxnt =
#endif
        LegalMoves = LegalMovesQxnt = IllegalMoves = IllegalMovesQxnt = 0L;

      clearNodeDelta();                 // See DisplayPrediction
      clearEarlyMoveCounts();           // Conditional
      clearPVDoubleCounts();            // Conditional
    }

    public Position Push(Position parent) {
      var child = PositionPool.Push();
      child.Parent = parent;             //[Init]
      child.State = this;
      child.Clear();
      return child;
    }

    public void Pop(ref Position child) {
      child.Parent = default;
      PositionPool.Pop(ref child);
    }

    public void Unmove() {
      var parent = MovePosition.Parent;
      Pop(ref MovePosition);
      MovePosition = parent;
    }

    protected void unwindPositions() {
      while (MovePosition is not null)
        Unmove();
      RootPosition = default;
    }

    public void Clear() {               // Called by UCI.newGame()
      unwindPositions();
      MovePosition = default;
      ClearSearchCounts();              //[Init]Normally called by Position.start()

      if (Rule is null)
        throw new BoardException("Null Castle Instance");
      else
        Rule.Clear();
    }

    public static void SetLanguage(String sLanguage) {
      Language = sLanguage;
      SetPieceSymbols(Language);
    }
    #endregion

    #region Interface Methods
    void IDisposable.Dispose() {
      Close();
    }

    private void Close() {
      Dispose(true);
      GC.SuppressFinalize(this);
    }

    protected virtual void Dispose(Boolean bDisposeManaged) {
      if (disposed) return;

      if (bDisposeManaged) {
        disposeTask();
      }

      disposed = true;
    }
    #endregion

    #region Banner Methods
    [Conditional("DisplayOptions")]
    protected void appendOptions(StringBuilder sb) {
      // x64 from 10% to 20% faster than x86 on a Dell i7-4702HQ at 2.2 GHz
      sb.Append(Environment.Is64BitProcess ? " x64" : " x86");

      var formatXPM = " w {0}Mx{1} XPM";
#if XPHash128
      formatXPM += "128";
#endif
      sb.AppendFormat(formatXPM,
                      XPMTank.LookupLength >> 20,
                      XPMTank.LookupBuckets);

      var formatXP = " w {0}Mx{1} XP";
#if XPHash128
      formatXP += "128";
#endif
      sb.AppendFormat(formatXP,
                      XPTank.LookupLength >> 20,
                      XPTank.LookupBuckets);

      var formatQXP = " w {0}Mx{1} QXP";
#if QXPHash128
      formatQXP += "128";
#endif
      sb.AppendFormat(formatQXP,
                      QXPTank.LookupLength >> 20,
                      QXPTank.LookupBuckets);
#if QuiescentTryXP
      sb.Append(" TryXP");
#endif
#if ThreadSafeTank
      sb.Append(" Safe");
#endif
#if GetSmart
      sb.Append(" Smart");
#endif
#if UseMoveSort
#if LazyMoveSort
      sb.Append(" Lazy");
#else
      sb.Append(" Sort");
#endif
#endif                                  // UseMoveSort
#if !AddBestMoves
      sb.Append(" sans BestMoves");
#endif
#if !HalfDeBruijn
      sb.Append(" FullDeBruijn");
#endif
#if Magic
      sb.Append(" Magic");
#endif
#if Controlled
      sb.Append(" Controlled");
#endif
#if Mobility
      sb.Append(" Mobility");
#endif
#if SwapOn
      sb.Append(" SwapOn");
#endif
#if QuietCheck
      sb.Append(" QuietCheck");
#elif QuietMate
      sb.Append(" QuietMate");
#endif
#if TestCornerCP
      sb.AppendEvalInfo(mKBNMateCornerWeight);
      sb.Append(" CornerWeight");
#endif
      if (IsOccam)
        sb.Append(" Occam");

      if (IsAspiration)
        sb.Append(" Aspiration");

      if (!IsFutility)
        sb.Append(" sans Futility");
      else if (FutilityMargin.Length != 1)
        sb.AppendFormat(" {0} Futility", FutilityMargin.Length);

      foreach (var extension in Extensions) {
        var vLimit = getNibble(ExtensionLimit, (Int32)extension);
#if !LateMoveReduction
        if (extension == SearchExtensions.Late)
          continue;                     //[Disabled]
#endif
        if (extension == SearchExtensions.Threat) {
#if MateThreat
          if (vLimit == 0) {
#else
          if (true) {
#endif
            sb.Append(" No Threats");
            continue;                   //[Disabled]
          }
        }
#if !SingularExtension
        if (extension == SearchExtensions.Singular)
          continue;                     //[Disabled]
#endif
        if (vLimit > 0) {
          sb.AppendFormat(" {0} {1}", vLimit, extension);

          if (vLimit != 1)              // Plural
            sb.Append("s");
        }
      }

      sb.AppendFormat(" {0} PVSMin", wPVSDepthMin);

      if (IsNullPrune) {
        sb.AppendFormat(" {0} ReducedMin", wReducedDepthMin);
#if TestLerp
        sb.AppendFormat(" {0} LerpMax", wLerpDepthMax);
#endif
      }
#if MateThreat
      if (getNibble(ExtensionLimit, vThreat) > 0)
        sb.AppendFormat(" {0} ThreatMin", wThreatDepthMin);
#endif
      if (MultiPVLength > 1)
        sb.AppendFormat(" {0} MultiPV", MultiPVLength);

      if (MoveBottle.nKillers != 1)
        sb.AppendFormat(" {0} Killers", MoveBottle.nKillers);
    }

    private void herald(DateTime dtStarted, String sName) {
      var sb = new StringBuilder();
      sb.AppendLine();                  // Following UCI prompt
      sb.AppendFormat("{0:yyyy-MM-dd}", dtStarted);

      if (!IsNullOrEmpty(sName)) {
        sb.Append(sSpace);
        sb.Append(sName);
      }

      Bound.AppendBounds(sb);
      appendOptions(sb);
#if DisplayPosition
      sb.AppendLine();
      MovePosition.Display(sb);
#endif
      sb.FlushLine();
    }

    protected void displayCounts(SearchMode mode, Double dElapsedMS) {
      //
      // All of the following display methods are Conditional
      //
      displayNodeCounts(dElapsedMS);
      displayPseudoMoveTotals();

      //
      // None of the following are counted in SearchMode.Perft:
      //
      if (mode == SearchMode.BestMove) {
        displayPVSTotals();
        displayEarlyMoveTotals();
        displayDetails();
        displayEvals();
        displayCXP();
        displayPXP();
        displayXPM();
        displayQXP();
        displayXP();
        displayEvalTypeCounts();
        displayPositionPool();
      }
    }
    #endregion
  }
}
