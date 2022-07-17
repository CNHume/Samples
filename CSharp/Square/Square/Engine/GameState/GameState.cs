//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2013-06-22 CNHume]Created Class
//
// Conditionals:
//
#define ShowGC
#define QuiescentTryXP
#define AddBestMoves
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
#define QuietMate
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
  using System.Runtime;
  using System.Text;

  using Test;

  using static Board;
  using static Position;
  using static System.String;

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Ply = UInt16;

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
      IsChess960 = false;
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

    public Position Push(Position? parent) {
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
      if (MovePosition is null) return;
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
      MovePosition = new Position();
      ClearCastleRule(MovePosition.Side);
      ClearSearchCounts();              //[Init]Normally called by Position.start()
    }

    public void ClearCastleRule(BoardSide[] sides, Boolean isChess960 = false) {
      IsChess960 = isChess960;
      foreach (var side in sides)
        side.Rule.Clear();
    }

    public static void SetLanguage(String? sLanguage) {
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
#if XPHash128
      var sXPM = "XPM128";
      var sXP = "XP128";
#else
      var sXPM = "XPM";
      var sXP = "XP";
#endif
#if QXPHash128
      var sQXP = "QXP128";
#else
      var sQXP = "QXP";
#endif
      sb.AppendFormat($" w {XPMTank.LookupLength >> 20}Mx{XPMTank.LookupBuckets} {sXPM}");
      sb.AppendFormat($" w {XPTank.LookupLength >> 20}Mx{XPTank.LookupBuckets} {sXP}");
      sb.AppendFormat($" w {QXPTank.LookupLength >> 20}Mx{QXPTank.LookupBuckets} {sQXP}");
#if QuiescentTryXP
      sb.Append(" TryXP");
#endif
#if ShowGC
      var sServerGC = GCSettings.IsServerGC ? "server" : "workstation";
      var sLatency = GCSettings.LatencyMode;
      sb.AppendFormat($" {sServerGC} {sLatency}");
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
      sb.AppendTZCMode();
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
        sb.AppendFormat($" {FutilityMargin.Length} Futility");

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
          sb.AppendFormat($" {vLimit} {extension}");

          if (vLimit != 1)              // Plural
            sb.Append("s");
        }
      }

      sb.AppendFormat($" {wPVSDepthMin} PVSMin");

      if (IsNullPrune) {
        sb.AppendFormat($" {wReducedDepthMin} ReducedMin");
#if TestLerp
        sb.AppendFormat($" {wLerpDepthMax} LerpMax");
#endif
      }
#if MateThreat
      if (getNibble(ExtensionLimit, vThreat) > 0)
        sb.AppendFormat($" {wThreatDepthMin} ThreatMin");
#endif
      if (MultiPVLength > 1)
        sb.AppendFormat($" {MultiPVLength} MultiPV");

      if (MoveBottle.nKillers != 1)
        sb.AppendFormat($" {MoveBottle.nKillers} Killers");
    }

    private void herald(DateTime dtStarted, String sName) {
      var sb = new StringBuilder();
      sb.AppendLine();                  // Following UCI prompt
      sb.AppendFormat($"{dtStarted:yyyy-MM-dd}");

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
