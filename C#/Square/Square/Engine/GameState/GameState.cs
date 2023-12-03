//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-06-22 CNHume]Created Class
//
// Conditionals:
//
#define ShowClockSpeed
//#define TestSlowManagementObject
#define ShowGC
//#define ShowGCLatency
//#define ShowSIMD
#define QuiescentTryXP
#define AddBestMoves
//#define Magic
//#define LateMoveReduction
//#define KillerCompositionHash
#define MateThreat
#define Mobility
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
#define CountEvalTypes                  // For displayEvalTypeCounts()
//#define CountCapturedPiece
#define CountEarlyMoves
#define CountPVDoubles
//#define Controlled
#define DisplayOptions
#define DisplayPosition
//#define DisplayPositionPool
#define GetSmart
#define UseKillers
#define LazyMoveSort
//#define QuietCheck
//#define QuietMate
//#define SwapOn
//#define TestBest
//#define TestCornerCP
//#define TestLerp
//#define ThreadSafeTank
//#define TracePosition
//#define MaterialBalance
//#define XPHash128
//#define QXPHash128

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Management;
using System.Runtime;
using System.Text;

using static System.String;

namespace Engine;

using Command;                          // For Scanner

using MoveOrder;                        // For MoveBottle

using Resource;

using Test;

using static Board;
using static Position;

//
// Type Aliases:
//
using Depth = UInt16;
using Ply = UInt16;

partial class GameState : IDisposable {
  #region Constants
  internal const Depth wDepthMax = 48;  // Used by Predict()
  internal const Ply wPlyHistory = 64;
  #endregion

  #region Enumerations
  public enum SearchMode : byte { BestMove, Perft };
  #endregion

  #region Constructors
  public GameState() {
    //
    //[ToDo]Allocate a separate Pool of positions for each thread:
    //
    PositionPool = new PooledPosition(this);

    Bound = new SearchBound();
    IsChess960 = false;
    Case = new PerfCase();

    newBestLine(wDepthMax);
    newTimers();
    SeededRandom = new Random();        // Variable seed based on Environment.TickCount

    loadEndgameValue();
    loadExtensionLimit();
#if UseKillers
#if KillerCompositionHash
    var uBottleLength = 8191U;
#else
    var uBottleLength = wPlyHistory;
#endif
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
  [MemberNotNull(
    nameof(Variation),
    nameof(QXPTank),
    nameof(XPTank),
    nameof(XPMTank)
    )]
  protected void wireControls() {
    Opponent = default;

    //
    // Wire up the Event Handlers to listen for UCI Option changes:
    //
    wireClearHash();
    // Step 6/6: Subscribe to Event Handler in Wireup methods
    wireMultiPV();
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

  [MemberNotNull(nameof(BestLine))]
  private void newBestLine(Int32 nCapacity) {
    BestLine = new List<Move>(nCapacity);
  }

  [MemberNotNull(
    nameof(IterationTimer),
    nameof(SearchTimer)
    )]
  private void newTimers() {
    IterationTimer = new Stopwatch();
    SearchTimer = new Stopwatch();
  }

  private void clearSearchCounts() {
#if UseKillers
    Bottle.Clear();
#endif                                  // UseKillers
    XPTank.Counts.Clear();
    QXPTank.Counts.Clear();
    PXPMemo.Counts.Clear();
    CXPMemo.Counts.Clear();

    Nodes = PseudoMoves =
      RepetitionPlies = RepetitionSearches =
      LookupCyclePlies = LookupCycleSearches =
      PinSkipTotal = QuietSkipTotal =
#if CountCapturedPiece
      CapturedPieces =
#endif
      NullMoves = PrunedNullMoves =
      DeltaPruneTotal = FutilePruneTotal = OccamPruneTotal =
      CheckExtCount = ThreatExtCount = SingularExtCount =
      DrawTotal = MateTotal =
      WhiteSearchedPositionCount = WhiteEarlyMoveTotal =
      BlackSearchedPositionCount = BlackEarlyMoveTotal =
      PVDoubleTotal = PVSimpleTotal = PVSingleTotal = ZWSimpleTotal =
      ReducedTotal = Evals = FullEvals =
      LowerCount = UpperCount = ExactCount =
#if QuiescentTryXP
      XPGetHitsQxnt = XPGetReadsQxnt =
#endif
      LegalMoves = LegalQxntMoves = IllegalMoves = IllegalQxntMoves = 0L;

    clearNodeDelta();                   // See DisplayPrediction
    clearEarlyMoveCounts();             //[Conditional]
    clearPVDoubleCounts();              //[Conditional]
  }

  public Position Push(Position? parent) {
    var child = PositionPool.Push();
    child.Parent = parent;
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

  private void unwindGame() {
    while (MovePosition is not null)
      Unmove();
    RootPosition = default;
  }

  public void Clear() {                 // Called by UCI.NewGame()
    unwindGame();
    clearSearchCounts();                //[Init]Normally called by startSearch()
  }

  public static void SetLanguage(String? sLanguage) {
    LanguageName = sLanguage;
    SetPieceSymbols(LanguageName);
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
  private UInt32? clockSpeed() {
    UInt32? uSpeed = default;
    if (OperatingSystem.IsWindows()) {  // Suppress SupportedOSPlatform warnings
#if TestSlowManagementObject
      const String sPath = "Win32_Processor.DeviceID='CPU0'";
      using var mo = new ManagementObject(sPath);
      uSpeed = (UInt32)mo["CurrentClockSpeed"];
#else                                   //!TestSlowManagementObject
      //[Note]Specifying the CurrentClockSpeed column improves performance
      const String sQuery = "select CurrentClockSpeed from Win32_Processor";
      using var mos = new ManagementObjectSearcher(sQuery);
      foreach (var mbo in mos.Get()) {
        var properties = mbo.Properties.Cast<PropertyData>();
        var pd = properties.FirstOrDefault(pd =>
          OperatingSystem.IsWindows() &&
          pd.Name == "CurrentClockSpeed");

        if (pd != null) {
          uSpeed = (UInt32)pd.Value;
          break;
        }
      }
#endif                                  // TestSlowManagementObject
    }
    return uSpeed;
  }

  [Conditional("DisplayOptions")]
  private void appendOptions(StringBuilder sb) {
#if ShowClockSpeed
    var uSpeedMHz = clockSpeed();
    if (uSpeedMHz != null) {
      var dSpeedGHz = (Double)uSpeedMHz.Value / 1E3;
      sb.AppendFormat($" {dSpeedGHz:0.0##} GHz");
    }
#endif                                  // ShowClockSpeed
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
#if ShowSIMD
    // SIMD-Accelerated Numeric Types
    var sHardware = Vector.IsHardwareAccelerated ? "Accelerated" : "None";
    sb.AppendFormat($" SIMD={sHardware}");
#endif
#if ShowGC
    var sServerGC = GCSettings.IsServerGC ? "Server" : "Workstation";
    sb.AppendFormat($" GC={sServerGC}");
#endif
#if ShowGCLatency
    var sLatency = GCSettings.LatencyMode;
    sb.AppendFormat($" Latency={sLatency}");
#endif
#if TracePosition
    sb.Append(" Trace");
#endif
#if ThreadSafeTank
    sb.Append(" Safe");
#endif
#if GetSmart
    sb.Append(" Smart");
#endif
#if LazyMoveSort
    sb.Append(" Lazy");
#endif
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
#if QuietCheck
    sb.Append(" QuietCheck");
#elif QuietMate
    sb.Append(" QuietMate");
#endif
#if SwapOn
    sb.Append(" SwapOn");
#endif
#if TestBest
    sb.Append(" TestBest");
#endif
#if TestCornerCP
    sb.AppendEvalInfo(mKBNvKMateCornerWeight);
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
      var vLimit = GetNibble(ExtensionLimit, (Int32)extension);
#if !LateMoveReduction
      if (extension == SearchExtensions.Late)
        continue;                       //[Disabled]
#endif
      if (extension == SearchExtensions.Threat) {
#if MateThreat
        if (vLimit == 0) {
#else
        if (true) {
#endif
          sb.Append(" No Threats");
          continue;                     //[Disabled]
        }
      }
#if !SingularExtension
      if (extension == SearchExtensions.Singular)
        continue;                       //[Disabled]
#endif
      if (vLimit > 0) {
        sb.AppendFormat($" {vLimit} {extension}");

        if (vLimit != 1)                // Plural
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
    if (GetNibble(ExtensionLimit, vThreat) > 0)
      sb.AppendFormat($" {wThreatDepthMin} ThreatMin");
#endif
    if (MultiPVLength > 1)
      sb.AppendFormat($" {MultiPVLength} MultiPV");

    if (MoveBottle.nKillers != 1)
      sb.AppendFormat($" {MoveBottle.nKillers} Killers");
  }

  private void herald(DateTime dtStarted, String? sName) {
    var sb = new StringBuilder();
    sb.AppendLine();                    // Following UCI prompt
    sb.AppendFormat($"{dtStarted:yyyy-MM-dd}");

    if (!IsNullOrEmpty(sName)) {
      sb.Append(cSpace);
      sb.Append(sName);
    }

    Bound.AppendBounds(sb);
    appendOptions(sb);
#if DisplayPosition
    sb.AppendLine();
    MovePosition?.Display(sb);
#endif
    sb.FlushLine();
  }

  private void displayCounts(SearchMode mode, Double dElapsedMS) {
    //
    // All of the following display methods are Conditional
    //
    displayNodeCounts(dElapsedMS);
    displayPseudoMoveTotals();

    //
    // None of the following are counted in SearchMode.Perft:
    //
    if (mode == SearchMode.BestMove) {
      displayPVSTotals();               //[Conditional]
      displayEarlyMoveTotals();         //[Conditional]
      displayDetails();                 //[Conditional]
      displayEvals();                   //[Conditional]
      displayCXP();                     //[Conditional]
      displayPXP();                     //[Conditional]
      displayXPM();                     //[Conditional]
      displayQXP();                     //[Conditional]
      displayXP();                      //[Conditional]
      displayEvalTypeCounts();          //[Conditional]
      DisplayPositionPool();            //[Conditional]
    }
  }
  #endregion                            // Banner Methods
}
