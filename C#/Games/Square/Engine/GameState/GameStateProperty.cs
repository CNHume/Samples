//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2013-06-24 CNHume]Created File
//
// Conditionals:
//
//#define LinkedTranspositions
#define QuiescentTryXP
#define UseTask
//#define CountCapturedPiece
//#define MaterialBalance

using System.Diagnostics;

namespace Engine;

using Cache;

using CacheValue;

using Commands;                          // For SearchBound

using MoveOrder;                        // For Variation

using Resource;

using Test;

using static Board;
using static Position;

//
// Type Aliases:
//
using Eval = Int16;
using Ply = UInt16;
using PlyDepth = Byte;

partial class GameState {
  #region Thread Management Fields
  public PooledPosition PositionPool;
#if UseTask
  public CancellationTokenSource? CancellationTokenSource;
  public CancellationToken CancellationToken;
  protected Timer? CancelTimer;
  public Task<List<Move>?>? EngineTask;
  public Task<List<Move>?>? FinishTask;
#endif
  public Stopwatch SearchTimer;
  public Stopwatch IterationTimer;
  public UInt64 IntervalNodes;
  public UInt64 HeartbeatNodes;
  public TimeSpan SearchElapsedOfLastHeartbeat;
  public TimeSpan HeartbeatPeriod;
  public Boolean IsDisplayHeartbeat;
  #endregion

  #region Count Fields
  //
  // Metrics
  //
  public Int64 DrawTotal;
  public Int64 MateTotal;
  public Int64 OccamPruneTotal;
  public Int64 DeltaPruneTotal;
  public Int64 FutilePruneTotal;
  public Int64 CheckExtCount;
  public Int64 ThreatExtCount;
  public Int64 SingularExtCount;

  public Int64 WhiteSearchedPositionCount;
  public Int64 BlackSearchedPositionCount;
  public Int64 WhiteEarlyMoveTotal;
  public Int64 BlackEarlyMoveTotal;

  public Int64 PVSimpleTotal;
  public Int64 PVSingleTotal;
  public Int64 PVDoubleTotal;
  public Int64 ZWSimpleTotal;
  public Int64 ReducedTotal;
  public Int64[] EarlyMoveCount;
  public Ply EarlyMoveMinPly;
  public Ply EarlyMoveMaxPly;
  public Int64[] PVDoubleCount;
  public Ply PVDoubleMinPly;
  public Ply PVDoubleMaxPly;
  public Int64 PseudoMoves;
  public Int64 PinSkipTotal;
  public Int64 QuietSkipTotal;
  public Int64 RepetitionSearches;
  public Int64 RepetitionPlies;
  public Int64 LookupCycleSearches;
  public Int64 LookupCyclePlies;
#if CountCapturedPiece
  public Int64 CapturedPieces;          // For Capture or Static Evaluation
#endif
  public Int64 PrunedNullMoves;
  public Int64 NullMoves;
  public Int64 Nodes;                   // Nodes == TotalMoves + NullMoves
  public Int64 IllegalMoves;
  public Int64 IllegalQxntMoves;
  public Int64 LegalMoves;
  public Int64 LegalQxntMoves;
  public Int64 Evals;
  public Int64 FullEvals;
  public Int64 ExactCount;
  public Int64 UpperCount;
  public Int64 LowerCount;
#if QuiescentTryXP
  public Int64 XPGetReadsQxnt;
  public Int64 XPGetHitsQxnt;
#endif
  public UInt64[] NodeDelta;
  public Double[] NodeDeltaLog;
  #endregion                            // Count Fields

  #region Lookup Tables
  public Tank<QuietPosition> QXPTank;
  public Tank<Transposition> XPTank;
  public Tank<PositionMove> XPMTank;
#if MaterialBalance
  public Memo2<Composition2> CXPMemo;
#else
  public Memo<Composition> CXPMemo;
#endif
  public Memo2<PawnPosition> PXPMemo;
  public PlyDepth StartDepth;
  public Variation[] Variation;
  public Byte MultiPVLength;            // # of variations sought [UCI] Option
  public Byte MultiPVCount;             // # of variations found
  public UInt16 ExpectedMovesToGo;      // Option
  public Eval ContemptValue;            // Option
  public UInt16 ExtensionLimit;
  #endregion                            // Lookup Tables

  #region Static Fields
  public static readonly SearchExtensions[] Extensions;
  #endregion                            // Static Fields

  #region Primary Fields
  private Boolean disposed = false;     // IDisposable
  public SearchBound Bound;

  public Position? MovePosition;
  public Position? RootPosition;
  public MoveBottle Bottle;
  public PerfCase Case;
  public Eval EndgameValue;

  public Boolean IsChess960;            // Used by AppendCastlingRights()
  public Boolean IsAspiration;          // option
  public Boolean IsFlip;                // option
  public Boolean IsFutility;            // option
  public Boolean IsNullPrune;           // option
  public Boolean IsOccam;               // option
  public Boolean IsPure;                // Write moves in PACN vs AN
  public Boolean IsPonderEnabled;       //[UCI Option]For Time Control
  public Boolean IsAnalyseMode;         //[UCI Option]
  public Boolean IsDisplayCurrentLine;  //[UCI Option]
  public String? Opponent;              //[UCI Option]
  public static String? LanguageName;
  public List<Move> BestLine;
  public Random SeededRandom;
  #endregion                            // Primary Fields

  #region Properties
  public Ply MovePly {
    get {
      ArgumentNullException.ThrowIfNull(MovePosition);
      return MovePosition.GamePly;
    }

    set {
      ArgumentNullException.ThrowIfNull(MovePosition);
      MovePosition.GamePly = value;
    }
  }

  public Boolean IsSearchInProgress {
    get {
#if UseTask
      //
      // IsCompleted means a task has entered one of the Final States:
      // Canceled, Faulted or RanToCompletion
      //
      return EngineTask != null && !EngineTask.IsCompleted;
#else
      return false;
#endif
    }
  }

  public Int64 TotalLegalMoves {
    get { return LegalMoves + LegalQxntMoves; }
  }

  public Int64 TotalIllegalMoves {
    get { return IllegalMoves + IllegalQxntMoves; }
  }

  public Int64 TotalMoves {
    get { return TotalIllegalMoves + TotalLegalMoves; }
  }

  public Int64 TotalQxntMoves {
    get { return IllegalQxntMoves + LegalQxntMoves; }
  }
  #endregion                            // Properties
}
