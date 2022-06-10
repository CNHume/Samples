//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
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

namespace Engine {
  using Cache;

  using CacheValue;

  using Command;                        // For SearchBound

  using MoveOrder;                      // For Variation

  using Resource;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Threading;
  using System.Threading.Tasks;

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
    public Pool<Position> PositionPool;
#if UseTask
    public CancellationTokenSource CancellationTokenSource;
    public CancellationToken CancellationToken;
    protected Timer? CancelTimer;
    public Task<List<Move>?>? EngineTask;
    public Task<List<Move>?>? FinishTask;
#endif
    public Stopwatch SearchTimer;
    public Stopwatch IterationTimer;
    public UInt64 IntervalNodes;
    public UInt64 HeartbeatNodes;
    public Int64 LastBeatMS;
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
    public Int64 PseudoMoveTotal;
    public Int64 PinSkipTotal;
    public Int64 QuietSkipTotal;
    public Int64 RepetitionSearches;
    public Int64 RepetitionPlies;
#if CountCapturedPiece
    public Int64 CapturedPieceTotal;    // For Capture or Static Evaluation
#endif
    public Int64 NullMovePruneTotal;
    public Int64 NullMoveTotal;
    public Int64 NodeTotal;             // IllegalMoves + LegalMoves + NullMoveTotal
    public Int64 IllegalMoves;
    public Int64 IllegalMovesQxnt;
    public Int64 LegalMoves;
    public Int64 LegalMovesQxnt;
    public Int64 TotalEvals;
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
    #endregion

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
    public Byte VariationCount;
    public Byte MultiPVLength;          //[UCI]Option
    public UInt16 ExpectedMovesToGo;    // Option
    public Eval Contempt;               // Option
    public Boolean IsHeartbeat;
    public UInt16 HeartbeatMS;
    public UInt16 ExtensionLimit;
    #endregion

    #region Static Field
    public static readonly SearchExtensions[] Extensions;
    #endregion

    #region Primary Field
    private Boolean disposed = false;   // IDisposable
    public ICommand Command;
    public SearchBound Bound;
    //
    //[Chess 960]Determined by InitCastleRules() or parseCastleRights():
    //
    public Boolean IsChess960;
    public Position? MovePosition;
    public Position? RootPosition;
    public MoveBottle Bottle;
    public PerfCase Case;
    public Eval EndgameValue;
    public Boolean IsAspiration;        // option
    public Boolean IsFlip;              // option
    public Boolean IsFutility;          // option
    public Boolean IsNullPrune;         // option
    public Boolean IsOccam;             // option
    public Boolean IsPure;              // Write moves in PACN vs. AN
    public Boolean IsPonderEnabled;     //[UCI Option]For Time Control
    public Boolean IsAnalyseMode;       //[UCI Option]
    public Boolean IsShowingLine;       //[UCI Option]
    public String? Opponent;             //[UCI Option]
    public static String? Language;
    public List<Move> BestMoves;
    public Random SeededRandom;
    #endregion

    #region Properties
    public Ply MovePly {
      get { return MovePosition.GamePly; }
      set { MovePosition.GamePly = value; }
    }

    public Boolean IsSearchInProgress {
      get {
#if UseTask
        //
        // IsCompleted means a task has entered one of the Final States:
        // Canceled, Faulted or RanToCompletion
        //
        return EngineTask is not null && !EngineTask.IsCompleted;
#else
        return false;
#endif
      }
    }

    public Int64 LegalMoveTotal {
      get { return LegalMoves + LegalMovesQxnt; }
    }

    public Int64 IllegalMoveTotal {
      get { return IllegalMoves + IllegalMovesQxnt; }
    }

    public Int64 MoveTotal {
      get { return IllegalMoveTotal + LegalMoveTotal; }
    }

    public Int64 MoveTotalQxnt {
      get { return IllegalMovesQxnt + LegalMovesQxnt; }
    }
    #endregion
  }
}
