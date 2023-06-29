//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2016-08-30 CNHume]Created File
//
// Conditionals:
//
#define UseTask
#define Herald
#define NoteStartAndFinish
//#define StackTrace

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Text;

namespace Engine;

using Command;

using Exceptions;

using static Board;
using static Logging.Logger;

//
// Type Aliases:
//
using Eval = Int16;

partial class GameState {
  #region Interface Methods
  private void disposeTask() {
#if UseTask
    freeCancelTimer();
#endif
  }
  #endregion                            // Interface Methods

  #region Cancellation Token Methods
  private void freeCancellationToken() {
    //
    // The following is an improvement upon "Working with CancellationToken and Dispose",
    // posted by Richard Blewett 2015-12-22.
    // See https://blogs.rocksolidknowledge.com/2015/12/22/working-with-cancellationtoken
    //
    if (CancellationTokenSource != null) {
      CancellationToken = default(CancellationToken);
      CancellationTokenSource.Dispose();
      CancellationTokenSource = default;
    }
  }

  private void freeCancelTimer() {
    if (CancelTimer != null) {
      CancelTimer.Dispose();
      CancelTimer = default;
    }

    // Free CancellationTokenSource referenced by CancelTimer
    freeCancellationToken();
  }

  private void throwIfCancelled() {
#if UseTask
    if (CancellationTokenSource != null) {
      //
      // Test for Cancellation
      //
      if (CancellationToken.IsCancellationRequested) {
#if DEBUG
        Console.Beep();
#endif
        //
        // Throw OperationCanceledException if cancellation has been requested for this token:
        //
        CancellationToken.ThrowIfCancellationRequested();
      }
    }
#endif                                  // UseTask
  }

  private CancellationToken getCancellationToken() {
    CancellationTokenSource = new CancellationTokenSource();
    return CancellationTokenSource.Token;
  }

  private void cancel() {
#if UseTask
    if (CancellationTokenSource != null)
      CancellationTokenSource.Cancel();
#endif                                  // UseTask
  }
  #endregion                            // Cancellation Token Methods

  #region Task Management
  [MemberNotNull(nameof(MovePosition))]
  public void OnMoveCommand() {
    if (IsSearchInProgress)
      throw new ChessException("Search in progress");

    if (MovePosition is null)
      throw new ChessException("Uninitialized Position");

    MovePosition.Validate();
    if (!MovePosition.IsLegal())
      throw new ChessException("Illegal Move");
  }

  private List<Move>? startSearch(Position? position, SearchMode mode) {
    try {
      if (position is null)
        throw new PositionException("Null Position");

      SearchTimer.Reset();
      IntervalNodes =
        HeartbeatNodes = (UInt64)Nodes;
      LastBeatMS = SearchTimer.ElapsedMilliseconds;

      if (position.IsLegal()) {
        StartDepth = 0;               //[Init]
        clearSearchCounts();

        if (UCI.IsDebug) {
          var dtStarted = DateTime.Now;
#if Herald
          herald(dtStarted, position.Name);
#endif
#if NoteStartAndFinish
          LogInfo(LogLevel.note, $"Started at {dtStarted:yyyy-MM-dd HH:mm:ss.ff}");
#endif
        }

        SearchTimer.Start();

        switch (mode) {
        case SearchMode.BestMove:
          var mValue = position.IteratePlies(Bound);
          break;
        case SearchMode.Perft:
          position.IterateCases();
          break;
        }
      }
      else
        throw new PositionException("Illegal Setup");
    }
    catch (OperationCanceledException) {
      //
      // OperationCanceledException is thrown in MonitorBound() via ThrowIfCancellationRequested(),
      // when Cancel() has been called on the source of this Task's CancellationToken.  Catching
      // it here allows stack frames for an active search to return Position objects to the pool.
      //
    }
    catch (FinalPositionException ex) {
      LogInfo(LogLevel.note, ex.Message);
    }
    catch (ApplicationException ex) {
#if StackTrace
        LogInfo(Level.error, ex.ToString());
#else
      LogInfo(LogLevel.error, ex.Message);
#endif
    }
    catch (Exception ex) {
      LogInfo(LogLevel.error, ex.ToString());
    }
    finally {
      if (SearchTimer.IsRunning)
        SearchTimer.Stop();

      if (UCI.IsDebug) {
        LogInfo(LogLevel.note);
#if NoteStartAndFinish
        LogInfo(LogLevel.note, $"Finished at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
#endif
        var dElapsedMS = (Double)SearchTimer.ElapsedMilliseconds;
        displayCounts(mode, dElapsedMS);
      }
    }

    //
    //[Optional]Repeat final bestmove report:
    //
    if (BestMoves != null &&
        position is not null) {
      var sb = new StringBuilder();
      //[Note]refreshPV() has been called
      sb.BestMove(BestMoves, position.Side, IsChess960);
      if (sb.Length > 0)
        LogLine(sb.ToString());
    }

    return BestMoves;
  }

  public void StartTask(Func<Object?, List<Move>?> fun, Position position) {
#if UseTask
    if (CancellationTokenSource == null)
      CancellationToken = getCancellationToken();

    //
    // Start EngineTask to invoke startSearch(), which tests for cancellation
    // via monitorBound() below.
    //
    EngineTask = Task.Factory.StartNew(fun, position, CancellationToken);
    var nTimeoutMS = Bound.MoveTime(ExpectedMovesToGo);

    if (EngineTask.IsCompleted)
      FinishTask = EngineTask;
    else {
      //
      // The .Net 4.5 Framework substantially simplifies this by
      // providing a CancelAfter() method for CancellationSource.
      // The explicit approach allows CancleTimer.Change() to be
      // called in ponderhit() below.
      //
      // Start CancelTimer, which uses the CancellationSource to
      // cancel EngineTask when it expires.
      //
      const Int32 period = Timeout.Infinite;
      CancelTimer = new Timer(state => ((CancellationTokenSource?)state)?.Cancel(),
                              CancellationTokenSource, nTimeoutMS, period);

      //
      // The FinishTask will clean up when its antecedent EngineTask completes:
      // whether normally or by cancellation.  CancelTimer is then disposed of,
      // whether it fired or not.  FinishTask returns the EngineTask Result as
      // its own.
      //
      Func<Task<List<Move>?>, List<Move>?> fun2 =
        antecedent => {
          try {
            CancelTimer.Dispose();
            var result = antecedent.Result;
            return result;
          }
          catch (AggregateException aex) {
            throw aex.Flatten();
          }
          finally {
            CancelTimer = default;
          }
        };

      FinishTask = EngineTask.ContinueWith<List<Move>?>(fun2);
    }
#else
    FinishTask = EngineTask = default;
    fun(position);
#endif                                  // UseTask
  }

  public async Task<Eval> Split(Func<Eval> fun) {
    return await Task.Run(fun);
  }

  public void BestMoveSearch(Parser parser) {
    OnMoveCommand();
    BestMoves.Clear();
    if (Bound.ParseBounds(parser, MovePosition))
      StartTask((state) => startSearch((Position?)state, SearchMode.BestMove), MovePosition);
  }

  public void PerftSearch() {
    OnMoveCommand();
    var bWTM = MovePosition.WTM();
    Bound.Clear(bWTM);
    StartTask((state) => startSearch((Position?)state, SearchMode.Perft), MovePosition);
  }

  public void Ponderhit() {
    //
    //[Note]This command should only occur while a Ponder Search is in progress, after its
    // "go ponder" command has been issued.  It indicates that the previously hypothetical
    // move was in fact made by the User; and that the search may continue normally.
    //
    // The sequence preceding this "ponderhit" is as follows:
    //
    // When in Ponder Mode, the Engine returns a Ponder Move as well as the Best Move when
    // completing a search.
    //
    // The GUI issues a "go ponder" command, which includes the Ponder Move as part of the
    // starting position for the search; and the command also includes Time Controls to be
    // used in the event that a "ponderhit" command is subsequently handled here.
    //
    // If the Opponent chooses some move other than the Ponder Move, the GUI simply issues
    // a "stop" command to cancel the Ponder Search.  Then the GUI will start a new search
    // from the correct starting position.  Note that this will not be a Ponder Search.
    //
    if (Bound.IsPonder) {
      Bound.IsPonder = false;         // Cease to Ponder
      var nTimeoutMS = Bound.MoveTime(ExpectedMovesToGo);
      if (nTimeoutMS != Timeout.Infinite) {
#if UseTask
        Debug.Assert(CancelTimer != null, "Null CancelTimer Instance");
        //[Note]Timeout.Infinite declares that the CancelTimer should not restart!
        CancelTimer.Change(nTimeoutMS, Timeout.Infinite);
#endif                                  // UseTask
      }
    }
  }

  public void Stop() {
    try {
      cancel();
#if UseTask
      //
      // Block GUI requested stops briefly, allowing the search
      // statistics to be written before this request completes.
      //
      const Int32 stopTimeoutMS = 5000;
      if (CancelTimer != null)
        CancelTimer.Change(stopTimeoutMS, Timeout.Infinite);

      FinishTask?.Wait();

      //
      //[Note]The old CancellationToken must be recycled after cancel() has been called.
      //
      freeCancellationToken();
#endif                                  // UseTask
    }
    catch (AggregateException aex) {
      var ex = aex.Flatten();
      //LogLine(ex.ToString());
      throw ex;
    }
  }
  #endregion                            // Task Management

  #region Move List Methods
  public void ListMovesFromParent(Position position, Position? parent, Boolean bPure, Boolean bAbbreviate = true) {
    var moves = position.MovesFromParent(parent, bAbbreviate);
    var sb = new StringBuilder();
    var wGamePly = RootPosition?.GamePly ?? 0;
    sb.WriteMoves(moves, wGamePly, bPure, position.Side, IsChess960)
      .FlushLine();
  }

  public void ListMovesFromRoot(Position position, Boolean bPure, Boolean bAbbreviate = true) {
    ListMovesFromParent(position, RootPosition, bPure, bAbbreviate);
  }
  #endregion                            // Move List Methods

  #region Move Count Methods
  //
  // Perform Heartbeat Tasks
  //
  private void heartbeat(UInt64 qNodesDelta, Double dElapsedMS, Position? position) {
    var sb = new StringBuilder("info");
    //[Test]GameState.DisplayRate(qNodesDelta, dElapsedMS);
    if (dElapsedMS != 0) {
      var dRate = qNodesDelta * 1E3 / dElapsedMS;
      sb.AppendFormat($" nps {dRate:0}");
    }

    if (IsShowingLine && position is not null) {
      const Boolean bAbbreviate = false;

      sb.Append(" currline");
      if (!IsPure)
        sb.Append("-an");

      // Show search progress:
      var moves = position.MovesFromParent(MovePosition, bAbbreviate);
      sb.AppendNumberedMoves(moves, MovePly, IsPure, position.Side, IsChess960);
    }

    sb.AppendLine()
      .FlushLine();
  }

  private void pollSearchTimer(Position? position, UInt64 qNodes) {
    var lSearchMS = SearchTimer.ElapsedMilliseconds;
    var lElapsedMS = lSearchMS - LastBeatMS;

    //
    // Test for Heartbeat Due
    //
    if (lElapsedMS > HeartbeatPeriodMS) {
      var qNodesDelta = qNodes - HeartbeatNodes;
      HeartbeatNodes = qNodes;
      LastBeatMS = lSearchMS;

      if (IsHeartbeat) {
        var dElapsedMS = (Double)lElapsedMS;
        heartbeat(qNodesDelta, dElapsedMS, position);
      }
    }
  }

  //
  // Called for each move made by [null|try]Move() during a search.
  // Position allows heartbeat() to display "currline".
  //
  public void MonitorHeartbeat(Position? position = default) {
    //
    // Assuming nodes are processed at >1 MHz, a Polling Interval
    // of 100K nodes ensures a HearbeatMS resolution of <0.01 sec:
    //
    const UInt32 uIntervalNodeMax = 100 * 1000;

    var qNodes = (UInt64)Nodes;
    var qIntervalDelta = qNodes - IntervalNodes;

    //
    // Test whether the Polling Interval has elapsed:
    //
    if (qIntervalDelta > uIntervalNodeMax) {
      IntervalNodes = qNodes;
      pollSearchTimer(position, qNodes);
    }
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public void IncMove(Boolean bLegal, Boolean bQxnt = false) {
    AtomicIncrement(ref Nodes);

    if (bQxnt) {
      if (bLegal)
        AtomicIncrement(ref LegalMovesQxnt);
      else
        AtomicIncrement(ref IllegalMovesQxnt);
    }
    else if (bLegal)
      AtomicIncrement(ref LegalMoves);
    else
      AtomicIncrement(ref IllegalMoves);

    //
    // Test Nodes Bound
    //
    if (Bound.Nodes <= (UInt64)Nodes) {
      cancel();
      throwIfCancelled();
    }
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public void IncNullMove() {
    AtomicIncrement(ref NullMoves);
  }
  #endregion                            // Move Count Methods
}
