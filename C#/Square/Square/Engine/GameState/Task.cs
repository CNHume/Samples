//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2016-08-30 CNHume]Created File
//
// Conditionals:
//
#define UseTask
#define Herald
#define NoteStartAndFinish
//#define StackTrace

namespace Engine {
  using Command;
  using Command.Exceptions;
  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Diagnostics.CodeAnalysis;
  using System.Text;
  using System.Threading;
  using System.Threading.Tasks;

  using static Board;
  using static Logging.Logger;
  using static System.String;

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

    private void freeCancelTimer() {
      if (CancelTimer is not null) {
        CancelTimer.Dispose();
        CancelTimer = default;
      }

      // Free CancellationTokenSource referenced by CancelTimer
      freeCancellationToken();
    }

    private void freeCancellationToken() {
      //
      // The following is an improvement upon "Working with CancellationToken and Dispose",
      // posted by Richard Blewett 2015-12-22.
      // See https://blogs.rocksolidknowledge.com/2015/12/22/working-with-cancellationtoken
      //
      if (CancellationTokenSource is not null) {
        CancellationToken = default(CancellationToken);
        CancellationTokenSource.Dispose();
        CancellationTokenSource = default;
      }
    }
    #endregion

    #region Task Management
    [MemberNotNull(nameof(MovePosition))]
    public void OnMoveCommand() {
      if (IsSearchInProgress)
        throw new ChessException("Search in progress");
      else if (MovePosition is null)
        throw new ChessException("Uninitialized Position");
      else {
        var sInvalid = MovePosition.IsValid();
        if (!IsNullOrEmpty(sInvalid))
          throw new InvalidPositionException(sInvalid);
        else if (!MovePosition.IsLegal())
          throw new ChessException("Illegal Move");
      }
    }

    private List<Move>? startSearch(Position? position, SearchMode mode) {
      try {
        if (position is null)
          throw new PositionException("Null Position");
        else if (SearchTimer is null)
          throw new PositionException("Null SearchTimer Stopwatch");
        else {
          SearchTimer.Reset();
          IntervalNodes =
            HeartbeatNodes = (UInt64)NodeTotal;
          LastBeatMS = SearchTimer.ElapsedMilliseconds;
        }

        if (position.IsLegal()) {
          StartDepth = 0;               //[Init]
          ClearSearchCounts();

          if (UCI.IsDebug) {
            var dtStarted = DateTime.Now;
#if Herald
            herald(dtStarted, position.Name);
#endif
#if NoteStartAndFinish
            LogInfo(Level.note, $"Started at {dtStarted:yyyy-MM-dd HH:mm:ss.ff}");
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
        LogInfo(Level.note, ex.Message);
      }
      catch (ApplicationException ex) {
#if StackTrace
        LogInfo(Level.error, ex.ToString());
#else
        LogInfo(Level.error, ex.Message);
#endif
      }
      catch (Exception ex) {
        LogInfo(Level.error, ex.ToString());
      }
      finally {
        if (SearchTimer.IsRunning)
          SearchTimer.Stop();

        if (UCI.IsDebug) {
          LogInfoNewLine(Level.note);
#if NoteStartAndFinish
          LogInfo(Level.note, $"Finished at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
#endif
          var dElapsedMS = (Double)SearchTimer.ElapsedMilliseconds;
          displayCounts(mode, dElapsedMS);
        }
      }

      //
      //[Optional]Repeat final bestmove report:
      //
      if (BestMoves is not null &&
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
      if (CancellationTokenSource is null)
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
#endif
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
          Debug.Assert(CancelTimer is not null, "Null CancelTimer Instance");
          //[Note]Timeout.Infinite declares that the CancelTimer should not restart!
          CancelTimer.Change(nTimeoutMS, Timeout.Infinite);
#endif
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
        if (CancelTimer is not null)
          CancelTimer.Change(stopTimeoutMS, Timeout.Infinite);

        FinishTask?.Wait();

        //
        //[Note]The old CancellationToken must be recycled after cancel() has been called.
        //
        freeCancellationToken();
#endif
      }
      catch (AggregateException aex) {
        var ex = aex.Flatten();
        //LogLine(ex.ToString());
        throw ex;
      }
    }

    public void ListMovesFromParent(Position position, Position parent, Boolean bPure, Boolean bAbbreviate = true) {
      var moves = position.MovesFromParent(parent, bAbbreviate);
      var sb = new StringBuilder();
      sb.WriteMoves(moves, RootPosition.GamePly, bPure, position.Side, IsChess960)
        .FlushLine();
    }

    public void ListMovesFromRoot(Position position, Boolean bPure, Boolean bAbbreviate = true) {
      ListMovesFromParent(position, RootPosition, bPure, bAbbreviate);
    }
    #endregion

    #region Heartbeat Methods
    //
    // Perform Heartbeat Tasks
    //
    private void heartbeat(Double dElapsedMS, UInt64 qNodeDelta, Position position) {
      var sb = new StringBuilder("info");
      //[Test]GameState.displayRate(dElapsedMS, qNodeDelta);
      if (dElapsedMS != 0) {
        var dRate = qNodeDelta * 1E3 / dElapsedMS;
        sb.AppendFormat($" nps {dRate:0}");
      }

      if (IsShowingLine) {
        sb.Append(" currline");
        if (!IsPure)
          sb.Append("-an");

        // Show search progress:
        var bAbbreviate = false;
        var moves = position.MovesFromParent(MovePosition, bAbbreviate);
        sb.AppendNumberedMoves(moves, MovePly, IsPure, position.Side, IsChess960)
          .AppendLine()
          .FlushLine();
      }
    }

    private void pollSearchTime(Position position, ulong qTotal) {
      var lSearchMS = SearchTimer.ElapsedMilliseconds;
      var lElapsedMS = lSearchMS - LastBeatMS;

      //
      // Test for Heartbeat Due
      //
      if (lElapsedMS > HeartbeatMS) {
        var qNodeDelta = qTotal - HeartbeatNodes;
        HeartbeatNodes = qTotal;
        LastBeatMS = lSearchMS;

        if (IsHeartbeat) {
          var dElapsedMS = (Double)lElapsedMS;
          heartbeat(dElapsedMS, qNodeDelta, position);
        }
      }
    }

    private void throwIfCancelled() {
#if UseTask
      if (CancellationTokenSource is not null) {
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
#endif
    }

    private CancellationToken getCancellationToken() {
      CancellationTokenSource = new CancellationTokenSource();
      return CancellationTokenSource.Token;
    }

    private void cancel() {
#if UseTask
      if (CancellationTokenSource is not null)
        CancellationTokenSource.Cancel();
#endif
    }

    //
    // Called whenever a move has been made, whether via [null|try]Move():
    //
    public void MonitorBound(Position position) {
      //
      // If nodes are processed at ~1 MHz, a Polling Interval of 100K nodes
      // ensures that the resolution for HearbeatMS is ~100 msec:
      //
      const UInt32 uIntervalNodeMax = 100 * 1000;

      //
      // Perform Bound Tests
      //
      AtomicIncrement(ref NodeTotal);
      if (Bound.Nodes <= (UInt64)NodeTotal)
        cancel();

      throwIfCancelled();

      var qTotal = (UInt64)NodeTotal;
      var qIntervalDelta = qTotal - IntervalNodes;

      //
      // Test whether the Polling Interval has elapsed:
      //
      if (qIntervalDelta > uIntervalNodeMax) {
        IntervalNodes = qTotal;
        pollSearchTime(position, qTotal);
      }
    }
    #endregion
  }
}