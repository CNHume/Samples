//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2013-03-17 CNHume]Split Performance Timers into their own file
//
// Conditionals:
//
//#define Magic
//#define ByteDeBruijn
//#define DeBruijn                        // DeBruijn vs Mask
//#define FullData                        // Full vs Half
#define UseMoveSort
#define TestOutsideSquare

namespace Engine {
  using CacheValue;
  using Command;

  using System;
  using System.Diagnostics;
  using System.Collections.Generic;
  using System.Numerics;
  using System.Text;

  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using CompositionCounter = UInt16;
  using Eval = Int16;
  using Ply = UInt16;
  using PlyDepth = Byte;
  using Depth = UInt16;
  using Plane = UInt64;

  partial class Position : Board {
    #region Timer Test Selector
    internal void TimerTest() {
      //[Test]writeDepthShallow((PlyDepth)32);
      //[Test]writeDepthDeep((PlyDepth)32);
      //
      //[Test]testOutsideSquare(sq.c1);
      //[Test]testOffsets();
      //[Test]testRotations();
      //[Test]testPieceMasks();
      //[Test]testPawnAttacks();
      //[Test]
      //for (sq sq = sq.a1; sq <= sq.h8; sq++)
      //  testAtxMasks(sq);
      //
      //[Time]timeRoots();
      //[Time]timeEval();
      timeWeighPieces();
      //[Time]timeGenerate(PseudoMoves, NoSwaps);
      //[Time]timeAddPieceCapturesAndMoves();
      //[Time]timeAddPawnCapturesAndMoves();
      //[Time]timeRectAtx();
      //[Time]timeSafe();
      //[Time]timeMagic();
      //[Time]
      //timeRemoveLo();
      //[Time]timeStaticLoads();
      //[Time]timeCapturedPiece();
      //[Test]timeExecute("test", 100000);
    }

    private static Stopwatch TimerStart(string sMethod, UInt64 qTrials) {
      LogLine($"Timing {qTrials:n0} {sMethod} trials at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
      var sw = new Stopwatch();
      sw.Start();
      return sw;
    }

    private static void TimerStop(Stopwatch sw, UInt64 qTrials) {
      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;
      var dRate = qTrials / dElapsedMS;
      LogLine($"Completed {qTrials:n0} trials in {dElapsedMS / 1000:0.0##} sec, Rate = {dRate:0.0##} KHz");
    }

    [Conditional("TestOutsideSquare")]
    protected void testOutsideSquare(sq sq) {
      var n = (Int32)sq;
      foreach (var parameter in Parameter) {
        testRect($"{parameter.SideName}KingToMoveLoss[{sq}]", parameter.KingToMoveLoss[n]);
        testRect($"{parameter.SideName}PawnToMoveWins[{sq}]", parameter.PawnToMoveWins[n]);
      }
    }

    protected void everyRoot() {
      for (UInt16 w = 0; w < UInt16.MaxValue; w++) {
        var root = ISqrt(w);
      }
    }
    #endregion

    #region Timer Tests
    protected void timeRoots(UInt64 qTrials = 100000UL) {
      //verifyISqrt();
      var sw = TimerStart(nameof(everyRoot), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        everyRoot();

      TimerStop(sw, qTrials);
    }

    protected void timeEval(UInt64 qTrials = 100000000UL) {
      var sw = TimerStart(nameof(staticEval), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        staticEval(out PawnPosition? pp);

      TimerStop(sw, qTrials);
    }

    protected void timeWeighPieces(UInt64 qTrials = 1000000000UL) {
      var sw = TimerStart(nameof(weighPieces), qTrials);

      CompositionCounter wPieceCounts = default;
      var fside = SideFlags.Pair;

      Eval value = default;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        value = weighPieces(wPieceCounts, fside);

      TimerStop(sw, qTrials);
    }

    // Generates moves at ~18 MHz
    protected void timeGenerate(List<Move> moves, Boolean bSwap, UInt64 qTrials = 10000000UL) {
      var sw = TimerStart($"{nameof(generate)}({bSwap})", qTrials);

      var qMoveCount = 0UL;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        qMoveCount += (UInt32)generate(moves, bSwap);

      TimerStop(sw, qTrials);
    }

    //~600 KHz, ~900 KHz sans List<Move>.Add()
    protected void timeAddPieceCapturesAndMoves(UInt64 qTrials = 10000000UL) {
      var sw = TimerStart(nameof(addPieceCapturesAndMoves), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        addPieceCapturesAndMoves2(MASK64, Side[White].Piece);
        //addPieceCapturesAndMoves2(MASK64, Side[Black].Piece);

        clearPseudoCaptures();
        clearPseudoMoves();
      }

      TimerStop(sw, qTrials);
    }

    protected void timeAddPawnCapturesAndMoves(UInt64 qTrials = 10000000UL) {      //~2690 KHz
      var sw = TimerStart(nameof(BoardSide.AddPawnMoves), qTrials);
      var whiteSide = Side[White];
      var blackSide = Side[Black];

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        whiteSide.AddPawnCaptures(this, blackSide.Piece);
        whiteSide.AddPawnMoves(this, ~RankPiece);

        blackSide.AddPawnCaptures(this, whiteSide.Piece);
        blackSide.AddPawnMoves(this, ~RankPiece);

        PseudoPawnBelowCapture.Clear();
        PseudoPawnAboveCapture.Clear();

        PseudoPawnBelowMove.Clear();
        PseudoPawnAboveMove.Clear();
      }

      TimerStop(sw, qTrials);
    }

    protected void timeSafe(UInt64 qTrials = 100000000UL) { // 1.22 MHz
      var sw = TimerStart(nameof(BoardSide.Safe), qTrials);
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      //var qpMoveTo = KingAtx[(Int32)sq.e4];
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var nFrom = (Int32)(qTrial % nSquares);
        var qpMoveTo = KingAtx[nFrom];
        qpMoveTo &= blackSide.Safe(qpMoveTo);
      }

      TimerStop(sw, qTrials);
    }

    // diagAtx() 17.837 MHz [56 sec] for Rotations [C++ inline 3.14x faster]
    // rectAtx() 19.365 MHz [52 sec] for Rotations [C++ inline 3.4x faster]
    // KnightAtx[] ~63 MHz [C++ 8.44x faster]
    protected void timeRectAtx(UInt64 qTrials = 1000000000UL) {
      var sw = TimerStart(nameof(rectAtx), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var nFrom = (Int32)(qTrial % nSquares);
        var qpMoveTo = rectAtx(nFrom);
      }

      TimerStop(sw, qTrials);
    }

    //
    // On a Compaq 3 GHz Pentium 4:
    //
    // 16 MHz via DllImport
    // 76.23 MHz [14,846,928,128 bits in 195 sec] 3.75x slower than Native C++ [92 MHz for 8-bit] 4x
    //
    // On a Dell i7-4702HQ at 2.2 GHz w 4-cores:
    // x86 was 233.13 MHz
    // x64 was 301.45 MHz +29%
    //
    // Using i7-9700K CPU at 3.60GHz w 8-cores:
    //
    // Full Mask      =  22,564.195 KHz
    // Half DeBruijn  =  15,456.906 KHz
    //
    // Full Mask 46% faster for timeRemoveLo()
    // Half DeBruijn 2.5% faster for perft2
    //
    protected void timeRemoveLo(UInt64 qTrials = 1000000000UL) {
      //Int32 n;
      //n = BitOperations.TrailingZeroCount((UInt64)0);
      //n = BitOperations.TrailingZeroCount((UInt32)0);
      //n = BitOperations.TrailingZeroCount((UInt16)0);
      //n = BitOperations.TrailingZeroCount((Byte)0);
      var sw = TimerStart(nameof(RemoveLo), qTrials);
      var qBits = 0UL;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        //[Warning]Avoid timing qpPiece == 0 cases, which do not arise in standard use
        var qpPiece = ~qTrial;
        while (qpPiece != 0) {
          var nFrom = RemoveLo(ref qpPiece);
          qBits++;
        }
      }

      var sb =
        new StringBuilder($"Counted {qBits:n0} bits using")
        .AppendTZCMode();
      LogLine(sb.ToString());

      TimerStop(sw, qTrials);
    }

    protected void timeMagic(UInt64 qTrials = 1000000000UL) {
      var sw = TimerStart(nameof(rotateRank), qTrials);
      var n = (Int32)sq.e4;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var vRank = rotateRank(n);                    // 11 sec, 90.65 MHz
        //var vFileHalf = hashFileHalf(RankPiece, n);    // 29.48 sec, 33.9 MHz
        //var vA1H8Half = hashA1H8Half(RankPiece, n);    // 30.69 sec, 32.59 MHz
        //var vA8H1Half = hashA8H1Half(RankPiece, n);    // 30.55 sec, 32.74 MHz
      }

      TimerStop(sw, qTrials);
    }

    protected void timeCapturedPiece(UInt64 qTrials = 1000000000UL) {
      var sw = TimerStart(nameof(getPieceIndex), qTrials);
      var nFrom = (Int32)sq.d1;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        getPieceIndex(nFrom);
      }

      TimerStop(sw, qTrials);
    }

    protected void timeMove(Move mov, UInt64 qTrials = 100000000UL) {
      var sb = new StringBuilder();
      sb.AppendPACN(mov, Side, State.IsChess960);
      var sw = TimerStart($"{nameof(movePiece)}({sb})", qTrials);

      //~4.5 MHz w resetMove() which is 5.33 times faster at ~24 MHz
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        resetMove();

        // Calculated to be ~5.54 MHz on old PC, now ~18.5 MHz
        var move = mov;
        var nEnPassant = movePiece(ref move);
      }

      TimerStop(sw, qTrials);
    }

    protected void timeListAdd(UInt64 qTrials = 10000000UL) {      //~35.56 MHz
      var sw = TimerStart(nameof(List<Move>.Add), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        PseudoPawnBelowMove.Add(Move.NullMove);
        if (qTrial % 1000 == 0)
          PseudoPawnBelowMove.Clear();
      }

      TimerStop(sw, qTrials);
    }

    protected void timeStaticLoads(UInt64 qTrials = 10000UL) {
      var sw = TimerStart("Static Load", qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        loadRankOffset();
#if Magic
        newMagic();
        loadMagic();
#else
        loadRotation();
        loadRectBit();
        loadDiagBit();
#endif
        colorSquares();
        loadPieceAtx();

        loadRectAtx();
        loadDiagAtx();
#if ByteDeBruijn
        loadDeBruijn(deBruijnByte, 3, vDeBruijn);
#endif
#if DeBruijn
#if FullData
        loadDeBruijn(deBruijnFull, 6, qDeBruijn);
#else                                   //!FullData
        loadDeBruijn(deBruijnHalf, 5, uDeBruijn);
#endif
#endif                                  // DeBruijn
        loadZobrist();
        //var state = new GameState(null);
        //state.loadEndgameValue();
      }

      TimerStop(sw, qTrials);
    }

    protected void timeExecute(String sInput, UInt64 qTrials) {
      using (var command = new UCI()) {
        var sw = TimerStart(nameof(command.Execute), qTrials);
        var qTrial = 0UL;
        for (var bContinue = true; bContinue && qTrial < qTrials; qTrial++) {
          bContinue = command.Execute(sInput);
        }

        TimerStop(sw, qTrial);
      }
    }
#endregion
  }
}
