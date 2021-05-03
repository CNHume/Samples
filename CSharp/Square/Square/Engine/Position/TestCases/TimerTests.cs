//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-03-17 CNHume]Split Performance Timers into their own file
//
// Conditionals:
//
//#define Magic
#define HalfDeBruijn
#define UseMoveSort
#define TestOutsideSquare

namespace Engine {
  using CacheValue;

  using System;
  using System.Diagnostics;
  using System.Collections.Generic;
  using System.Text;

  using static CastleRule;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Ply = System.UInt16;
  using PlyDepth = System.Byte;
  using Depth = System.UInt16;
  using Plane = System.UInt64;

  partial class Position : Board {
    #region Performance Timers
    protected void everyRoot() {
      for (var w = (UInt16)0; w < UInt16.MaxValue; w++) {
        var root1 = ISqrt(w);
      }
    }

    protected void timeRoots() {
      //verifyISqrt();
      const UInt64 qTrials = 100000UL;

      var t0 = DateTime.Now;
      LogLine("Timing ISqrt() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        everyRoot();

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    protected void timeEval() {
      const UInt64 qTrials = 100000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing staticEval() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        staticEval(out PawnPosition pp);

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    protected void timeGenerate(List<Move> moves, Boolean bSwap) {    // Generates moves at ~18 MHz
      const UInt64 qTrials = 10000000UL;
      var qMoveCount = 0UL;

      var t0 = DateTime.Now;
      LogLine("Timing generate() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        qMoveCount += (UInt32)generate(moves, bSwap);

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qMoveCount / dElapsedMS;
      LogLine("Generated {0} moves in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qMoveCount, dElapsedMS / 1000, dRate);
    }

    protected void timeAddPieceCapturesAndMoves() {     //~600 KHz, ~900 KHz sans List<Move>.Add()
      const UInt64 qTrials = 10000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing addPieceCapturesAndMoves() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        addPieceCapturesAndMoves2(MASK64, Side[White].Piece);
        //addPieceCapturesAndMoves2(MASK64, Side[Black].Piece);

        clearPseudoCaptures();
        clearPseudoMoves();
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var qTrial2 = 1 * qTrials;
      var dRate = qTrial2 / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrial2, dElapsedMS / 1000, dRate);
    }

    protected void timeAddPawnCapturesAndMoves() {      //~2690 KHz
      const UInt64 qTrials = 10000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing addPawnCapturesAndMoves() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        addPawnCaptures(Side[White], Side[Black].Piece);
        addPawnMoves(Side[White], ~RankPiece);

        addPawnCaptures(Side[Black], Side[White].Piece);
        addPawnMoves(Side[Black], ~RankPiece);

        PseudoPawnBelowCapture.Clear();
        PseudoPawnAboveCapture.Clear();

        PseudoPawnBelowMove.Clear();
        PseudoPawnAboveMove.Clear();
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var qTrial2 = 2 * qTrials;
      var dRate = qTrial2 / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrial2, dElapsedMS / 1000, dRate);
    }

    protected void timeFoeAtx() { // 1.22 MHz
      const UInt64 qTrials = 100000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing timeFoeAtx() at {0:HH:mm:ss.ff}", t0);

      //var qpMoveTo = KingAtx[(Int32)sq.e4];
      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var nFrom = (Int32)(qTrial % nSquares);
        var qpMoveTo = KingAtx[nFrom];
        qpMoveTo &= ~attacks(Side[Black], qpMoveTo);
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    // diagAtx() 17.837 MHz [56 sec] for Rotations [C++ inline 3.14x faster]
    // rectAtx() 19.365 MHz [52 sec] for Rotations [C++ inline 3.4x faster]
    // KnightAtx[] ~63 MHz [C++ 8.44x faster]
    protected void timeRectAtx() {
      const UInt64 qTrials = 1000000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing rectAtx() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var nFrom = (Int32)(qTrial % nSquares);
        var qpMoveTo = rectAtx(nFrom);
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
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
    protected void timeRemoveLo() {
      const UInt64 qTrials = 1000000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing RemoveLo() at {0:HH:mm:ss.ff}", t0);

      var qBits = 0UL;
      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var qpPiece = (Plane)qTrial;
        while (qpPiece != 0) {
          var nFrom = RemoveLo(ref qpPiece);
          qBits++;
        }
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qBits / dElapsedMS;
      LogLine("Counted {0} bits in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qBits, dElapsedMS / 1000, dRate);
      LogLine("# of trials = {0}", qTrials);
    }

    protected void timeMagic() {
      const UInt64 qTrials = 1000000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing Magic at {0:HH:mm:ss.ff}", t0);

      var n = (Int32)sq.e4;
      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var vRank = rotateRank(n);                    // 11 sec, 90.65 MHz
        //var vFileHalf = hashFileHalf(RankPiece, n);    // 29.48 sec, 33.9 MHz
        //var vA1H8Half = hashA1H8Half(RankPiece, n);    // 30.69 sec, 32.59 MHz
        //var vA8H1Half = hashA8H1Half(RankPiece, n);    // 30.55 sec, 32.74 MHz
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    protected void timeCapturedPiece() {
      const UInt64 qTrials = 1000000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing getPiece() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      var nFrom = (Int32)sq.d1;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        getPiece(nFrom);

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    protected void timeMove(Move mov) {
      const UInt64 qTrials = 100000000UL;
      var sb = new StringBuilder();
      sb.AppendPACN(mov, State.Rule);

      var t0 = DateTime.Now;
      LogLine("Timing {0} at {1:HH:mm:ss.ff}", sb, t0);

      var sw = new Stopwatch();
      sw.Start();

      //~4.5 MHz w resetMove() which is 5.33 times faster at ~24 MHz
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        resetMove();
        var move = mov;
        var bWTM = WTM();
        (BoardSide friend, BoardSide foe) = getSides(bWTM);
        (CastleRuleSide friendRule, CastleRuleSide foeRule) = getRules(bWTM);

        // Calculated to be ~5.54 MHz on old PC, now ~18.5 MHz
        movePiece(friend, friendRule, foe, foeRule, ref move);
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    protected void timeListAdd() {      //~35.56 MHz
      const UInt64 qTrials = 10000000UL;

      var t0 = DateTime.Now;
      LogLine("Timing List.Add() at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        PseudoPawnBelowMove.Add(Move.NullMove);
        if (qTrial % 1000 == 0)
          PseudoPawnBelowMove.Clear();
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    protected void timeStaticLoads() {
      const UInt64 qTrials = 10000UL;

      var t0 = DateTime.Now;
      LogLine("Timing Static Loads at {0:HH:mm:ss.ff}", t0);

      var sw = new Stopwatch();
      sw.Start();

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

        loadDeBruijn(deBruijnByte, 3, vDeBruijn);
#if HalfDeBruijn
        loadDeBruijn(deBruijnHalf, 5, uDeBruijn);
#else
        loadDeBruijn(deBruijnFull, 6, qDeBruijn);
#endif
        loadZobrist();
        //var state = new GameState(null);
        //state.loadEndgameValue();
      }

      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;

      var dRate = qTrials / dElapsedMS;
      LogLine("Completed {0} trials in {1:0.0##} sec, Mean = {2:0.0##} KHz",
              qTrials, dElapsedMS / 1000, dRate);
    }

    internal void TimerTests() {
      //[Time]timeEval();
      //[Time]timeRoots();
      //[Time]timeAddPieceCapturesAndMoves();
      //[Time]timeGenerate(PseudoMoves, NoSwaps);
      //[Time]timeRectAtx();
      //[Time]timeFoeAtx();
      //[Time]timeMagic();
      //[Time]timeRemoveLo();
      //[Time]timeStaticLoads();
      //[Time]timeCapturedPiece();

      //[Test]testPieceMasks();
      //[Test]testOffsets();
      //[Test]testRotations();
      //[Test]writeDepthShallow((PlyDepth)32);
      //[Test]writeDepthDeep((PlyDepth)32);
      //[Test]testPieceMasks();
      //[Test]testPawnAttacks();
      //[Test]
      testOutsideSquare(sq.c1);
      //[Test]
      //for (sq sq = sq.a1; sq <= sq.h8; sq++)
      //  testAtxMasks(sq);
    }

    [Conditional("TestOutsideSquare")]
    protected void testOutsideSquare(sq sq) {
      var n = (Int32)sq;
      foreach (var parameter in Parameter) {
        testRect($"{parameter.SideName}KingToMoveLoss[{sq}]", parameter.KingToMoveLoss[n]);
        testRect($"{parameter.SideName}PawnToMoveWins[{sq}]", parameter.PawnToMoveWins[n]);
      }
    }
    #endregion
  }
}
