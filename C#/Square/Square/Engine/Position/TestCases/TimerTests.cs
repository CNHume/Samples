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
#define TimePlayMove

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Text;

  using CacheValue;

  using Command;

  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using CompositionCounter = UInt16;
  using Eval = Int16;

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
      //[Time]timeMove((Move)0x00140759);       //[Perft3]b4f4
      //[Time]timeMove((Move)0x0001078E);       //[Perft3]g2g4 with tryEP()
      //timeWeighPieces();
      //[Time]timeGenerate(PseudoMoves, NoSwaps);
      //[Time]
      timeAddPieceCapturesAndMoves();
      //[Time]timeAddPawnCapturesAndMoves();
      //[Time]timeOrthAtx();
      //[Time]timeSafe();
      //[Time]timeMagic();
      //[Time]
      //timeRemoveLo();
      //[Time]timeStaticLoads();
      //[Time]timeCapturedPiece();
      //[Test]timeExecute("test", 100000);
    }

    private static Stopwatch timerStart(string sMethod, UInt64 qTrials) {
      LogLine($"Timing {qTrials:n0} {sMethod} trials at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
      var sw = new Stopwatch();
      sw.Start();
      return sw;
    }

    private static void timerStop(Stopwatch sw, UInt64 qTrials) {
      sw.Stop();
      var dElapsedMS = (Double)sw.ElapsedMilliseconds;
      var dRate = qTrials / dElapsedMS;
      LogLine($"Completed {qTrials:n0} trials in {dElapsedMS / 1000:0.0##} sec, Rate = {dRate:0.0##} KHz");
    }

    [Conditional("TestOutsideSquare")]
    private void testOutsideSquare(sq sq) {
      var n = (Int32)sq;
      foreach (var parameter in Parameter) {
        testOrth($"{parameter.SideName}KingToMoveLoss[{sq}]", parameter.KingToMoveLoss[n]);
        testOrth($"{parameter.SideName}PawnToMoveWins[{sq}]", parameter.PawnToMoveWins[n]);
      }
    }

    private void everyRoot() {
      for (UInt16 w = 0; w < UInt16.MaxValue; w++) {
        var root = ISqrt(w);
      }
    }
    #endregion

    #region Timer Tests
    private void timeRoots(UInt64 qTrials = 100000UL) {
      //verifyISqrt();
      var sw = timerStart(nameof(everyRoot), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        everyRoot();

      timerStop(sw, qTrials);
    }

    private void timeEval(UInt64 qTrials = 100000000UL) {
      var sw = timerStart(nameof(staticEval), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        staticEval(out PawnPosition? pp);

      timerStop(sw, qTrials);
    }

    private void timeWeighPieces(UInt64 qTrials = 1000000000UL) {
      var sw = timerStart(nameof(weighPieces), qTrials);

      CompositionCounter wPieceCounts = default;
      var fsideWeight = SideFlags.Pair;

      Eval value = default;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        value = weighPieces(wPieceCounts, fsideWeight);

      timerStop(sw, qTrials);
    }

    // Generates moves at ~18 MHz
    private void timeGenerate(List<Move> moves, Boolean bSwap, UInt64 qTrials = 10000000UL) {
      var sw = timerStart($"{nameof(generate)}({bSwap})", qTrials);

      var qMoveCount = 0UL;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++)
        qMoveCount += (UInt32)generate(moves, bSwap);

      timerStop(sw, qTrials);
    }

    // 13 MHz
    private void timeAddPieceCapturesAndMoves(UInt64 qTrials = 10000000UL) {
      var sw = timerStart(nameof(addPieceCapturesAndMoves), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        addPieceCapturesAndMoves2(MASK64);
        clearPseudoCaptures();
        clearPseudoMoves();
      }

      timerStop(sw, qTrials);
    }

    private void timeAddPawnCapturesAndMoves(UInt64 qTrials = 10000000UL) {      //~2690 KHz
      var sw = timerStart(nameof(BoardSide.AddPawnMoves), qTrials);
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

      timerStop(sw, qTrials);
    }

    private void timeSafe(UInt64 qTrials = 100000000UL) { // 1.22 MHz
      var sw = timerStart(nameof(BoardSide.Safe), qTrials);
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      //var qpMoveTo = AtxKing[(Int32)sq.e4];
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var nFrom = (Int32)(qTrial % nSquares);
        var qpMoveTo = AtxKing[nFrom];
        qpMoveTo &= blackSide.Safe(qpMoveTo);
      }

      timerStop(sw, qTrials);
    }

    // RayDiag() 17.837 MHz [56 sec] for Rotations [C++ inline 3.14x faster]
    // RayOrth() 19.365 MHz [52 sec] for Rotations [C++ inline 3.4x faster]
    // AtxKnight[] ~63 MHz [C++ 8.44x faster]
    private void timeOrthAtx(UInt64 qTrials = 1000000000UL) {
      var sw = timerStart(nameof(RayOrth), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var nFrom = (Int32)(qTrial % nSquares);
        var qpMoveTo = RayOrth(nFrom);
      }

      timerStop(sw, qTrials);
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
    private void timeRemoveLo(UInt64 qTrials = 1000000000UL) {
      //Int32 n;
      //n = BitOperations.TrailingZeroCount((UInt64)0);
      //n = BitOperations.TrailingZeroCount((UInt32)0);
      //n = BitOperations.TrailingZeroCount((UInt16)0);
      //n = BitOperations.TrailingZeroCount((Byte)0);
      var sw = timerStart(nameof(RemoveLo), qTrials);
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

      timerStop(sw, qTrials);
    }

    private void timeMagic(UInt64 qTrials = 1000000000UL) {
      var sw = timerStart(nameof(rotateRank), qTrials);
      var n = (Int32)sq.e4;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        var vRank = rotateRank(n);                    // 11 sec, 90.65 MHz
        //var vFileHalf = hashFileHalf(RankPiece, n);    // 29.48 sec, 33.9 MHz
        //var vA1H8Half = hashA1H8Half(RankPiece, n);    // 30.69 sec, 32.59 MHz
        //var vA8H1Half = hashA8H1Half(RankPiece, n);    // 30.55 sec, 32.74 MHz
      }

      timerStop(sw, qTrials);
    }

    private void timeCapturedPiece(UInt64 qTrials = 1000000000UL) {
      var sw = timerStart(nameof(GetPieceIndex), qTrials);
      var nFrom = (Int32)sq.d1;
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        GetPieceIndex(nFrom);
      }

      timerStop(sw, qTrials);
    }

    //
    //[Perft3]b4f4 13.3 MHz
    //[Perft3]g2g4 9.57 MHz 39% slower with tryEP()
    //
    [Conditional("TimePlayMove")]
    private void timePlayMove(Move mov, UInt64 qTrials = 100000000UL) {
      var sbMove = new StringBuilder();
      sbMove.AppendPACN(mov, Side, State!.IsChess960);
      var sw = timerStart($"{nameof(PlayMove)}({sbMove})", qTrials);

      // ~15 Mhz
      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        resetMove();
        var move = mov;
        PlayMove(ref move);
      }

      timerStop(sw, qTrials);
    }

    private void timeMove(Move move, UInt64 qTrials = 100000000UL) {
      var child = Push();               // Push Position to make the moves
      try {
        child.timePlayMove(move, qTrials);
      }
      finally {
        Pop(ref child);                 // Pop Position used for this Timer Test
      }
    }

    private void timeListAdd(UInt64 qTrials = 10000000UL) {      //~35.56 MHz
      var sw = timerStart(nameof(List<Move>.Add), qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        PseudoPawnBelowMove.Add(Move.NullMove);
        if (qTrial % 1000 == 0)
          PseudoPawnBelowMove.Clear();
      }

      timerStop(sw, qTrials);
    }

    private void timeStaticLoads(UInt64 qTrials = 10000UL) {
      var sw = timerStart("Static Load", qTrials);

      for (var qTrial = 0UL; qTrial < qTrials; qTrial++) {
        loadRankOffset();
#if Magic
        newMagic();
        loadMagic();
#else
        loadRotation();
        loadOrthBit();
        loadDiagBit();
#endif
        colorSquares();
        loadPieceAtx();

        loadOrthAtx();
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

      timerStop(sw, qTrials);
    }

    private void timeExecute(String sInput, UInt64 qTrials) {
      using (var command = new UCI()) {
        var sw = timerStart(nameof(command.Execute), qTrials);
        var qTrial = 0UL;
        for (var bContinue = true; bContinue && qTrial < qTrials; qTrial++) {
          bContinue = command.Execute(sInput);
        }

        timerStop(sw, qTrial);
      }
    }
    #endregion
  }
}
