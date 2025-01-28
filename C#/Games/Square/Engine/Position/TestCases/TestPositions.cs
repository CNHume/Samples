//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split TestCases into their own file
//
namespace Engine;

using Test;

partial class Position : Board {
  #region Constants
  protected static readonly Tabiya[] TestPositions = {
      new Tabiya {
        Name = "Start Position",
        FEN = sOrthodoxStartFEN,
        PerfCases = new PerfCase[] {
      new PerfCase {
        Plies =  0,
        TotalNodes = 20,
        LeafNodes = 20,
        Captures = 0,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 1,
        TotalNodes = 420,
        LeafNodes = 400,
        Captures = 0,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 2,
        TotalNodes = 9322,
        LeafNodes = 8902,
        Captures = 34,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 12,
        Checkmates = 0 },
      new PerfCase {
        Plies = 3,
        TotalNodes = 206603,
        LeafNodes = 197281,
        Captures = 1576,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 469,
        Checkmates = 8 },
      new PerfCase {
        Plies = 4,
        TotalNodes = 5072212,
        LeafNodes = 4865609,
        Captures = 82719,
        EnPassant = 258,
        Castles = 0,
        Promotions = 0,
        Checks = 27351,
        Checkmates = 347 },
      new PerfCase {
        Plies = 5,
        TotalNodes = 124132536,
        LeafNodes = 119060324,
        Captures = 2812008,
        EnPassant = 5248,
        Castles = 0,
        Promotions = 0,
        Checks = 809099,
        Checkmates = 10828 }
        }
      },
      new Tabiya {
        Name = "Perft2",
        FEN = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
        PerfCases = new PerfCase[] {
      new PerfCase {
        Plies = 0,
        TotalNodes = 48,
        LeafNodes = 48,
        Captures = 8,
        EnPassant = 0,
        Castles = 2,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 1,
        TotalNodes = 2087,
        LeafNodes = 2039,
        Captures = 351,
        EnPassant = 1,
        Castles = 91,
        Promotions = 0,
        Checks = 3,
        Checkmates = 0 },
      new PerfCase {
        Plies = 2,
        TotalNodes = 99949,
        LeafNodes = 97862,
        Captures = 17102,
        EnPassant = 45,
        Castles = 3162,
        Promotions = 0,
        Checks = 993,
        Checkmates = 1 },
      new PerfCase {
        Plies = 3,
        TotalNodes = 4185552,
        LeafNodes = 4085603,
        Captures = 757163,
        EnPassant = 1929,
        Castles = 128013,
        Promotions = 15172,
        Checks = 25523,
        Checkmates = 43 },
      new PerfCase {
        Plies = 4,
        TotalNodes = 197876242,
        LeafNodes = 193690690,
        Captures = 35043416,
        EnPassant = 73365,
        Castles = 4993637,
        Promotions = 8392,
        Checks = 3309887,
        Checkmates = 30171 }
        }
      },
      new Tabiya {
        Name = "Perft3",
        FEN = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
        PerfCases = new PerfCase[] {
      new PerfCase {
        Plies = 0,
        TotalNodes = 14,
        LeafNodes = 14,
        Captures = 1,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 2,
        Checkmates = 0 },
      new PerfCase {
        Plies = 1,
        TotalNodes = 205,
        LeafNodes = 191,
        Captures = 14,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 10,
        Checkmates = 0 },
      new PerfCase {
        Plies = 2,
        TotalNodes = 3017,
        LeafNodes = 2812,
        Captures = 209,
        EnPassant = 2,
        Castles = 0,
        Promotions = 0,
        Checks = 267,
        Checkmates = 0 },
      new PerfCase {
        Plies = 3,
        TotalNodes = 46255,
        LeafNodes = 43238,
        Captures = 3348,
        EnPassant = 123,
        Castles = 0,
        Promotions = 0,
        Checks = 1680,
        Checkmates = 17 },
      new PerfCase {
        Plies = 4,
        TotalNodes = 720879,
        LeafNodes = 674624,
        Captures = 52051,
        EnPassant = 1165,
        Castles = 0,
        Promotions = 0,
        Checks = 52950,
        Checkmates = 0 },
      new PerfCase {
        Plies = 5,
        TotalNodes = 11750962,
        LeafNodes = 11030083,
        Captures = 940350,
        EnPassant = 33325,
        Castles = 0,
        Promotions = 7552,
        Checks = 452473,
        Checkmates = 2733 },
      new PerfCase {
        Plies = 6,
        TotalNodes = 190384623,
        LeafNodes = 178633661,
        Captures = 14519036,
        EnPassant = 294874,
        Castles = 0,
        Promotions = 140024,
        Checks = 12797406,
        Checkmates = 87 }
        }
      },
      new Tabiya {
        Name = "Perft4",
        FEN = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
        PerfCases = new PerfCase[] {
      new PerfCase {
        Plies = 0,
        TotalNodes = 6,
        LeafNodes = 6,
        Captures = 0,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 1,
        TotalNodes = 270,
        LeafNodes = 264,
        Captures = 87,
        EnPassant = 0,
        Castles = 6,
        Promotions = 48,
        Checks = 10,
        Checkmates = 0 },
      new PerfCase {
        Plies = 2,
        TotalNodes = 9737,
        LeafNodes = 9467,
        Captures = 1021,
        EnPassant = 4,
        Castles = 0,
        Promotions = 120,
        Checks = 38,
        Checkmates = 22 },
      new PerfCase {
        Plies = 3,
        TotalNodes = 432070,
        LeafNodes = 422333,
        Captures = 131393,
        EnPassant = 0,
        Castles = 7795,
        Promotions = 60032,
        Checks = 15492,
        Checkmates = 5 },
      new PerfCase {
        Plies = 4,
        TotalNodes = 16265362,
        LeafNodes = 15833292,
        Captures = 2046173,
        EnPassant = 6512,
        Castles = 0,
        Promotions = 329464,
        Checks = 200568,
        Checkmates = 50562 },
      new PerfCase {
        Plies = 5,
        TotalNodes = 722310395,
        LeafNodes = 706045033,
        Captures = 210369132,
        EnPassant = 212,
        Castles = 10882006,
        Promotions = 81102984,
        Checks = 26973664,
        Checkmates = 81076 }
        }
      },
      new Tabiya {
        Name = "Perft5",
        FEN = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1",
        PerfCases = new PerfCase[] {
      new PerfCase {
        Plies = 0,
        TotalNodes = 24,
        LeafNodes = 24,
        Captures = 11,
        EnPassant = 0,
        Castles = 0,
        Promotions = 12,
        Checks = 3,
        Checkmates = 0 },
      new PerfCase {
        Plies = 1,
        TotalNodes = 520,
        LeafNodes = 496,
        Captures = 203,
        EnPassant = 0,
        Castles = 0,
        Promotions = 252,
        Checks = 59,
        Checkmates = 0 },
      new PerfCase {
        Plies = 2,
        TotalNodes = 10003,
        LeafNodes = 9483,
        Captures = 2921,
        EnPassant = 0,
        Castles = 0,
        Promotions = 3224,
        Checks = 992,
        Checkmates = 0 },
      new PerfCase {
        Plies = 3,
        TotalNodes = 192841,
        LeafNodes = 182838,
        Captures = 50647,
        EnPassant = 0,
        Castles = 0,
        Promotions = 65620,
        Checks = 20154,
        Checkmates = 0 },
      new PerfCase {
        Plies = 4,
        TotalNodes = 3797944,
        LeafNodes = 3605103,
        Captures = 754747,
        EnPassant = 0,
        Castles = 0,
        Promotions = 955220,
        Checks = 399962,
        Checkmates = 134 },
      new PerfCase {
        Plies = 5,
        TotalNodes = 74977083,
        LeafNodes = 71179139,
        Captures = 13902699,
        EnPassant = 0,
        Castles = 0,
        Promotions = 19191520,
        Checks = 8130299,
        Checkmates = 3308 }
        }
      },
      new Tabiya {
        Name = "218 Move Position",
        FEN = "1BK1NNBk/4Q1pp/2Q4Q/Q4Q2/3Q4/1Q4Q1/4Q3/R6R w - - 0 1",
        PerfCases = new PerfCase[] {
      new PerfCase {
        Plies = 0,
        TotalNodes = 218,
        LeafNodes = 218,
        Captures = 9,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 7,
        Checkmates = 7 },
      new PerfCase {
        Plies = 1,
        TotalNodes = 327,
        LeafNodes = 109,
        Captures = 42,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 2,
        TotalNodes = 21681,
        LeafNodes = 21354,
        Captures = 857,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 1984,
        Checkmates = 1610 },
      new PerfCase {
        Plies = 3,
        TotalNodes = 52043,
        LeafNodes = 30362,
        Captures = 5569,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 4,
        TotalNodes = 5640953,
        LeafNodes = 5588910,
        Captures = 221339,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 578631,
        Checkmates = 482053 },
      new PerfCase {
        Plies = 5,
        TotalNodes = 13154679,
        LeafNodes = 7513726,
        Captures = 1127908,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 0,
        Checkmates = 0 },
      new PerfCase {
        Plies = 6,
        TotalNodes = 1323011035,
        LeafNodes = 1309856356,
        Captures = 49805417,
        EnPassant = 0,
        Castles = 0,
        Promotions = 0,
        Checks = 154171908,
        Checkmates = 114185948 }
        }
      },
      new Tabiya {
        Name = "218 Move Position",
        FEN = "1BK1NNBk/4Q1pp/2Q4Q/Q4Q2/3Q4/1Q4Q1/4Q3/R6R w - - 0 1" },
      new Tabiya {
        Name = "Q v BP draw",
        FEN = "8/8/4Q3/5K2/8/8/2p5/k7 w - - 0 1" },
      new Tabiya {
        Name = "Q v RP draw",
        FEN = "8/8/8/4QK2/8/8/p7/2k5 b - - 0 1" },
      new Tabiya {
        Name = "Légal Trap",
        FEN = "r2qkbnr/ppp2ppp/2np4/4p2b/2B1P3/2N2N1P/PPPP1PP1/R1BQK2R w KQkq - 0 6" },
      new Tabiya {
        Name = "Fischer v Panno 1970",  // 16-ply in 2:49:43 to find 28. Be4!
        FEN = "2nq1nk1/5p1p/4p1pQ/pb1pP1NP/1p1P2P1/1P4N1/P4PB1/6K1 w - - 0 28" },
      new Tabiya {
        Name = "Q v R Philidor #10",  // 13-ply in 47.8 sec
        FEN = "4Q3/6rk/5K2/8/8/8/8/8 w - - 0 1" },
      new Tabiya {
        Name = "R v B Philidor from Rc3 #18",  // 18-ply in 45:30
        FEN = "3k4/1R6/3K4/8/8/1Br5/8/8 w - - 0 4" },
      new Tabiya {
        Name = "Xiong v Nakamura (0-1) 2019-03-31 US Championship R11 -#8", // 12-ply in 1:39.1 sec
        FEN = "8/2N5/5R1p/2pP4/1pP3pP/1P2k3/r3n3/7K b - - 0 55" },
      new Tabiya {
        Name = "Hou Yifan v Kacper Piorun 2017-09-04",  // 13-ply in 4:21 to find 37. g4 Qc6
        FEN = "4Qb1k/6pp/8/5r2/pn1B4/5N2/1Pq3PP/5RK1 w - - 0 37" },
      new Tabiya {
        Name = "Paehtz v Hou Yifan KBN #12",  // 18-ply in 60.66 sec
        FEN = "8/8/8/8/8/5k2/3n3b/4K3 b - - 0 79" },
      new Tabiya {
        Name = "Wrong Bishop",
        FEN = "k7/7p/8/2nKb3/8/8/8/8 w - - 0 1" },
      new Tabiya {
        // 62. Nd7 Rf5 63. Rf8+ Kg6 64. Rg8+ Kf7 65. Ke4 Ra5 66. Rf8+ Kg7
        // [66... Kg6?! 67. Ne5+! Kxg5 68. Rf5+ Kh6 [68... Kh4 69. Ng6+] 69. Ng4+ Kg6 70. Rxa5]
        // 67. Kf4 Ra4+ 68. Kf5 Rd4 69. Rd8 Rd5+ 70. Kg4 Kf7 71. Rf8+ Kg7 72. Rg8+ Kf7 73. Nf6
        Name = "Kramnik v Vachier-Lagrave 2013-08-27",
        FEN = "1R3N2/5k2/8/6P1/8/4K3/8/5r2 w - - 0 62" },
      new Tabiya {
        Name = "Vancura 1922",          // J. Vancura 1922, Ceske Slovo (care of Frederic Friedel) [12-ply in 1.86]
        FEN = "8/8/8/8/8/2K4B/5k1P/8 w - - 0 1" },
      new Tabiya {
        Name = "Carlsen v Topalov 2015-06-16",  // 0-1 R1 Time Forfeit, 13-ply in 5:19
        FEN = "1b1Q4/1P6/6k1/1B6/5p2/6PK/5q2/8 w - - 0 66" },
      new Tabiya {
        Name = "Reinfeld Combo #357",   // #357 of Fred Reinfeld's 1001 Winning Chess Sacrifices and Combinations
        FEN = "6k1/p4R1p/1p5q/5Q2/2Pb4/8/P6P/7K w - - 0 1" },
      new Tabiya {
        Name = "Khismatullin v Eljanov 2015-03-06",     // 0.00 at 12-ply, 3:00 [0.30 at 13-ply, ~30:00]
        FEN = "5Q2/5p1p/1pPr2p1/6k1/8/3pP2P/2q2PP1/3R1K2 w - - 0 44" },
      new Tabiya {
        Name = "Taimanov v Kuzminykh 1950",     //+3.85 at 14-ply, 11:48:15
        FEN = "r1qr2k1/1p3pp1/2pbbn2/p3N3/3P3Q/P5PB/1B3P2/R3R1K1 w - - 0 1" },
      new Tabiya {
        Name = "Jensen v Urkedal 2013 #10",     // 13-ply in 5:42 [w 6 checks]
        FEN = "rnb3nr/pppp1k1p/3b2q1/7Q/5B2/8/PPP3PP/RN3R1K w - - 0 14" },
      new Tabiya {
        Name = "Kramnik v Meier 2012-07-22",    // 13-ply in 7:18:41
        FEN = "r1bqr1k1/pp3ppp/2nB4/1N6/2Bn4/8/PP4PP/2RQR1K1 w - - 0 24" },
      new Tabiya {
        Name = "Caruana v Gustafsson 2012-07-17",       //>12-ply
        FEN = "q1r3k1/5p1p/6pB/1p6/2bN4/2P1Q2P/5P2/r2BR1K1 w - - 0 35" },
      new Tabiya {
        Name = "Caruana v Gustafsson 2012-07-17 #8",    // #8, 12-ply in 55
        FEN = "5rk1/5p1p/5Qp1/1p6/3N4/2P4P/5P2/r2B1RK1 w - - 0 39" },
      new Tabiya {
        Name = "Botvinnik v Capablanca 1938 AVRO R11",  // 16-ply in 6:14:06 [14.152 Gnode]
        FEN = "8/p3q1kp/1p2Pnp1/3pQ3/2pP4/1nP3N1/1B4PP/6K1 w - - 0 30" },
      new Tabiya {
        Name = "Ninov v Colovic 2015",
        FEN = "r4rk1/p4pb1/bp5p/2pNqPp1/6P1/7Q/PPP3P1/2KR2BR w - - 0 1" },
      new Tabiya {
        Name = "Pin and Tempo Combo",   // Pin and Tempo [Edouard-Novikov 2011-02-13]
        FEN = "5k2/3n1p2/4p1rp/2q1PN2/1Q6/8/1P3PP1/4R1K1 w - - 0 1" },
      new Tabiya {
        Name = "Benko Mate",            // Pal Benko for Bobby Fischer, 6-ply
        FEN = "8/8/8/8/4k3/8/8/2BQKB2 w - - 0 1" },
      new Tabiya {
        Name = "Aronian v Caruana 2012",
        FEN = "8/5pk1/5np1/2p4p/2Pp4/P3p1P1/3qB2P/1Q3RK1 w - - 0 38"  },
      new Tabiya {
        Name = "Website Mate",
        FEN = "8/4R3/1pN2p2/1N1k4/1P6/4p1P1/4B3/4K3 w - - 0 1" },
      new Tabiya {
        Name = "Kasparov v Ribli 1989",
        FEN = "5rk1/5ppp/p1Q1p3/1r6/q2b4/4B1P1/P2RPP1P/1R4K1 w - - 0 26" },
      new Tabiya {
        Name = "Abbrev",
        FEN = "8/7p/8/8/4N1K1/2q5/6k1/1N6 w - - 0 1" },
      new Tabiya {
        Name = "Draw3",
        FEN = "2q1rrk1/5p2/6p1/8/2B5/8/2Q3P1/6K1 w - - 0 1" },
      new Tabiya {
        Name = "Radjabov v Karjakin 2012",
        FEN = "8/5p2/1P4p1/7p/r2kp2P/2RbN1P1/5P1K/8 w - - 0 61" },
      new Tabiya {
        Name = "Unzicker v Averbakh",   // 11-ply, see below
        FEN = "5rk1/1rP3pp/p4n2/3Pp3/1P2Pq2/2Q4P/P5P1/R3R1K1 b - - 0 1" },
      new Tabiya {
        Name = "DrawQvBP",
        FEN = "8/k1p/5K3/5Q3/8/8/8/8 w - - 0 1" },
      new Tabiya {
        Name = "DrawRvBB",
        FEN = "b1R1b3/8/8/b3k3/8/8/K3b3/8 w - - 0 1" },
      new Tabiya {
        Name = "DrawN",
        FEN = "8/8/4K3/8/8/2n5/3k4/8 w - - 0 1" },
      new Tabiya {
        Name = "Byrne v Fischer 35",    // 35... Bc5+ { #9, 11-ply }
        FEN = "1Q3b2/5pk1/2p3p1/1p1bN2p/4n2P/8/r5P1/6K1 b - - 0 35" },
      new Tabiya {
        Name = "Byrne v Fischer 17",    // 17... Be6!!
        // 17... Bg4-e6 18. Qa3xNc3! { best at 11-ply, 6:15 } Qb6xBc5 19. d4xc5 Bg7xQc3 20. Bc4xBe6 f7xe6
        // 21. Nf3-e1 Bc3xNe1 22. Rd1xBe1 Ra8-b8 23. Kf1-g1 Rb8-c8 24. Re1-a1 { 14-ply, 04:50:22 }
        FEN = "r3r1k1/pp3pbp/1qp3p1/2B5/2BP2b1/Q1n2N2/P4PPP/3R1K1R b - - 0 17" },
      new Tabiya {
        Name = "Byrne v Fischer 15",    // 15... Nxc3!, 14-ply in 23:22:50
        FEN = "r4rk1/pp2Bpbp/1qp3p1/8/2BPn1b1/Q1P2N2/P4PPP/3RK2R b K - 0 15" },
      new Tabiya {
        Name = "Byrne v Fischer 13",    // 13... Nxe4!, >12-ply
        FEN = "r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13" },
      new Tabiya {
        Name = "Byrne v Fischer 11",    // 11... Na4!!, >11-ply
        FEN = "r2q1rk1/pp2ppbp/1np2np1/2Q3B1/3PP1b1/2N2N2/PP3PPP/3RKB1R b K - 0 11" },
      new Tabiya {
        Name = "Johannessen v Fischer", // 26... Rxf4!? 16-ply
        FEN = "r6k/pb2pr1p/2q3p1/2p1b1B1/5N1Q/6PP/PP4B1/R5K1 b - - 0 26" },
      new Tabiya {
        Name = "Johannessen v Fischer Line",
        // arises after 26... Rxf4!? 27.Bxc6 Bd4+ 28.Kh2 Rf2+ 29.Kg1 Rxb2+ 30.Kf1?! Bxc6 14-ply
        FEN = "r6k/p3p2p/2b3p1/2p3B1/3b3Q/6PP/Pr6/R4K2 w - - 0 31" },
      new Tabiya {
        Name = "Johannessen v Fischer #8",      // #8, 11-ply in 14:53
        // 31. Bxe7 Bb5+ 32. Ke1 Re8 33. a4 Bc4 34. Kd1 Bb3+ 35. Kc1 Rc2+ 36. Kd1 Bxa1 37. Qf4 Bc3 38. Bf6+ Bxf6
        // 39. Qxf6+ Kg8 40. Qf1 Rf2+ 41. Kc1 Rxf1+ 42. Kb2 Rf3 43. Ka1 Rf2 44. Kb1 Re1# { 12-ply in 1:32:48 }
        FEN = "4r2k/p3B2p/6p1/2p5/P6Q/1b4PP/2r5/b2K4 w - - 0 37" },
      new Tabiya {
        Name = "Mavo Nice Mate1 #6",            // #6, 11-ply in 92 sec
        FEN = "1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PPK/8 w - - 0 4" },
      new Tabiya {
        Name = "Aronian v Caruana Line",        // Horizon beyond 14-ply in 1:10:14 [advanced pawn couple]
        FEN = "8/4Qpk1/5np1/2p4p/2Pp4/P3p1P1/4q2P/5RK1 b - - 0 39" },
      new Tabiya {
        Name = "Hamppe v Meitner",              // 7-ply
        FEN = "r1bk3r/2p2ppp/1pK5/p2pp3/8/P7/1PPP2PP/R1BQ2NR b - - 0 1" },
      new Tabiya {
        Name = "Nakamura v Adams 2011",         // 11-ply in ~39 min
        FEN = "5r1k/2P4p/1q2Np2/1r1P4/6p1/2Q1Rb2/1p5P/4R1K1 b - - 0 41" },
      new Tabiya {
        Name = "Nakamura v Adams 2011 Line",
        FEN = "5r1k/2P4p/1q2Np2/3r4/6p1/2Q1Rb2/1p5P/4R1K1 w - - 0 42" },
      new Tabiya {
        Name = "1964 Bielicki v Smyslov (0-1) Key",     // 15-ply in <14h
        FEN = "3r2k1/1p3pp1/p1p5/8/1P2r3/P3PqBp/2Q2P1P/R3RK2 b - - 0 29" },
      new Tabiya {
        Name = "1964 Bielicki v Smyslov (0-1) Line",    // 15-ply
        FEN = "6k1/1p3pp1/p1p5/8/1P6/P3PqBp/2R2P1P/3rRK2 w - - 0 32" },
      new Tabiya {
        Name = "Réti v Tartakower 1910",
        FEN = "rnb1kb1r/pp3ppp/2p5/4q3/4n3/3Q4/PPPB1PPP/2KR1BNR w kq - 0 9" },
      new Tabiya {
        Name = "Réti Draw",
        FEN = "7K/8/k1P5/7p/8/8/8/8 w - - 0 1" },
      new Tabiya {
        Name = "KBN v K #9",            // 14-ply for #9 in 22.1 sec
        FEN = "8/5b2/8/8/5knK/8/8/8 w - - 0 13" },
      new Tabiya {
        Name = "Stamma Mate",
        FEN = "8/3N4/8/8/8/p7/k2K4/8 w - - 0 1" },
      new Tabiya {
        Name = "Interference Mate",     // Interference [Mastrovasillis-Marechal 2011-02-26]
        FEN = "5rk1/p3np1n/2q1r2p/1p3N1Q/3B2P1/3P3P/P4R1K/8 w - - 0 1" },
      new Tabiya {
        Name = "Chekhov's Gun",         // [Zwischenzug followed by Zugzwang] 9-ply
        FEN = "8/K6N/8/2N5/1n6/6Q1/6pn/7k w - - 0 1" },
      new Tabiya {
        Name = "R+2P v R Mate",         // Mate in 7
        FEN = "r6k/8/5KPP/4R3/8/8/8/8 w - - 0 1" },
      new Tabiya {
        Name = "ep Mate",
        FEN = "k1n1K3/p7/8/B2pP3/8/8/6B1/8 w - d6 0 1" },
      new Tabiya {
        Name = "KKR No Move",
        FEN = "5k1R/8/5K2/8/8/8/8/8 b - - 0 1" },
      new Tabiya {
        Name = "KKR Mate",
        FEN = "5k2/7R/4K3/8/8/8/8/8 w - - 0 1" },
      new Tabiya {
        Name = "Position",
        FEN = "8/3Pp2p/6Pk/pP2p3/Pp1bPp2/2n1Q3/2PP1K1P/8 w - a6 0 1" },
      new Tabiya {
        Name = "PositionA",
        FEN = "8/3Pp2k/8/pP2p3/Pp1QPp2/2n5/2PP1K1P/8 b - a3 0 1" },
      new Tabiya {
        Name = "PositionB",
        FEN = "8/3Pp2k/8/pP2p3/Pp1bP3/2n1p3/2PP2KP/8 b - - 0 1" },
      new Tabiya {
        Name = "Perpetua",              // 19-ply in 30:20
        FEN = "8/8/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1" },
      new Tabiya {
        Name = "Perpetua0",             // 18-ply in 15:40
        FEN = "7r/8/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1" },
      new Tabiya {
        Name = "Perpetua1",
        FEN = "8/7r/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1" },
      new Tabiya {
        Name = "Perpetua2",
        FEN = "8/8/7r/pP2pQ1k/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1" },
      new Tabiya {
        Name = "CastlingPin",
        FEN = "r3k2r/ppp1pppp/8/8/3N4/8/PPP1PPPP/R2K3R b kq - 0 1" },
      new Tabiya {
        Name = "CastlingAttacks",
        FEN = "r3k2r/pppPpppp/8/8/8/8/PPP1PPPP/R3K2R b KQkq - 0 1" },
      new Tabiya {
        Name = "Castling",
        FEN = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1" }
    };

  /*
   * Solution to Unzicker v Averbakh Draw [now found in 11-ply]:
   *
   * 1... Rxc7! 2. Qxc7 Ng4 3. hxg4 Qf2+= 4. Kh2 Qh4+ 5. Kg1 Qf2+ 6. Kh1 Qh4+ 7. Kg1 Qf2+ {3X}
   *
   * For Website Mate, see http://www.usefulchess.com/problems/chessproblem3_2.html
   *
   * Full Solution:
   *
    1. Rg7 Ke4

      1. ... f5 2. Rg6 Ke4 3. Nc3 1-0
      1. ... Ke4 2. Nc3+ Kf5 3. Bg4 1-0
      1. ... Kxc6 2. Bc4 f6 3. Rc7 1-0
      1. ... Ke6 2. Bc4+ Kf5 3. Nd6 1-0

      2. Nc3+ Kf5 3. Bg4 1-0
   */
  #endregion
}
