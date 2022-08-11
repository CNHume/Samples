//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2014-09-07 CNHume]Created File
//
// Conditionals:
//
//#define UseScanMethods

namespace Command {
  using Engine;                         // For GameState

  using System;

  partial class UCI {
    #region FEN Constants
    private const String sDefaultFEN =
    //
    // Perft FEN
    // ---------
    //"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"// Perft1 (startpos) in 24 sec @5,428 MHz over 130.36 Mnode
    //"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"; // Perft2 in 30.475 sec @6.747 MHz over 205.63 Mnode
    //"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";  // Perft3 in 41.432 sec @5.282 MHz over 218.84 Mnode
    //"r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"; // Perft4 in 2:02.5 @6.2628 MHz over 767.28 Mnode
    //"n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1";  // Perft5 in 17.18 sec @5.1 MHz over 87.6 Mnode
    //"";
    //"k1b5/pppN4/1R6/8/Q6K/8/8/8 w - - 0 1"; // #2 [4-ply 3,541 node]
    //"7k/6p1/3P3p/p7/P3Q1P1/8/6PK/3q4 w - - 0 46"; // Jordan Van Foreest v Mamedyarov #16 Line 2022 Oslo Esports Cup 2022-04-27
    // [18-ply in 7:05 @1.517 over 644.9 Mnode] eval 11.1 after:
    // 46. Qe7 Qxg4 47. d7 Qh5+ 48. Kg3 Qg6+ 49. Kf4 Kh7 50. d8=Q Qb1 51. Qd5 Qb4+
    // 52. Qde4+ Qxe4+ 53. Qxe4+ Kg8 54. Qe8+ Kh7 55. Qd7 Kg8 56. Qe6+ Kh7 57. Qe8 h5
    // [20-ply in 1:01:57 @1.581 MHz over 5.877 Gnode] eval 11.65 after:
    // 46. Qe7 Qxg4 47. d7 Qh5+ 48. Kg3 Qg6+ 49. Kf4 Kh7 50. d8=Q Qb1 51. Qe5!? Qf1+ 52. Kg3 Qb1 53. Qed6 Qb3+
    // 54. Kh2 Qc4 55. Qe5 Qg8 56. Qd8xa5
    //
    // Enabling QuietMate avoided a quiet() stalemate anomaly [19-ply trace in 22:10] maintains eval 9.95 after:
    // 52. Qf5 Qxf5+ 53. Kxf5 h5 54. Kg5 h4 55. Kxh4 Kg6 56. Qg5+ Kf7 57. Qxa5
    // Disabling QuietMate exhibited the anomaly [19-ply trace in 35:28 @1.226 MHz] eval 11.0 after:
    // 49... Kh7 50. Qe4 Qxe4+ 51. Kxe4 Kg6 52. d8=Q h5 53. Qxa5 Kh6 54. Qb6+ Kg5 55. Qd8+ Kg4 56. Qd7+ Kh4 57. Qxg7 stalemate
    //"8/6Q1/8/7p/P3K2k/8/6P1/8 b - - 0 57"; // stalemate
    //
    //"8/6B1/1N4kP/2p5/2P5/1p6/2r5/1K6 w - - 0 56"; // Jordan Van Foreest v Liem Quan Le Line1 2022 Oslo Esports Cup 2022-04-28
    //"8/3N2Bk/7P/2p5/2P5/1p6/2r5/1K6 w - - 0 57"; // Jordan Van Foreest v Liem Quan Le #21 Line2 2022 Oslo Esports Cup 2022-04-28
    //"8/8/8/8/5b2/5k2/3p3p/5N1K b - - 0 1"; // Underpromotion -#11 [17-ply in 1:37.8 @1.495 MHz over 146.2 Mnode]
    // 1... d1=B 2. Nxh2+ Kg3 3. Kg1 Be3+ 4. Kf1 Bh5 5. Ke1 Kxh2 6. Kf1 Bg4 7. Ke1 Kg3 8. Kf1 Be3d2
    //"2r5/5pk1/6p1/6P1/1K6/8/3R4/8 b - - 0 66"; // Vidit Gujrathi v Rameshbabu Praggnanandhaa Endgame 2022 Tata Steel Masters R10 2022-01-26
    //"3r2k1/2pqnpp1/1p5p/p2P4/P2p1rP1/2P1N1P1/1PQ2P2/3RR1K1 b - - 0 23"; // Nepomniachtchi v Carlsen 2021 WCC R11 2021-12-10 [~22-ply]
    // moves d4e3 g3f4 d7g4 g1f1 g4h3 f1g1
    //"3r2k1/2p1npp1/1p5p/p2P4/P4P2/2P1p2q/1PQ2P2/3RR1K1 b - - 0 26"; // Nepomniachtchi v Carlsen 2021 WCC R11 2021-12-10 Line
    // moves e3f2 c2f2 d8d6 f2f1 d6g6 g1f2 h3h2 f2e3 g6g3 e3d4 e7f5 d4e4 h2c2 d1d3 f5d6 e4d4
    //"6k1/4bpp1/1p1p4/5R1P/4PQ2/5P2/r4q1P/2R4K w - - 0 49"; // Carlsen v Karjakin 2016 WCC R16 2016-11-30 #8
    // [12-ply in 17.27 sec @1.516 MHz over 26.18 Mnode] #8 after:
    // 49. Rc8+ Bf8 50. Rxf8+ Kxf8 51. Rxf7+ Ke8 52. Rf8+ Kd7 53. Qf5+ Kc6 54. Rc8+ Kb7 55. Qd7+ Ka6 56. Ra8#
    //"2rq1rk1/pb1n1ppN/4p3/1pb5/3P1Pn1/P1N5/1PQ1B1PP/R1B2RK1 b - - 0 16"; // Aronian v Anand 2013-01-16 Tata Steel [13-ply to find 16... Nde5!]
    //"5R2/8/8/4k3/4p3/2r3P1/5P2/5K2 b - - 0 1"; // Firouzja v Mamedyarov
    //"8/8/8/6N1/8/7R/1K2PRn1/3q2k1 w - - 0 1"; // Zugzwang Threat [7-ply 0.53 sec]
    //"kr6/ppq3b1/2pNQ1p1/7p/7P/1R4P1/P4PB1/3n2K1 w - - 0 1"; // Awonder Liang vs Gunay Mammadzada 2021-04-08 #10
    // [16-ply in 14:36 over 1.4341 Gnode @1.6373 MHz] eval 13.0 [17-ply] eval 14.2 [18-ply in 40:59 @1.529 MHz over 3.76 Gnode] #10 after:
    // 1. Rxb7 Rxb7 2. Qe8+ Qb8 3. Qxc6 Nc3 4. Nxb7 Ne2+ 5. Kh2 Nd4 6. Qd7 Qxg3+ 7. fxg3 Nf3+ 8. Bxf3 Be5 9. Qc8+ Bb8 10. Na5#
    //"rn3rk1/pbppq1pp/1p2pb2/4N2Q/3PN3/3B4/PPP2PPP/R3K2R w KQ - 0 11"; // Edward Lasker v George Alan Thomas 1912-10-29 #7 [10-ply 26.35 sec @1.2 MHz over 32 Mnode]
    //"rn3r2/pbppq1p1/1p2pN2/8/3P2NP/6P1/PPP1BP1R/R3K1k1 w Q - 5 18"; // Lasker v Thomas 1912-10-29 #1
    //"1BK1NNBk/4Q1pp/2Q4Q/Q4Q2/3Q4/1Q4Q1/4Q3/R6R w - - 0 1"; // 218 Move Position
    //"R6R/4Q3/1Q4Q1/3Q4/Q4Q2/2Q4Q/4Q1pp/1BK1NNBk w - - 0 1"; // 218 Move Position Reflected
    //"8/8/8/2k5/2P5/2N5/2N3Q1/3K4 w - - 0 1"; // Mrs. W.J. Baird (1859-1924) #3
    //"4br1b/2PpBkPR/3p3P/3P1N2/8/8/8/6K1 w - - 0 1"; // Wolfgang Pauly, Deutsche Schachzeitung 1901 #2  [4-ply]
    // 1. Ng3 Rg8 (1... Kg6 2. gxf8=N#) (1... Kg8 2. gxh8=R#) 2. gxh8=N#
    //"r3k3/8/8/8/8/8/8/4K2R w Kq - 0 1"; // Castling Test Position
    //"7K/8/k1P5/7p/8/8/8/8 w - - 0 1"; // Réti Draw" [10-ply]
    //"8/8/4Q3/5K2/8/8/2p5/k7 w - - 0 1"; // Q v BP draw
    //"8/8/8/4QK2/8/8/p7/2k5 b - - 0 1";  // Q v RP draw
    //"8/8/8/3K4/4Q3/5p2/5k2/8 b - - 0 4";  // Q v RP #8
    //"8/8/8/8/5K2/8/4Qp2/6k1 w - - 0 8";  // Q v RP #4
    //"8/1KQ5/8/8/8/5p2/6k1/8 w - - 0 1";  // Q v RP loss for b
    //"8/6K1/5P2/8/8/8/1kq5/8 b - - 0 1";  // Q v RP loss for w
    //"8/8/8/6Q1/4K3/7k/5p2/8 w - - 0 8";  // Q v RP stalemate
    //"2r1rnk1/1p4bp/p2pp1p1/q5P1/2PN1P1Q/1P1nB2R/P5BP/5RK1 w - - 0 1"; // Chesney (2435) v Craig Mar (2516), Lera 1989.  [14-ply to find 1. f5!]
    //"3k4/1P6/2K5/8/4N3/8/7b/8 b - - 0 1"; // Magnus Carlsen vs. Ian Nepomniachtchi - MC Invitational (2020)
    // 1... Bb8? (1... Bc7! 2. Nc5 Bb8) 2. Nc5 Bh2 (2... Ke8 3. Na6 Ba7 4. Kc7 Ke7 5. Nb4 (5. Nb8?! Bg1 6. Kc8 Kd6 7. Nc6 Kxc6 8. b8=Q)
    // 5... Ke6 6. Nc6 Bc5 7. Kc8 Bd6 8. Nd4+ Kd5 9. Nb5 Bg3 10. Nc7+ Kc6 11. b8=Q) (2... Ba7 3. Na6 Ke8 4. Kc7 {xpos}) 3. Ne6+ 1-0
    //"1b1k4/1P6/2K5/8/4N3/8/8/8 w - - 0 2"; // After 1... Bb8?
    //"5K1k/5B2/4NN2/8/8/8/5p2/5r2 w - - 0 1"; // A. Grunenwald 1960 #3 [7-ply 0.31 sec]
    //"8/2N5/5R1p/2pP4/1pP3pP/1P2k3/r3n3/7K b - - 0 55"; // Xiong v Nakamura (0-1) 2019-03-31 US Championship R11 -#8 [15-ply to find #-9 in 72.41 sec @1.5 MHz over 100.63 Mnode]
    // 1... Nd4 2. d6 Nf3 3. Nd5+ Kf2 4. Rxf3+ Kxf3 5. Ne3 Ra1+ 6. Nf1 Rxf1+ 7. Kh2 g3+ 8. Kh3 Rh1#
    //"r1bnknr1/1pp1qp2/p3p1p1/3pP1Np/3P3R/2NB1RQ1/PPP2PP1/2K5 w q - 0 1"; // Stockfish 8 v Alpha Zero (1-0) too deep to find Bc4!
    // See https://www.youtube.com/watch?v=Fwzq7qK6MMQ for Stockfish 8 variations
    // For example: moves d3c4 d5c4 d4d5 f8d7 h4c4 c7c6 c3e4 c6d5 e4d6 e7d6 e5d6 d5c4 g3h4 g8g7 g2g4 d7e5 g5f7 d8c6 f7e5 c6e5 h4f6 g7f7 f6h8 e8d7 f3f7 e5f7 h8h7 d7d6 h7f7
    //"8/8/6b1/3k4/8/1N5P/p7/3K4 b - - 0 77"; // Fedoseev v Carlsen 0-1 [18-ply in 1:48 @1.275 MHz over 137.4 Mnode] eval -6.65
    //"8/n7/P7/8/2n5/8/2k5/K7 b - - 0 1"; // 2N v P #11 [16-ply in 36.83 @1.489 MHz over 54.85 Mnode]
    // 1... Kb3 2. Kb1 Nb2 3. Kc1 Kc3 4. Kb1 Nd3 5. Ka2 [5. Ka1 Kc2 transposing to 7... Kc2] Kb4
    // 6. Kb1 [6. Ka1 Kb3 7. Kb1 Nb5 8. Ka1 Na3 9. a7 Ne1 10. a8=Q Nec2#] 6... Kb3 7. Ka1 Kc2 8. Ka2 Nb5
    // 9. Ka1 [9. a7 Nc1+ [or 9... Nb4+ 10. Ka1 Nd4] 10. Ka1 transposing to 10. a7] 9... Nc1 10. a7 Nc3 11. a8=Q Nb3#
    //"5k2/8/5pK1/5PbP/2Bn4/8/8/8 b - - 0 68"; // Carlsen v Caruana 2018-11-09 WCC R6 London
    // 68... Bh4! 69. Bd5 Ne2 70. Bf3 (70. Kh7 Bg5 71. Bf3 Ng3! 72. Kg6 (72. Bg4 Kf7 73. Kh8 Be3 74. Kh7 Bc5 75. h6 (75. Kh8?! Bf8 76. Kh7??)
    // 75... Bf8 76. Bh3 Ne4 77. Bg2 Ng5+ 78. Kh8 Bxh6 79. Bd5+ Kf8 80. Be4 Bg7#) 72... Kg8-+)
    // 70... Ng1 71. Bg4 (71. Bd5 Bg5 72. Kh7 Ne2 73. Bf3 Ng3 74. Bg4 Kf7 75. Kh8 Bc1 76. Kh7 Ba3) 71... Kg8-+
    //"8/8/4R3/5pk1/8/3B4/7p/2nK4 w - - 0 1"; // Blindfold Study given to Wesley So by Sagar Shah [1. Be4 fxe4 2. Re5+ Kg4 3. Rxe4+ Kg3 4. Re1 Nd3 5. Rf1 Kg2 6. Ke2 Nf4+ 7. Ke1 Nh3 8. Rh1 Kxh1 9. Kf1=]
    //[BuildMove Test]moves d3e4 f5e4 e6e5 g5g4 e5e4 g4g3 e4e1 c1d3 e1f1 g3g2 d1e2 d3f4 e2e1 f4h3 f1h1 g2h1 e1f1
    //"k1K5/7p/PB4pP/1P3pP1/5P2/3pP3/p1p5/rbQ5 w - - 0 2"; // Quiescent Mate Test
    //"k1K5/7p/PBN3pP/1P3pP1/4pP2/2p1P3/pp6/r5Q1 w - - 0 1"; // Solve #4 [go mate 4 over 613,162 nodes]
    //"7k/8/5N1P/8/2p5/2N5/8/3K3R w - - 0 1"; // Solve #4 [go mate 4 over 451,773 nodes]
    //"4Q3/6rk/5K2/8/8/8/8/8 w - - 0 1"; // Q v R Philidor #10 [15-ply 65.55 sec @1.34 MHz over 87.9 Mnode]
    //"4Q3/6rk/5K2/8/8/8/8/8 b - - 0 1"; // Q v R Philidor #7 [10-ply 1.59 sec @994 KHz over 1.58 Mnode]
    //"8/1B6/p7/1p1p4/2p2n2/P1P1k3/1KP5/8 b - - 0 64"; // Caruana v Hou Yifan Line Grenke Chess Classic R6 2018-04-06
    // [19-ply in 19:45.1 @1.243 MHz over 1.45 Gnode] eval -3.3 after:
    // 64... Kd2 65. Bxa6 Nd3+ 66. Kb1 Ne1 67. Bxb5 Nxc2 68. Ba4 Ne3 69. a4 Kxc3 70. axb5 axb5 71. Bc6 b4 72. Bb5 b3 73. Bd7 b2 74. Bc8
    // [20-ply in 26:16.5 @1.224 MHz over 1.93 Gnode] eval -3.2 after:
    // 64... a5 65. Ba6 Ne2 66. Bxb5 Kd2 67. Bd7 Nxc3 68. Bc6 Nd1+ 69. Kb1 Ne3 70. Ba4 d4 71. Ka2 Nxc2 72. Bxc2 Kxc2 73. a4 c3 74. Ka3 d3
    // [21-ply in 1:26:16 @1.262 MHz over 6.533 Gnode] eval -3.25
    // 64... a5 65. Ba6 Ne2 66. Bxb5 Kd2 67. Bd7 Nxc3 68. Bf5 Nd1+ 69. Kb1 Ne3 70. Bg6 d4 71. Bh7 d3 72. Bxd3 cxd3 73. cxd3 Kxd3 74. Ka1 Nf1 75. Kb1 Kd3c3
    // Formerly:
    // [21-ply trace in 1:34:58 @1.212 MHz over 6.9 Gnode] eval -3.6
    // 64... Kd2 65. Bxa6 Nd3+ 66. Kb1 Ne1 67. Bxb5 Nxc2 68. Ba4 Ne3 69. Kb2 d4? [69... Nd1+ 70. Bxd1 Kxd1] 70. cxd4 c3+ 71. Kb1 c2+ 72. Bxc2 Nxc2 73. d5 Nxa3+
    // moves e3d2 b7a6 f4d3 b2b1 d3e1 a6b5 e1c2 b5a4 c2e3 b1b2
    //"8/8/8/3p4/B1p5/P1P1n3/1K1k4/8 b - - 0 69"; // Critical Position from Caruana v Hou Yifan Line
    // 69... Nd1+ 70. Bxd1 Kxd1 71. a4 Kd2 72. a5 d4 73. Ka2 dxc3 74. a6 c2 75. a7 c1=Q 76. a8=Q Qc2+
    // 77. Ka1 Qc3+ 78. Kb1 Qb3+ 79. Ka1 Kc1 80. Qh1+ Qd1 81. Qxd1+ Kxd1 82. Kb2 Kd2 83. Ka2 c3 84. Ka1 c2 85. Ka2 c1=Q
    //"8/8/4k2p/7p/5P2/6P1/6K1/8 w - - 0 1";  // Distant Opposition from Doluhanova v Roumegous 2017 [26-ply in 26:38.28 over 2.012 Gnode @1.249 MHz] eval 1.75 after:
    // 1. Kf2 Kf5 [1... Kf6 2. Ke3 Kf5 3. Kf3] 2. Kf3 Ke6 3. Ke4 Kf6 4. f5 Ke7 5. Ke5 Kf7 6. f6 Ke8 [6... Kg8 7. Kf4 Kf8 8. Ke4! Ke8] 7. Ke4 Kf8 8. Kf4! Ke8
    // 9. Ke5 Zugzwang Kd7 10. Kf5 Ke8 11. Kg6 Kf8 [12. Kxh6 Kf7 13. Kg5]
    //"1k6/R7/K7/8/8/8/2p5/8 w - - 0 1"; // Forced Draw 6-ply
    //"3k4/1R6/3K4/8/8/1Br5/8/8 w - - 0 1"; // R v B Philidor from Rc3 #16 [17-ply in 30:41 @1.223 MHz over 2.251 Gnode] eval 7.75
    // [18-ply in 1:18:10.6 @1.197 MHz over 5.614 Gnode] to find #18
    // 1. Be6! Rd3+ 2. Bd5 Rc3 3. Rd7+ Kc8 4. Rg7 Kb8 5. Rb7+ Kc8 6. Rb4 Rd3 7. Rh4 Rxd5+ 8. Kxd5 Kb7 9. Rh3 Kb6 10. Rb3+ Kc7 11. Rb3b5
    // [20-Ply in 3:57:34 @1.2834 MHz over 18.294 Gnode]finds #20 currently.  Due to Futility Pruning in UpdateBest()?
    // [20-ply trace in 5:40:45 @1.119 MHz over 22.882 Gnode] found #16 previously
    // 1. Be6! Rd3+ 2. Bd5 Rc3 3. Rd7+ Kc8 [3... Ke8? 4. Rg7 Rf3 5. Bxf3] 4. Rf7 Kb8 5. Rb7+ Kc8 6. Rb4 Rd3 7. Ra4! Rxd5+ 8. Kxd5 Kb7
    // [8... Kc7 9. Rb4] 9. Kc5 Kc7 10. Ra7+ Kd8 11. Kd6 Ke8 12. Rb7 Kf8 13. Kd6e6
    // moves b3e6 c3d3 e6d5 d3c3 b7d7 d8c8 d7f7 c8b8 f7b7 b8c8 b7b4 c3d3 b4a4 d3d5 d6d5
    //"4Qb1k/6pp/8/5r2/pn1B4/5N2/1Pq3PP/5RK1 w - - 0 37"; // Hou Yifan v Kacper Piorun 2017-09-04 [13-ply in 1:45 @1.539 MHz over 161.4 Mnode] eval 4.5 after:
    // 37. g4! Qc6 38. Qxc6 Nxc6 39. gxf5 Nxd4 40. Nxd4+-
    //"r2qr1k1/pb3pbp/1p4p1/8/3N4/BPN3P1/P2Q3P/R2R1K2 b - - 0 21"; // RE Byrne v Fischer 1963-12-18
    //"8/2k5/K7/1p6/3B4/8/P7/8 w - - 0 62"; // 2017-02-24 Nakamura v Grischuk Wrong Bishop Line [17-ply in 12.23 sec @1.288 MHz over 15.76 Mnode] to find 62. Be5+
    //"Nn6/8/1pPk4/1K1bpB2/1p6/8/8/8 w - - 0 1"; // Fischer Endgame
    //"4k3/2R5/2n1r3/3R4/7P/5BP1/7K/4q3 w - - 0 54"; // 2017-01-21 Carlsen v Giri Line
    //"4r1k1/5pp1/1b4p1/3n4/3p2qP/3Q2P1/1P1B1P2/2N1R1K1 b - - 0 35"; // 2017-01-27 Carlsen v Adhiban Line
    //"5r2/4p1bk/3pQ1pp/Rp5q/1P1p4/3P2P1/3BPP1P/6K1 b - - 0 1"; // Forced Draw [9-ply]
    //"6k1/3b3p/2pP4/p5q1/4QN2/6P1/PP5P/1K1R4 b - - 0 1"; // Puzzle [White to win after: ...Bf5.  Hint: The key move and the "follow up" transpose.]
    //"4k3/8/8/8/8/8/4P3/4K3 b - - 0 1";
    //"8/8/2k5/7p/8/2K5/8/8 w - - 0 1";   // Inside Square
    //"k7/7p/8/2nKb3/8/8/8/8 w - - 0 1";  // Wrong Bishop
    //"1R3N2/5k2/8/6P1/8/4K3/8/5r2 w - - 0 62"; // Kramnik v Vachier-Lagrave 2013-08-27
    // 62. Nd7 Rf5 63. Rf8+ Kg6 64. Rg8+ Kf7 65. Ke4 Ra5 66. Rf8+ Kg7
    // [66... Kg6?! 67. Ne5+! Kxg5 68. Rf5+ Kh6 [68... Kh4 69. Ng6+] 69. Ng4+ Kg6 70. Rxa5]
    // 67. Kf4 Ra4+ 68. Kf5 Rd4 69. Rd8 Rd5+ 70. Kg4 Kf7 71. Rf8+ Kg7 72. Rg8+ Kf7 73. Nf6
    //"8/8/8/8/8/2K4B/5k1P/8 w - - 0 1"; // J. Vancura 1922, Ceske Slovo (care of Frederic Friedel) [11-ply in 0.867 sec to find 1. Bd7!]
    //"8/3P3k/n2K3p/2p3n1/1b4N1/2p1p1P1/8/3B4 w - - 0 1"; // Mike's "Famous Study"
    // See "Solution to a truly remarkable study" by Frederic Friedel, 2018-02-12 https://en.chessbase.com/post/solution-to-a-truly-remarkable-study
    // This study was composed by Gijs van Breukelen and presented to the players at a Super Tournament in Brussels, 1987.  First solved by Mikhail Tal.
    // 1. Nf6+ Kg7 2. Nh5+ Kg6 3. Bc2+!! Kxh5 Here Stockfish 12 shows +3.66 4. d8=Q Kg4
    // [4... Nf7+? 5. Ke6 Nxd8+ 6. Kf5! e2 7. Be4 e1=N 8. Bd5 c2 9. Bc4 c1=N 10. Bb5 Nc6 11. Bxc6 Nc7 12. Ba4 Nc2 13. Bxc2 Ne2 14. Bd1 -- 15. Bxe2#]
    // 5. Bd1+ Kxg3 6. Qe8 c4+ 7. Kc6 Bc5 8. Qe5+ +-
    //
    // Hint position after moves g4f6 h7g7 f6h5 g7g6 d1c2 g6h5 d7d8q g5f7 d6e6 f7d8 e6f5:
    // 3n4/8/n6p/2p2K1k/1b6/2p1p1P1/2B5/8 b - - 0 1 [18-ply]
    //
    //"r1r3k1/2q1bp1p/4b1pQ/p2pP3/P2B1R2/2PB4/6PP/5R1K w - - 0 25"; // Kamsky v D'Costa 2016-02-13 #14 Capelle La Grande [double rook sacrifice]
    //"r4rk1/p4pb1/bp5p/2pNqPp1/6P1/7Q/PPP3P1/2KR2BR w - - 0 1"; // Ninov vs Colovic 2015
    //"4rbk1/1ppr1p2/5R2/pP6/2Q5/P7/1B2q1PP/5R1K w - - 0 30"; // Giri v Sunilduth Lyna, Narayanan 2015-12-20 #6 [9-ply in 7 sec @1.451 MHz over 10.19 Mnode]
    //"6rk/5p2/Q2pP2p/pNp1b2n/P3P1Nq/5R1P/3P2P1/6K1 b - - 0 30"; // Carlsen v Grischuk LCC R9 2015-12-13
    //"1b1Q4/1P6/6k1/1B6/5p2/6PK/5q2/8 w - - 0 66"; // Carlsen v Topalov 2015-06-16 (0-1 R1 Time Forfeit)
    // [15-Ply in 17:17 @1.327 over 1.3762 Gnode] eval 5.95, [16-Ply in 6:56:40 @1.317 MHz over 32.922 Gnode] eval 8.30
    // 66. Be8+ Kf5 67. Bd7+ Ke4 68. Bc6+ Ke3 69. Qb6+ Ke2 70. Qxf2+ Kxf2 71. g4 Be5 72. g5 Bb8 73. g6 Be5 74. g7 Bxg7 75. b8=Q Bh6 76. Bb5
    //
    // 66. Bd3+! Kf7 67. Bc4+ Kg6 68. Qg8+ Kf6 (68... Kh6 69. Qf8+ Kg5 70. Qg7+ Kf5 71. g4+ Ke4 72. Qg6+ Kd4 {xpos})
    // 69. Qf7+ Kg5 70. Qg7+ Kf5 71. g4+! Ke4 72. Qg6+ Kd4 (72... Kf3 73. Qc6+ Ke3 74. Qc5+ Kf3 75. Qd5+ Ke3 76. Qd3#)
    // 73. Qb6+ Kxc4 74. Qxf2 Kb5 75. Qa2 Kc5 76. Qa6
    //
    //"5Q2/5p1p/1pPr2p1/6k1/8/3pP2P/2q2PP1/3R1K2 w - - 0 44"; // Khismatullin v Eljanov 2015-03-06 [12-ply in 1:19.3 @1.317 MHz] eval 0.20
    // [13-ply in 3:52.4 @1.323 MHz] eval 0.87
    //"r1qr2k1/1p3pp1/2pbbn2/p3N3/3P3Q/P5PB/1B3P2/R3R1K1 w - - 0 1"; // Taimanov v Kuzminykh 1950 [13-ply in 1:40 @1.472 over 161.9 Mnode] eval 2.35,
    // [14-ply in 5:59.1 @1.3865 MHz over 497.9 Mnode] eval 2.35, [15-ply in 15:59 @1.387 MHz over 1.33 Gnode] eval 2.55
    //"5rk1/5ppp/p1Q1p3/1r6/q2b4/4B1P1/P2RPP1P/1R4K1 w - - 0 26"; // Kasparov v Ribli 1989 [20-ply in 5:05:01 @1.288 MHz over 23.571 Gnode] eval 0.03
    // 26. Rxb5 Bxe3 27. Rd8 Bxf2+ 28. Kxf2 Qxb5 29. Qd6 Qf5+ 30. Kg1 Qb1+ 31. Kg2 Qe4+ 32. Kh3 {with 14-ply needed for shelter}
    // 32... Qf5+ 33. g4 Qf1+ 34. Kg3 Qe1+ (34... Qg1+ 35. Kf3 Qf1+ {xpos}) 35. Kf3 Qf1+ 36. Ke3 Qh3+ 37. Kd4 Qxg4+ 38. Kc3 Qh3+ 39. Kb2 h6
    // 40. Qxf8+ Kh7 41. Qh8+ Kg6 42. Rg8
    //"8/8/8/8/4k3/8/8/2BQKB2 w - - 0 1"; // Pal Benko for Bobby Fischer #3 [6-ply in 0.5 sec over 260,802 nodes]
    //"r1b1r1k1/ppq2p1p/3b2pQ/3pn3/8/2P4P/PPBN1PP1/R1B1R1K1 b - -"; // Hit Rate 1 15.8% hits [plus 1.1% Qxnt] at 12-ply, expecting ~25%
    //"6k1/5p1p/P1pb1nq1/6p1/3P4/1BP2PP1/1P1Nb2P/R1B3K1 b - -"; // Hit Rate 2 26.6% at 12-ply [plus 4% Qxnt], exceeding ~30% Hits
    //"8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - -"; //[Fine 70]87% Hits at 37-ply, should solve by 26-ply
    //"8/1pr5/PR6/1P4kp/2K3p1/6P1/8/8 w - - 0 53"; // Karjakin v Morozevich 2012-11-29
    //"bnrbkrqn/pppppppp/8/8/8/8/PPPPPPPP/BNRBKRQN w FCfc - 0 1";
    //"r5k1/1pq1Nppn/2pr4/p7/3P3Q/P5PB/1B3P2/R5K1 b - - 0 4";
    //"r2r2k1/1pq2ppn/2pbR1N1/p7/3P3Q/P5PB/1B3P2/R5K1 w - - 0 3";
    //"r1qr2k1/1p3pp1/2pbbn2/p3N3/3P3Q/P5PB/1B3P2/R3R1K1 w - - 0 1";
    //"1K1n2q1/1p4pR/1prPPpPn/1RB1p1rP/8/1B2p3/2p2b2/1k2N3 w - - 0 1";
    //"8/8/4N3/p4K2/8/3k1n2/8/8 w - - 0 1";
    //"3k4/6p1/7p/1Pp1Np2/p3nPP1/3K4/7P/8 b - - 0 56"; // Hao v Carlsen 2012-07-30
    //
    // 56... fxg4 57. Kxe4 a3 58. b6 Kc8 59. b7+ Kxb7 60. Nd3 a2 61. Nxc5+ Kc6 62. Nb3 h5-+
    // (56... Nf2+ 57. Kc4 Nxg4 58. b6 Kc8 59. Nc6 a3 60. Kb3 Kb7 61. Nd8+ Kxb6 62. Ne6
    // Nxh2 63. Nxg7 Nf3 64. Kxa3 Nd4 65. Kb2 c4 66. Nh5 Kc5 67. Kc3 Nb5+ 68. Kd2 c3+
    // 69. Kd3 Kb4 70. Ng3 Kb3 71. Nf1 Na3 72. Ne3 c2 73. Nxc2 Nxc2-+; 56... Nd6 57. gxf5 Nxb5
    // 58. Kc4 a3 59. Kb3 Ke7 60. Nc4 h5 61. Ne3 Kf6 62. h4 Nd4+ 63. Kxa3 Nf3-+)
    //
    //"2kr4/3q1R2/p1pp4/4p3/2P1n2r/4Q3/PBP4P/5R1K b - - 0 23";  // Harmon-Vellotti v Troff 2014-06-26 #5 [6-ply in 0.2 sec]
    //"8/8/5pk1/6p1/1pp5/3q1P2/1P1N2P1/2K1R3 b - - 0 38"; // Gelfand v Inarkiev 2016-07-12 #6 11-ply
    // underpromotion line: moves b4b3 c1d1 c4c3 e1e2 c3d2 e2e1 d3c2 d1e2 d2d1n e2f1 c2f2
    //"8/5k1N/4q1p1/3pB1Q1/r7/5P2/5KP1/8 w - - 0 44"; // Carlsen v Giri (1-0) 2016-07-22 [11-ply in 9.04 sec @1.188 MHz over 10.737 Mnode]
    //"2nq1nk1/5p1p/4p1pQ/pb1pP1NP/1p1P2P1/1P4N1/P4PB1/6K1 w - - 0 28"; // Fischer v Panno 1970 [16-ply in 4:24.35 @1.5053 MHz over 398 Mnode] eval 1.33 after:
    // 28. Be4! Be8 29. hxg6 hxg6 30. Nh5 gxh5? 31. Bh7+ Nxh7 32. Nxh7 f5 33. Nf6+ Kf7 34. Nxh5 Ke7 35. Qg7+ Bf7 36. Qf6+ Ke8 37. Ng7+
    //"4b1q1/7k/3PpQp1/p2p4/1p1P2P1/1P1B4/P4P2/6K1 b - - 0 37"; // Fischer v Panno 1970 Line
    //"2r1q1k1/r4p1p/b3pBp1/n3P1QP/p2p3R/P5P1/2p2PB1/R5K1 w - - 0 2"; // Fischer Mate, 8-ply for #7
    //"8/8/3Q3Q/8/8/5q2/Q6Q/2Qbk1K1 w - - 0 1"; // Abbreviation Tests
    //"8/6Q1/1Q6/5b2/B7/6r1/7Q/K1k5 w - - 0 1";
    //"8/7p/8/8/4N1K1/2q5/6k1/1N6 w - - 0 1";
    //"r1bqr1k1/pp3ppp/2nB4/1N6/2Bn4/8/PP4PP/2RQR1K1 w - - 0 24"; // Kramnik v Meier 2012-07-22
    //"r1bqr3/pp5Q/2nB1kp1/1N6/3n4/8/PP4PP/2R1R1K1 w - - 0 4"; // Kramnik v Meier 2012-07-22 White Choice
    //"r1bqr3/ppN4Q/2nB1kp1/8/3n4/8/PP4PP/2R1R1K1 b - - 0 4"; // Kramnik v Meier 2012-07-22 Black Choice
    //"5k2/3n1p2/4p1rp/2q1PN2/1Q6/8/1P3PP1/4R1K1 w - - 0 1"; // Pin and Tempo
    //"8/2Q2pk1/7p/1r2Pp2/8/8/5PPK/8 w - - 0 8"; // Pin and Tempo1
    //"8/5pk1/7p/1PQ1Pp2/8/8/5PPK/r7 b - - 0 6"; // Pin and Tempo2
    //"1Q6/5pk1/8/4Pp1p/8/8/5PPK/8 w - - 0 12"; // Pin and Tempo3, #7 in 11-ply
    //"r2r2k1/p4ppp/2q2n2/8/N2n4/P4PP1/QN2P1KP/R3R3 b - - 0 24";  // Wood v Sowell 2014-07-16
    //"q1r3k1/5p1p/6pB/1p6/2bN4/2P1Q2P/5P2/r2BR1K1 w - - 0 35"; // Caruana v Gustafsson 2012-07-17
    //"q1r3k1/4Qp1p/6pB/1p6/3N4/2P4P/5P2/r2B1RK1 b - - 0 36"; // Caruana v Gustafsson 2012-07-17 Line
    //"5rk1/5p1p/5Qp1/1p6/3N4/2P4P/5P2/r2B1RK1 w - - 0 39"; // Caruana v Gustafsson 2012-07-17 #8 [13-ply in 17.6 sec @1.619 MHz over 28.5 Mnode]
    //"8/p3q1kp/1p2Pnp1/3pQ3/2pP4/1nP3N1/1B4PP/6K1 w - - 5 30"; // Botvinnik v Capablanca 1938 AVRO R11 [15-ply in 9:16.3 @1.413 MHz over 786.3 Mnode] eval 2.45 after:
    // 30. Ba3!! Qe8 31. Qc7+ Kh8 32. Be7 Kg7 33. Qxa7 Nc1 34. Bd8+ Kf8 35. Bxf6 Qxe6 36. Be5 h5 37. Qb8+ Ke7 38. h4 Na2
    // Capablanca played 30... Qxa3? 31. Nh5+ gxh5 32. Qg5+ Kf8 33. Qxf6+ Kg8 34. e7 [34. Qf7+ Kh8 35. g3! Nxd4!? 36. e7 Qc1+ 37. Kg2 Qc2+ 38. Kh3 Qf5+ 39. Qxf5 Nxf5 40. e8=Q+]
    // 34... Qc1+ 35. Kf2 Qc2+ 36. Kg3 Qd3+ 37. Kh4 Qe4+ 38. Kxh5 Qe2+ 39. Kh4 Qe4+ 40. g4 Qe1+ 41. Kh5
    //"8/5B2/8/8/5KNk/8/8/8 b - - 0 13";  // KBN v K #9 [15-ply in 14.6 sec @1.5801 MHz over 23.08 Mnode]
    //"8/5b2/8/8/5knK/8/8/8 w - - 0 13";  // KBN v K #9 [15-ply in 15.76 sec sec @1.563 MHz over 24.63 Mnode]
    //"8/8/3n1b2/8/7K/1k6/8/8 w - - 0 67";  // Paehtz v Hou Yifan (0-1) 2016-10-08 Isle of Man [KBN vs K Endgame] Deeper
    //"8/8/3n1b2/8/5K2/3k4/8/8 w - - 4 69";  // Paehtz v Hou Yifan (0-1) 2016-10-08 Isle of Man [KBN vs K Endgame] Easier
    //"8/8/3n4/8/3b4/8/4k3/7K b - - 9 71";  // Paehtz v Hou Yifan KBN #21 [23-ply in 4:18:07 @1.095 MHz over 16.957 Gnode] to find #29
    //"8/8/8/8/8/5k2/3n3b/4K3 b - - 0 79";  // Paehtz v Hou Yifan KBN #12 [18-ply in 33.4 sec @1.6 MHz over 53.4 Mnode]
    //"8/8/7K/4k2P/6b1/6n1/8/8 w - - 0 1";  // KBN v K Endgame Test #26
    // 1. Kg6 Nxh5 2. Kh7 Nf4 3. Kg7 Be6 4. Kh7 Kf5 5. Kh6 Bg8 6. Kg7 Bc4 7. Kh6 Ng6 8.Kh5 Bg8 9. Kh6 Kf6 10. Kh5 Ne5
    // 11. Kh4 Kf5 12. Kg3 Ng4 13. Kf3 Bc4 14. Kg3 Bd5 15. Kh3 Kf4 16. Kh4 Be6
    // 17. Kh5 Bf7+ 18. Kh4 Ne3 19. Kh3 Be6+ 20. Kh4 Nf5+ 21. Kh3 Kf3 22. Kh2 Ne3 23. Kg1 Kg3 24. Kh1 Kf2 25. Kh2 Nf1+ 26. Kh1 Bd5#
    //"8/8/8/4nk1b/8/6K1/8/8 w - - 0 14";// KBN v K Endgame Test Line #12 [19-ply in 53.4 sec @1.598 MHz]
    //"6k1/p4R1p/1p5q/5Q2/2Pb4/8/P6P/7K w - - 0 1"; // Reinfeld Combo #357
    //"8/K6N/8/2N5/1n6/6Q1/6pn/7k w - - 0 1"; // Chekhov's Gun [White to Win in 4, Zwischenzug followed by Zugzwang] 8-ply in 0.68 sec
    //"1Bb3BN/R2Pk2r/1Q5B/4q2R/2bN4/4Q1BK/1p6/1bq1R1rb w - - 0 1"; // #1
    //"8/5p2/1P4p1/7p/r2kp2P/2RbN1P1/5P1K/8 w - - 0 61";  // Radjabov vs Karjakin 2012
    //"5r1k/2P4p/1q2Np2/3r4/6p1/2Q1Rb2/1p5P/4R1K1 w - - 0 42";  // Nakamura vs Adams 2011 Line [12-ply in 80.9 sec @1.476 MHz over 119.4 Mnode]
    //"3r2k1/8/5RPK/6NP/2b5/8/8/8 w - - 0 66"; // Caruana v Aronian 2014-02-03 Zurich R5 #8 [15-ply in 6:46.1 @1.266 MHz over 514.2 Mnode]
    // to find 66. Nh7 Re8 67. Rc6 Be6 68. Rc7 Rf8 69. Rg7+ Kh8 70. Nxf8 Ba2 71. Nd7 Bb1 72. Nf6 Bxg6 73. Rg8#
    //"1k5r/1r1B2pp/1PQ5/4pp2/R7/3q3P/5PP1/6K1 w - - 0 1";    // Mavo's Nice Tactics! [12-ply in 18 sec @1.472 MHz] eval 7.85, [16-ply in 6:46.6 @1.469 over 597 Mnode] eval 9.1
    // 1. Bxf5 Qd1+ 2. Kh2 Qxa4 3. Qxa4 Rxb6 4. Be4 Kc8 5. Qa7 Rf6 6. Qa8+ Kd7 7. Qxh8 Rxf2 8. Qxg7+ Ke6 9. Qg8+ Ke7 10. Qd5 Kf6 11. Qd6+ Kg5 12. Qxe5+
    //"1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PPK/8 w - - 0 4";       // Mavo Nice Mate1 #6 [10-ply in 2.675 sec @1.3125 MHz over 3.5 Mnode]
    // 4. Ra6 Rd8 5. Qa4 Ra7 6. Rxa7 Rd5 7. Ra8+ Kb7 8. Bxd5+ Kxb6 9. Qc6#
    //"1k1q3r/1r4pp/1PQ5/4pB2/R7/7P/5PP1/6K1 w - - 0 2";      // Mavo Nice Mate2 [longer]
    //"1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PP1/6K1 w - - 0 3";     // Mavo Nice Mate3 #8 [12-ply in 14.17 sec @1.577 MHz]
    //"5rk1/1rP3pp/p4n2/3Pp3/1P2Pq2/2Q4P/P5P1/R3R1K1 b - - 0 1"; // Unzicker vs Averbach [10-ply in 16.8 sec @1.474 MHz over 24.77 Mnode]
    //"r1bk3r/2p2ppp/1pK5/p2pp3/8/P7/1PPP2PP/R1BQ2NR b - - 0 1"; // Hamppe vs Meitner, 7-ply
    //"8/4Qpk1/5np1/2p4p/2Pp4/P3p1P1/4q2P/5RK1 b - - 0 39";   // Aronian vs Caruana Line [17-ply in 2:45.54 @1.141 MHz over 94.77 Mnode] eval 3.63
    //"8/1R3pk1/6p1/P1p4p/8/3pp1P1/7P/5K2 b - - 0 46";        // Aronian vs Caruana Rook Behind
    //"r6k/p3p2p/2b3p1/2p3B1/3b3Q/6PP/Pr6/R4K2 w - - 0 31";   // Johannessen vs Fischer Line
    //"4r2k/p3B2p/6p1/2p5/P6Q/1b4PP/2r5/b2K4 w - - 0 37";     // Johannessen vs Fischer #8 [13-ply in 38.217 sec @1.522 MHz over 58.18 Mnode]
    //"1Q3b2/5pk1/2p3p1/1p1bN2p/4n2P/8/r5P1/6K1 b - - 0 35";  // Byrne vs Fischer 35 #9 [14-ply in 3:00.3 over 268.66 Mnode @1.49 MHz]
    // 35... Bc5+ 36. Kh2 Nd2 37. Kh1 Ra1+ 38. Kh2 Nf1+ 39. Kh1 Ng3+ 40. Kh2 Bf2 41. Qf8+ Kxf8 42. Nxg6+ fxg6 43. Kh3 Rh1#
    //"r3r1k1/pp3pbp/1qp3p1/2B5/2BP2b1/Q1n2N2/P4PPP/3R1K1R b - - 0 17"; // Byrne vs Fischer 17 [14-ply in 8:23 over 713 Mnode @1.418 MHz] eval -1.47
    //"r4rk1/pp2Bpbp/1qp3p1/8/2BPn1b1/Q1P2N2/P4PPP/3RK2R b K - 0 15"; // Byrne vs Fischer 15 [14-ply in 15:52.5 over 1.353 Gnode @1.42 MHz] eval -1.07
    //"r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13"; // Byrne vs Fischer 13
    //"r2q1rk1/pp2ppbp/1np2np1/2Q3B1/3PP1b1/2N2N2/PP3PPP/3RKB1R b K - 0 11"; // Byrne vs Fischer 11 >11-ply
    //"8/K1P5/8/3q4/4k3/8/8/8 b - - 0 1";
    //"8/P1K5/8/3q4/4k3/8/8/8 w - - 0 1";
    //"2q2r1k/3brp2/7Q/8/2BPp3/6b1/1B4P1/6K1 b - d3 0 1"; // EPIllegal
    //"k1n1K3/p7/8/B2pP3/8/8/6B1/8 w - d6 0 1"; // ep Mate
    //"7k/b1n3q1/8/2PpP3/3K4/8/8/8 w - d6 0 1"; // EPIllegal, so Inconsistent FEN, 9-ply for #6 in 0:36
    //"2q1rrk1/5p2/6p1/8/2B5/8/2Q3P1/6K1 w - - 0 1"; // Draw3QxG6
    //"2r2rk1/8/2b4Q/3pP3/2q5/6B1/5P2/5R1K w - d6 0 1"; // Draw3EPIllegal
    //"2r2rk1/8/2b4Q/3pP3/2q5/6B1/5P2/5RK1 w - d6 0 1"; // Draw3EPLegal
    //"2r2rk1/8/7Q/3pP3/2q1n3/6B1/5P2/5RK1 w - d6 0 1"; // Draw3NEP
    //"2r2rk1/8/7Q/3pP3/2q1n3/6B1/5P2/5RK1 w - - 0 1"; // Draw3N
    //"7r/8/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1"; // Perpetua0
    //"8/8/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1"; // Perpetua Draw3 [24-ply in 21:13 @1.3532 over 1.273 Gnode] eval 0.0
    // moves h6h5 f6f5 h5h4 f5h3 h4g5 h3f5 g5h4 f5h3 h4g5 h3f5 g5h6 f5f6 h6h7 f6f7 h7h8 f7f8 h8h7 f8f7 h7h8 f7f8 h8h7 f8f7, or
    // moves h6h5 f6f7 h5g4 f7f5 g4h4 f5f6 h4h5 f6f5 h5h6 f5f6 h6h7 f6f7 h7h6 f7f6 [once inevitable, postponing a draw is not favored]
    //"rnb3nr/pppp1k1p/3b2q1/7Q/5B2/8/PPP3PP/RN3R1K w - - 0 14"; // Jensen vs Urkedal 2013 #10 [16-ply in 14:39 @1.9 MHz over 1.67 Gnode]
    // 14. Bxd6+ Nf6 15. Rxf6+ Kg7 16. Rxg6+ hxg6 17. Qe5+ Kg8 18. Qe8+ Kg7 19. Qe7+ Kh6 20. Bf4+
    //"rnb4r/pppp1k1p/3B1nq1/7Q/8/8/PPP3PP/RN3R1K w - - 0 15"; // Jensen vs Urkedal 2013 #9 [13-ply in 15.24 sec @1.797 MHz over 27.4 Mnode]
    //"rnb4r/pppp2k1/3B2p1/7Q/8/8/PPP3PP/RN5K w - - 0 17";  // Jensen vs Urkedal 2013 #7 [9-ply in 1.3 sec @915 KHz]
    "rnb4r/pppp2k1/3B2p1/4Q3/8/8/PPP3PP/RN5K b - - 0 17"; // Jensen vs Urkedal 2013 #6 [12-ply?! in 1.8 sec @1.287 MHz over 2.3 Mnode]
    //"r4rk1/pp2Bpbp/1qp3p1/8/2BPn1b1/Q1P2N2/P4PPP/3RK2R b K - 0 15";
    //"3r2k1/1p3pp1/p1p5/8/1P2r3/P3PqBp/2Q2P1P/R3RK2 b - - 0 29"; // 1964 Bielicki v Smyslov (0-1) [13-ply in 2:00 @1.548 MHz over 185.7 Mnode] eval -9.35
    //"3r2k1/1p3pp1/p1p5/8/1Pr5/P3PqBp/1Q3P1P/R3RK2 b - - 0 30"; // 1964 Bielicki v Smyslov (0-1) #9 [15-ply 1:18 @1.633 MHz]
    //"6k1/1p3pp1/p1p5/8/1P4r1/P3PqBp/1Q3P1P/3rRK2 w - - 0 32";
    //"3r2k1/1p3pp1/p1p5/8/1P4r1/P3PqBp/1Q3P1P/R3RK2 w - - 0 31"; // 1964 Bielicki v Smyslov (0-1) Final
    //"6k1/1p3pp1/p1p5/8/1P6/P3PqBp/2R2P1P/3rRK2 w - - 0 32"; // 1964 Bielicki v Smyslov (0-1) Line, 15-ply
    //"6k1/1p3pp1/p1p5/4B3/1P3K2/P3P2p/2R2PqP/3r4 b - - 0 37";
    //"6k1/1p3pp1/p1p5/4B3/1P6/P3Pq1p/2R2P1P/3rRK2 b - - 0 32"; // Line, after Be5
    //"r3kbnr/pp2pppp/8/1N6/3n4/5P2/PP3P1P/R1B1KB1R b KQkq - 0 10"; // c3 Sicilan w 4... d5
    //"N5nr/pp1kppbp/6p1/1B6/8/4BP2/PP3P1P/n2K3R b - - 0 15";
    #endregion

    #region EPD Constants
    //
    // Bratko-Kopec Test Suite: 24 positions
    //
    private const String sDefaultEPD =
      @"4b1q1/7k/3PpQp1/p2p4/1p1P2P1/1P1B4/P4P2/6K1 b - - hmvc 0; fmvn 37; id ""Fischer v Panno 1970 Line"";";
    //@"1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 b - - bm Qd1+; id ""BK.01"";";
    //@"3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - - bm d5; id ""BK.02"";";
    //@"2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - - bm f5; id ""BK.03"";";
    //@"rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq - bm e6; id ""BK.04"";";
    //@"r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - - bm Nd5 a4; id ""BK.05"";";
    //@"2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - - bm g6; id ""BK.06"";";
    //@"1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - - bm Nf6; id ""BK.07"";";
    //@"4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - - bm f5; id ""BK.08"";";
    //@"2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - - bm f5; id ""BK.09"";";
    //@"3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - - bm Ne5; id ""BK.10"";";
    //@"2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - - bm f4; id ""BK.11"";";
    //@"r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - - bm Bf5; id ""BK.12"";";
    //@"r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - - bm b4; id ""BK.13"";";
    //@"rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - - bm Qd2 Qe1; id ""BK.14"";";
    //@"2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - - bm Qxg7+; id ""BK.15"";";
    //@"r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq - bm Ne4; id ""BK.16"";";
    //@"r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - - bm h5; id ""BK.17"";";
    //@"r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - - bm Nb3; id ""BK.18"";";
    //@"3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - - bm Rxe4; id ""BK.19"";";
    //@"r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - - bm g4; id ""BK.20"";";
    //@"3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - - bm Nh6; id ""BK.21"";";
    //@"2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - - bm Bxe4; id ""BK.22"";";
    //@"r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq - bm f6; id ""BK.23"";";
    //@"r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - - bm f4; id ""BK.24"";";

    //
    // Arasan Test Suite V18: 250 positions
    //
    //@"r1bq1r1k/p1pnbpp1/1p2p3/6p1/3PB3/5N2/PPPQ1PPP/2KR3R w - - bm g4""; id ""arasan18.1""; c0 ""J. Polgar-Berkes, Budapest Hunguest Hotels 2003"";";
    //@"r1b2rk1/1p1nbppp/pq1p4/3B4/P2NP3/2N1p3/1PP3PP/R2Q1R1K w - - bm Rxf7; id ""arasan18.2""; c0 ""Van der Wiel-Ribli, IBM Amsterdam 1980"";";
    //@"r1br2k1/pp2qpp1/1b2p2p/3nB3/6P1/3B4/PPPNQP1P/1K1R3R w - - bm g5; id ""arasan18.3""; c0 ""Victorious (Stockfish 191013SL)-AKIM(Houdini 3 Pro), playchess.com 2013"";";
    //@"r3r1kb/1npb1p1p/2q2np1/4p1B1/ppP1P2Q/1P2RNNP/P1B2PP1/3R2K1 w - - bm Nf5; id ""arasan18.4""; c0 ""Salgado Lopez-Granda Zuniga, IV ch-IberoAmerican Final GpA 2012"";";
    //@"2rr3k/2qnbppp/p1n1p3/1p1pP3/3P1N2/1Q1BBP2/PP3P1P/1KR3R1 w - - bm Bxh7; id ""arasan18.5""; c0 ""Eljanov-Van Wely, Tch-RUS Sochi 2008"";";
    //@"2rqk3/p2n1pp1/Q3p1b1/3p3r/3P3b/2N1P3/PP3P2/R1B1KBR1 b Q - bm Bxf2+; id ""arasan18.6""; c0 ""Vaganian-Palatnik, USSR 1980"";";
    //@"r1b1k2r/1p1pppb1/p5pp/3P4/q2p1B2/3P1Q2/PPP2PPP/R3R1K1 w kq - bm Rxe7+; id ""arasan18.7""; c0 ""Svidler-Carlsen, Longyearbyen 2006"";";
    //@"R4bk1/2Bbp2p/2p2pp1/1rPp4/3P4/4P2P/4BPPK/1q1Q4 w - - bm Qa4; id ""arasan18.8""; c0 ""Gurevich-Bareev, Cap D'Agde KO 2002"";";
    //@"r2q1rk1/5ppp/3b4/1nnP2P1/1Q3BbP/Pp4N1/1P4B1/1KN2R1R b - - bm Nxa3+; id ""arasan18.9""; c0 ""Dothan-Iotov, AEAC 5 Years - Alhambra, ICCF 2007"";";
    //@"2r3k1/3q3p/5Qp1/p3Bp2/1b1p1P2/6P1/PP3PBP/3b2K1 b - - bm Bb3; id ""arasan18.10""; c0 ""Karkuth-Wunderlich, Alfonsino Lannaioli mem. ICCF 2007"";";
    //@"r7/5pk1/5p2/2p5/pp6/1P1B1KPN/P6P/8 b - - bm c4; id ""arasan18.11""; c0 ""Beliavsky-Dolmatov, USSR ch Minsk 1979"";";
    //@"1r4k1/5r1p/bnpq1p2/p2p1Pp1/Pp1Pp1P1/1P2P1P1/2P1N3/1K1RQB1R b - - bm Nxa4; id ""arasan18.12""; c0 ""Compi-Nargothrond, freechess.org 2010"";";
    //@"r2q3r/1p1bbQ2/4p1Bk/3pP3/1n1P1P1p/pP6/Pn4PP/R1B1R1K1 w - - bm g4; id ""arasan18.13""; c0 ""Fier-Ryan, Sabadell op 2009"";";
    //@"8/5pk1/3p1bp1/1B5p/2P1PP2/3Q2PK/5q2/8 w - - am Qxd6; id ""arasan18.14""; c0 ""Arasan-Rookie, ICC 2012"";";
    //@"r1bqr1k1/3nbppp/p2pp3/1p4PQ/3BP2P/2NB4/PPP2P2/2KR3R w - - bm Bxg7; id ""arasan18.15""; c0 ""Madl-Summermatter, Geneva 1988"";";
    //@"1r2brk1/4n1p1/4p2p/p2pP1qP/2pP1NP1/P1Q1BK2/2P4R/6R1 b - - bm Bg6; id ""arasan18.16""; c0 ""David-Enders, Bundesliga 9900 1999"";";
    //@"1rb2k1r/2q2pp1/p2b3p/2n3B1/2QN4/3B4/PpP3PP/1K2R2R w - - bm Bd8; id ""arasan18.17""; c0 ""Volokitin-Mamedyarov, EU Club Cup Eilat ISR 2012"";";
    //@"5rk1/1pp3p1/3ppr1p/pP2p2n/4P2q/P2PQ2P/2P1NPP1/R4RK1 b - - bm Rf3; id ""arasan18.18""; c0 ""Arasan-Crafty, test game 2012"";";
    //@"r3k2r/2q1b1pp/pp2p3/2pR4/P7/4BP2/1PP1Q1PP/5RK1 w kq - bm Bf4; id ""arasan18.19""; c0 ""Rozentalis-Dokhoian, USSR 1986"";";
    //@"r3r1k1/pp3pp1/3p4/2q4p/2P5/1PB2Q1P/n4PP1/3R1RK1 w - - bm Bxg7; id ""arasan18.20""; c0 ""Bobotsov-Penrose, Palma de Mallorca 1969"";";
    //@"8/8/1p3kp1/p1pP1r1p/P1P5/1P2R1P1/4K3/8 b - - bm Kf7; id ""arasan18.21""; c0 ""Rookie-Arasan, CCT-blitz 2014"";";
    //@"2kr2r1/ppq1bp1p/4pn2/2p1n1pb/4P1P1/2P2N1P/PPBNQP2/R1B1R1K1 b - - bm Nfxg4; id ""arasan18.22""; c0 ""Stockfish 4-Houdini 3, 2013 (Rybka forum)"";";
    //@"3r4/Nk3p2/4pP2/3q2Pr/Ppp5/3nQPP1/1P4K1/R2R4 b - - bm Qf5; id ""arasan18.23""; c0 ""Hase-Macchia, W-ch22 sf10 corr 1997"";";
    //@"b7/P4kp1/R4p2/4p3/1Bp1P2p/2P2P2/1q3NP1/4K3 b - - bm h3; id ""arasan18.24""; c0 ""Deep Junior 8-Deep Fritz 8, SSDF 2007"";";
    //@"3r1rk1/q4pp1/n1bNp2p/p7/pn2P1N1/6P1/1P1Q1PBP/2RR2K1 w - - bm Nxh6+ e5; id ""arasan18.25""; c0 ""Anand-Topalov, Sofia (m/4) 2010"";";
    //@"r1q2rk1/ppnbbpp1/n4P1p/4P3/3p4/2N1B1PP/PP4BK/R2Q1R2 w - - bm Bxh6; id ""arasan18.26""; c0 ""Shirazi-Guichard, Malakoff op 2009"";";
    //@"1R6/5p1k/4bPpp/3pN3/2pP1P1P/2r5/6PK/8 w - - bm h5; id ""arasan18.27""; c0 ""Carlsen-Aronian, Elista candidates (m/5) 2007"";";
    //@"3q1rk1/pr1b1p1p/1bp2p2/2ppP3/8/2P1BN2/PPQ3PP/R4RK1 w - - bm Bh6; id ""arasan18.28""; c0 ""Shredder-Rybka, WCCC 2006"";";
    //@"8/5pk1/p4npp/1pPN4/1P2p3/1P4PP/5P2/5K2 w - - bm Nxf6; id ""arasan18.29""; c0 ""Wunderlich-Patrici, WCCC27CT01 ICCF 2007"";";
    //@"1r1b4/n2b3k/1p1Np1p1/p2pP1Pp/P2P1P2/1PBN4/6K1/2R5 w - - bm f5; id ""arasan18.30""; c0 ""Houdini 4 - Hannibal 1.4b, TCEC Season 6 - Stage 1a 2014"";";
    //@"5rk1/pp3p1p/7b/2pR4/8/2P4P/P1PNr1P1/2K4R b - - bm Rxd2; id ""arasan18.31""; c0 ""Kaplan-Bronstein, Hastings 1975/76"";";
    //@"r5k1/1pp3b1/3p1qp1/p2Ppr2/2P4P/2Nn1N1R/PP6/R1BQ1K2 b - - bm e4; id ""arasan18.32""; c0 ""Hergott-Goldenberg, Financial Concept op, North Bay CAN 1999"";";
    //@"2rq1rk1/3bbp1p/p3pp2/n2p1P2/7B/5N2/PPPQ2PP/1K1R1B1R w - - bm Qh6; id ""arasan18.33""; c0 ""Planinc-Spassov, Rubinstein mem 1979"";";
    //@"8/6p1/P1b1pp2/2p1p3/1k4P1/3PP3/1PK5/5B2 w - - bm Bg2; id ""arasan18.34""; c0 ""Vincent Lejeune #36"";";
    //@"r3rk2/pb2bpp1/1n5p/q1pP1B2/1p3B2/5N2/PPQ2PPP/R2R2K1 w - - bm Bc8; id ""arasan18.35""; c0 ""Szabo-Kottnauer, Schiavno Zdroj 1950"";";
    //@"r5n1/p1p1q2k/4b2p/3pB3/3PP1pP/8/PPPQ2P1/5RK1 w - - bm Qf4; id ""arasan18.36""; c0 ""Shulman-Marciano, Ubeda 1997"";";
    //@"2b2rk1/r3q1pp/1nn1p3/3pP1NP/p1pP2Q1/2P1N3/1P1KBP2/R5R1 w - - bm Nxh7; id ""arasan18.37""; c0 ""Vincent-Sebagh, 66th corr ch-FRA 2001"";";
    //@"rnb3k1/p3qpr1/2p1p3/2NP3p/1pP3p1/3BQ3/P4PP1/4RRK1 w - - bm Qd4; id ""arasan18.38""; c0 ""Papenin-Karacsony, CS University Craiova - 10 Years ICCF 2010"";";
    //@"r3r1k1/p3bppp/q1b2n2/5Q2/1p1B4/1BNR4/PPP3PP/2K2R2 w - - bm Rg3; id ""arasan18.39""; c0 ""Tukmakov-Minenkov, Soviet Union 1969"";";
    //@"rn1rb1k1/pq2bppp/4p3/2p1N3/4PQ2/1PB3P1/P4PBP/2R2RK1 w - - bm Ng4; id ""arasan18.40""; c0 ""Alekhine/Monosson - Stoltz/Reilly, Nice consult 1931"";";
    //@"3q1r1k/2r2pp1/p6p/1pbppP1N/3pP1PP/3P1Q2/PPP4R/5RK1 w - - bm g5; id ""arasan18.41""; c0 ""Pinkovetsky-Fischer, WC/27 final ICCF 2011"";";
    //@"1q6/6k1/5Np1/1r4Pp/2p4P/2Nrb3/PP6/KR5Q b - - bm Bd4; id ""arasan18.42""; c0 ""De Carbonnel-Berliner, 5th World CC Ch Final 1967"";";
    //@"b2rk3/r4p2/p3p3/P3Q1Np/2Pp3P/8/6P1/6K1 w - - bm Qh8+; id ""arasan18.43""; c0 ""Grischuk-Svidler, WCh Mexico City (6) 2007"";";
    //@"2kr1b1r/1pp1ppp1/p7/q2P3n/2BB1pb1/2NQ4/P1P1N3/1R3RK1 w - - bm Rxb7; id ""arasan18.44""; c0 ""Critter 1.6a 4CPU-Rybka 4.1 4CPU, Scandinavian Thematic 2013 (G. Banks)"";";
    //@"r1br2k1/ppp2q1p/nb3p2/6p1/2PN1B2/2P2B2/P1Q2PPP/3RR1K1 w - g6 bm Nc6; id ""arasan18.45""; c0 ""Shredder-Nimzo, IPCCC Paderborn 2000"";";
    //@"br4k1/1qrnbppp/pp1ppn2/8/NPPBP3/PN3P2/5QPP/2RR1B1K w - - bm Nxb6; id ""arasan18.46""; c0 ""Anand-Illescas Cordoba, Linares 1992"";";
    //@"r2q1rk1/ppp2p2/3p1np1/4pNQ1/4P1pP/1PPP4/1P3P2/R3K1R1 w Q - bm Qh6; id ""arasan18.47""; c0 ""Morella-Fernandez, Cuba 1998"";";
    //@"1qb2rk1/3p1pp1/1p6/1pbBp3/r5p1/3QB3/PPP2P1P/2KR2R1 w - - bm b3; id ""arasan18.48""; c0 ""Shirov-Zvjaginsev, Biel 1995"";";
    //@"r1b2q1k/2Qn1p1p/1p1Rpp2/p6B/4P2P/6N1/P4PP1/6K1 w - - bm e5; id ""arasan18.49""; c0 ""Amateur-Rybka, ACCA East-West team match, ICC 2008"";";
    //@"r1q2k1r/ppp2ppp/4n3/3N4/2B5/3Q4/Pb3PPP/4RRK1 w - - bm Rxe6; id ""arasan18.50""; c0 ""Akopian-Ramaswamy, Gilbraltar Masters 2007"";";
    //@"r4rk1/p4ppp/qp2p3/b5B1/n1R5/5N2/PP2QPPP/1R4K1 w - - bm Bf6; id ""arasan18.51""; c0 ""Alekhine-Sterk, Budapest 1921"";";
    //@"r2q1rk1/4bppp/3pb3/2n1pP2/1p2P1PP/1P3Q2/1BP1N1B1/2KR3R b - - bm Ra2; id ""arasan18.52""; c0 ""Gereben-Geller, Budapest 1952"";";
    //@"1rb4k/p5np/3p1rp1/1ppB4/2N2P2/1P2R1P1/P1P4P/4R1K1 w - - bm Re8+; id ""arasan18.53""; c0 ""Bagirov-Grigorian, USSR 1976"";";
    //@"r4rk1/1b2qppp/pp6/2p1P3/5P2/2RB4/PP2Q1PP/5RK1 w - - bm Bxh7+; id ""arasan18.54""; c0 ""Hebert-Krzyszton, corr olm11 fin B1 1992"";";
    //@"2b1rk2/5p2/p1P5/2p2P2/2p5/7B/P7/2KR4 w - - bm f6; id ""arasan18.55""; c0 ""study by Smyslov"";";
    //@"rnb1k2r/pp3ppp/3b4/q2pN1B1/8/3B4/Pp3PPP/1R1Q1RK1 w kq - bm Nxf7; id ""arasan18.56""; c0 ""Keres-Winter, Warsaw olm 1935"";";
    //@"rn1qr1k1/1p2bppp/p3p3/3pP3/P2P1B2/2RB1Q1P/1P3PP1/R5K1 w - - bm Bxh7+; id ""arasan18.57""; c0 ""Polugaevsky-Torre, London 1984"";";
    //@"4n3/2p5/1p2r2P/p1p2R2/P1N1k3/1PP4K/8/8 w - - bm Re5+; id ""arasan18.58""; c0 ""Anand-Landa, Bundesliga 0506 2005"";";
    //@"r2qr1k1/pp1nbppp/3p1n2/2pP2N1/2B2B2/3P4/PPP1Q1PP/4RRK1 w - - bm Nxf7; id ""arasan18.59""; c0 ""Volokitin-Kozul, Vidmar Mem. 2001"";";
    //@"r1bq1rk1/4pp2/p4bpB/1ppPp2n/2P1N3/1P3Pp1/1P1QB3/2KR3R w - - bm Bd3 Rxh5; id ""arasan18.60""; c0 ""Murray-Craig, WC36/sf04 ICCF 2012"";";
    //@"8/2p1k3/3p3p/2PP1pp1/1P1K1P2/6P1/8/8 w - - bm g4; id ""arasan18.61""; c0 ""Camacho Martinez-An. C. Hernandez, Cuba 1995"";";
    //@"r1b2rk1/pp2bppp/3p4/q7/3BN1n1/1B3Q2/PPP3PP/R4RK1 w - - bm Qxf7+; id ""arasan18.62""; c0 ""Velimirovic-Popovic, Yugoslavia 1986"";";
    //@"r1b1rk2/p1pq2p1/1p1b1p1p/n2P4/2P1NP2/P2B1R2/1BQ3PP/R6K w - - bm Nxf6; id ""arasan18.63""; c0 ""Gutsche-Jones, W-ch24 sf05 email 2000"";";
    //@"r2qr3/2p1b1pk/p5pp/1p2p3/nP2P1P1/1BP2RP1/P3QPK1/R1B5 w - - bm Bxh6; id ""arasan18.64""; c0 ""Gurgenidze-Klovans, Soviet Union 1959"";";
    //@"1rbq1rk1/p5bp/3p2p1/2pP4/1p1n1BP1/3P3P/PP2N1B1/1R1Q1RK1 b - - bm Bxg4; id ""arasan18.65""; c0 ""Grefe-Kaplan, Berkeley 1968"";";
    //@"k1b4r/1p3p2/pq2pNp1/5n1p/P3QP2/1P1R1BPP/2P5/1K6 b - - am Nxg3; id ""arasan18.66""; c0 ""Deep Fritz 13 Q6600-Arasan 17.2 Q6600, SSDF 2014"";";
    //@"7b/8/kq6/8/8/1N2R3/K2P4/8 w - - bm Nd4; id ""arasan18.67""; c0 ""study by Kubbel"";";
    //@"q3nrk1/4bppp/3p4/r3nPP1/4P2P/NpQ1B3/1P4B1/1K1R3R b - - bm Nc7; id ""arasan18.68""; c0 ""Karjakin-Anand, Corus Wijk aan Zee 2006"";";
    //@"2r5/8/6k1/P1p3p1/2R5/1P1q4/1K4Q1/8 w - - bm a6; id ""arasan18.69""; c0 ""Stockfish 091114 - Houdini 4, TCEC Season 7, Stage 3 2014"";";
    //@"8/3R1P2/1ppP1p2/3r4/8/K7/p4k2/8 w - - bm Kb2; id ""arasan18.70""; c0 ""A. Herbstmann, 1954"";";
    //@"4rr1k/p1p3p1/1p1bP2p/1N3p1q/4p2P/PQ2PnP1/1P3PK1/1BRR4 b - h3 bm Bxg3; id ""arasan18.71""; c0 ""Bauer-Bologan, Enghien-les-Bains 2001"";";
    //@"r7/n1rqppk1/p2p2p1/1b1P3p/2B4R/1P2QN1P/P4PP1/4R1K1 w - h6 bm Rxh5; id ""arasan18.72""; c0 ""Zhong Zhang-Sandipan, Mindsports Beijing 2008"";";
    //@"2qrrbk1/1b3ppp/pn1Pp3/6P1/1Pp2B2/1nN2NQB/1P3P1P/3RR1K1 w - - bm g6; id ""arasan18.73""; c0 ""Gulko-Popovic, Paris 1986"";";
    //@"r3r1k1/pp3p1p/3bb1BB/4q1Q1/8/7P/P4PP1/R2R2K1 w - - bm Rxd6; id ""arasan18.74""; c0 ""ET2008-Arasanx, ICC 2009"";";
    //@"2r2rk1/1b2bppn/1qp1p2p/p5N1/PpBPP2P/1P6/1BQ2PP1/1K1R3R w - - bm f4; id ""arasan18.75""; c0 ""Goloschapov-Skatchkov, Moscow 2004"";";
    //@"6k1/1p3p1p/5qp1/Q7/2Rp1P2/P2P3P/1P2r1P1/6K1 b - f3 bm Qh4; id ""arasan18.76""; c0 ""Arasan-Crafty, ICC 2009"";";
    //@"r1b1qr1k/pppn1pb1/4p1p1/4N1Bp/2BP2Q1/2P4R/P4PPP/4R1K1 w - h6 bm Rxh5+; id ""arasan18.77""; c0 ""Halkias-Perunovic, Kostic Memorial 2008"";";
    //@"6k1/Qb1q1pp1/2r4p/8/1P2P3/P4P2/2rp1NPP/R2R2K1 b - - bm Rc1; id ""arasan18.78""; c0 ""Arasan-Data, ICC 2009"";";
    //@"8/k4r2/1p3q2/p1p1n3/P3Pnp1/3p1PQ1/1P4PP/R2B1R1K b - - bm Nh5; id ""arasan18.79""; c0 ""Johnny-Junior, WCCCC Pampolona Openclass 2009"";";
    //@"r1b2rk1/pp1p2pR/8/1pb2p2/5N2/7Q/qPPB1PPP/6K1 w - - bm g3; id ""arasan18.80""; c0 ""Rybka-Shredder, WCCCC Pamplona Openclass 2009"";";
    //@"7q/3k2p1/n1p1p1Pr/1pPpPpQ1/3P1N1p/1P2KP2/6P1/7R w - - bm Nxd5; id ""arasan18.81""; c0 ""Nezhmetdinov-Zagorovsky, URS 1967"";";
    //@"3rr3/6k1/2p3P1/1p2nP1p/pP2p3/P1B1NbPB/2P2K2/5R2 w - - bm g4; id ""arasan18.82""; c0 ""Kasparov-Deep Blue (m/1) 1997"";";
    //@"3r2k1/pb3Np1/4pq1p/2pp1n2/3P4/1PQ5/P4PPP/R2R2K1 b - - bm Nxd4; id ""arasan18.83""; c0 ""Arasan-Crafty, ICC 2009"";";
    //@"r1bq1rk1/1p2b1pp/p1np4/8/2P1P1n1/N1N3B1/PP2BP1P/R2QK2R b KQ - bm Nxf2; id ""arasan18.84""; c0 ""Nunn-Nataf, FRA-chT France 1999"";";
    //@"8/p1p1r2p/1p5p/2p1r2k/2P1PR1P/5K2/PP6/6R1 w - - bm Rf5+; id ""arasan18.85""; c0 ""Boukal-Vavrla, CZE-ch15 corr01012 2001"";";
    //@"2kr1r2/ppq1b1p1/2n5/2PpPb1N/QP1B1pp1/2P5/P2N1P1P/R4RK1 b - - bm Rh8; id ""arasan18.86""; c0 ""Arasan-Q9550 (Rybka), ICC 2009"";";
    //@"2r2r1k/pBBq2p1/1p5p/4p3/8/2Q1PPPb/P6P/2R3K1 b - - bm e4; id ""arasan18.87""; c0 ""Arasan-Data, ICC 2009"";";
    //@"3r2k1/6p1/B1R2p1p/1pPr1P2/3P4/8/1P3nP1/2KR4 w - - bm Rc8; id ""arasan18.88""; c0 ""Ruffian-Arasan, test game 2009"";";
    //@"3qb1k1/5rb1/r3p1Np/1n1pP2P/p1pB1PQ1/2P5/R1B4K/6R1 w - - bm Bc5; id ""arasan18.89""; c0 ""Maximus-Akhtar, 90'+15, IC Freestyle Battle 2014"";";
    //@"2kr3r/1p2bpp1/p1p1pn1p/R3N2P/2PP4/1Q6/PP1B1Pq1/2KR4 w - - bm Rxa6; id ""arasan18.90""; c0 ""Data-Arasan, ICC 2009"";";
    //@"3q1k2/p4pb1/3Pp3/p3P3/r6p/2QB3P/3B1P2/6K1 w - - bm Bb5; id ""arasan18.91""; c0 ""MingLi (Rybka 3 Dyanmic)-Ultrapower (Rybka), playchess.com 2009"";";
    //@"r4r1k/ppqbn1pp/3b1p2/2pP3B/2P4N/7P/P2B1PP1/1R1QR1K1 w - - bm Rxe7; id ""arasan18.92""; c0 ""Q9550(Rybka)-Arasan, ICC 2009"";";
    //@"1r4k1/1q3pp1/r3b2p/p2N4/3R4/QP3P2/2P3PP/1K1R4 w - - bm Nf6+; id ""arasan18.93""; c0 ""Topalov-Lutz, Sparkassen Gp 1 2002"";";
    //@"r2q1r2/1bp1npk1/3p1p1p/p3p2Q/P3P2N/1BpPP3/1P1N2PP/5RK1 w - - bm Rf3; id ""arasan18.94""; c0 ""Spark 0.3-Doch 09.980, Johan Havegheer 2009"";";
    //@"2k1r2r/1pqb1p2/p2pp2b/4n1p1/PQ1NP2p/1P3P1P/2P1NBP1/R4RK1 w - - bm Nb5; id ""arasan18.95""; c0 ""Pacificrabbit (Rybka 3)-Anarquia69 (Rybka 3), playchess.com 2009"";";
    //@"2r3r1/1p1qb2k/p5pp/2n1Bp2/2RP3P/1P2PNQ1/5P1K/3R4 w - - bm Ng5+; id ""arasan18.96""; c0 ""Stockfish-Shredder, Pavel Háse 2010"";";
    //@"rn3rk1/pp1q3p/4p1B1/2p5/3N1b2/4B3/PPQ2PPP/3R2K1 w - - bm Nf5; id ""arasan18.97""; c0 ""Chiru-Tudor, ROM-ch2 email 2006"";";
    //@"rn1r2k1/p5bp/4b1p1/1Bp2p2/4qB2/2P2N2/P3QPPP/2R1K2R w K - bm Ng5; id ""arasan18.98""; c0 ""Giri-Howell, Corus B Wijk aan Zee 2010"";";
    //@"8/6rk/7p/1PNppp1n/1P6/P1r4P/5R1K/3R4 w - - bm Rg1; id ""arasan18.99""; c0 ""Petir-Arasan, Aser Huerga 2010"";";
    //@"rnbqk2r/pp1pbppp/2p5/8/2BP1p2/2P5/P1P1N1PP/R1BQ1RK1 w kq - bm Bxf7+; id ""arasan18.100""; c0 ""Mungo-Arasan, ICC 2010"";";
    //@"6k1/1p1q4/p1rP2p1/5p1p/5Q2/1P5P/5PP1/3R2K1 w - h6 bm g4; id ""arasan18.101""; c0 ""Shredder-Arasan, freechess.org 2010"";";
    //@"1qrrbbk1/1p1nnppp/p3p3/4P3/2P5/1PN1N3/PB2Q1PP/1B2RR1K w - - bm Bxh7; id ""arasan18.102""; c0 ""Georgiev-Ionescu, UAE 1986"";";
    //@"r1b2rk1/qp5p/p1n1ppp1/7N/4P1P1/2N1pP2/PPP5/2KR1QR1 w - - bm e5; id ""arasan18.103""; c0 ""Van Kempen-Kariz, EU-chT fin 03 corr 1999"";";
    //@"3r4/2q5/5pk1/p3n1p1/N3Pp1p/1PPr1P1P/2Q1R1P1/5R1K b - - bm g4; id ""arasan18.104""; c0 ""Critter 0.60-Deep Fritz 12, Martin Thoresen 2010"";";
    //@"r1b2r1k/ppqn2bp/3p2p1/3Q1p2/8/BP1BR2P/P1PP1PP1/4R1K1 w - - bm Re8; id ""arasan18.105""; c0 ""Nezhmetdinov-Kotkov, 17th RSFSR Ch 1957"";";
    //@"6r1/8/2k5/1pPp1p1p/pP3P2/P3P1P1/4K3/4B3 b - - bm d4; id ""arasan18.106""; c0 ""Chess Tiger 2007.1-Hiarcs 13.1, ChessWar XV A 2010"";";
    //@"r1b1k2r/2q2pp1/p1p1pn2/2b4p/Pp2P3/3B3P/1PP1QPP1/RNB2RK1 b kq - bm Ng4; id ""arasan18.107""; c0 ""Nataf-Volokitin, SCG-chT Budva 2004"";";
    //@"2r1rb1k/ppq2pp1/4b2p/3pP2Q/5B2/2PB2R1/P4PPP/1R4K1 w - - bm Rxg7; id ""arasan18.108""; c0 ""IvanHoe 57aU-x64.F-Deep Rybka 4, 2010"";";
    //@"3rr1k1/1bp3p1/1p4qp/pNp1p3/PnPn1pP1/3P1P2/1P2P1BP/1R1QBR1K b - - bm Nxf3; id ""arasan18.109""; c0 ""Arasan-bookbuilder, ICC 2010"";";
    //@"rnb1kb1r/pp1p1ppp/1q2p3/8/3NP1n1/2N1B3/PPP2PPP/R2QKB1R w KQkq - bm Qxg4; id ""arasan18.110"";";
    //@"r3kb1r/1b1n2p1/p3Nn1p/3Pp3/1p4PP/3QBP2/qPP5/2KR1B1R w kq - bm Qg6+; id ""arasan18.111""; c0 ""Morozevich - Vachier-Lagrave, Biel 2009 (analysis after 17. .. Bb7)"";";
    //@"1r1qrbk1/pb3p2/2p1pPpp/1p4B1/2pP2PQ/2P5/P4PBP/R3R1K1 w - - bm Bxh6; id ""arasan18.112""; c0 ""Future Breeze(Naum 4.1)-Ultrapower(Rybka 3), playchess.com 2010 (analysis)"";";
    //@"2r1r2k/1b1n1p1p/p3pPp1/1p1pP2q/3N4/P3Q1P1/1PP4P/2KRRB2 w - - bm g4; id ""arasan18.113""; c0 ""Blackborn(Deep Rybka 4)-Zax(Stockfish 1.8 JA), playchess.com 2010"";";
    //@"2r1b1k1/5p2/1R2nB2/1p2P2p/2p5/2Q1P2K/3R1PB1/r3q3 w - - bm Rxe6; id ""arasan18.114""; c0 ""bookbuilder-Arasan, ICC 2010"";";
    //@"rn2r1k1/ppq1pp1p/2b2bp1/8/2BNPP1B/2P4P/P1Q3P1/1R3RK1 w - - bm Bxf7+; id ""arasan18.115""; c0 ""Leitao-El Debs, Brazilian Chess Championship 2010"";";
    //@"1kr5/1p3p2/q3p3/pRbpPp2/P1rNnP2/2P1B1Pp/1P2Q2P/R5K1 b - - bm Bxd4; id ""arasan18.116""; c0 ""Argentum (Deep Rybka 4)-Obscure(Deep Rykba 4), playchess.com 2010"";";
    //@"r3r2k/1pq2pp1/4b2p/3pP3/p1nB3P/P2B1RQ1/1PP3P1/3R3K w - - bm Rf6; id ""arasan18.117""; c0 ""Houdini 1.5-Rybka 4, Pal Larkin 2011"";";
    //@"r3brk1/2q1bp1p/pnn3p1/1p1pP1N1/3P4/3B2P1/PP1QNR1P/R1B3K1 w - - bm Nxh7; id ""arasan18.118""; c0 ""Houdini 1.5-Rybka 4, Pal Larkin 2011"";";
    //@"1r3r2/q5k1/4p1n1/1bPpPp1p/pPpR1Pp1/P1B1Q3/2B3PP/3R2K1 w - - bm Rxd5; id ""arasan18.119""; c0 ""Ivanchuk-Erdos, Tradewise Gilbraltar 2011"";";
    //@"rq3rk1/1b1n1ppp/ppn1p3/3pP3/5B2/2NBP2P/PP2QPP1/2RR2K1 w - - bm Nxd5; id ""arasan18.120""; c0 ""Kaidanov-Donaldson, USA 1992"";";
    //@"7r/k4pp1/pn2p1pr/2ppP3/1q3P2/1PN2R1P/P1P2QP1/3R3K w - - bm a3; id ""arasan18.121""; c0 ""Gull 1.1-Hannibal 1.0, 2011"";";
    //@"3r1k2/pb1q1r2/npp5/3pB3/3PPPpb/PQ4N1/1P1R2B1/5RK1 w - - bm Bh1; id ""arasan18.122""; c0 ""Houdini 1.5-Rybka 4, Pal Larkin 2011"";";
    //@"1r3rk1/3bbppp/1qn2P2/p2pP1P1/3P4/2PB1N2/6K1/qNBQ1R2 w - - bm Bxh7+; id ""arasan18.123""; c0 ""nick-Fritz 11, playchess.com 2011"";";
    //@"1r1qrbk1/5ppp/2b1p2B/2npP3/1p4QP/pP1B1N2/P1P2PP1/1K1R3R w - - bm Bxh7+; id ""arasan18.124""; c0 ""Vandermeulen-Napalkov, Sergey Korolev mem. email 2008"";";
    //@"r5k1/pbpq1pp1/3b2rp/N3n3/1N6/2P3B1/PP1Q1PPP/R4RK1 b - - bm Rxg3; id ""arasan18.125""; c0 ""Arasan-KingsBishopFour, freechess.org 2011"";";
    //@"2r2bk1/3n1pp1/1q5p/pp1pPp1P/3P4/P1B2QP1/1P2N1K1/5R2 w - - bm Bxa5; id ""arasan18.126""; c0 ""Mamedyarov-Carlsen, Baku Grand Prix 2008"";";
    //@"r4r2/pp1b1ppk/2n1p3/3pPnB1/q1pP2QP/P1P4R/2PKNPP1/R7 w - - bm Qh5+; id ""arasan18.127""; c0 ""Bobras-Can, 8th ch-Euro Dresden 2007"";";
    //@"r4rk1/3b1ppp/p1np4/qp1N1P1Q/3bP3/4R3/PPB3PK/1RB5 b - - bm g6 Be5+; id ""arasan18.128""; c0 ""Chukichess-Arasan, freechess.org 2011"";";
    //@"2r1kb1r/1p1b1pp1/p5qp/3pP3/P2N1B2/7P/1PP2QP1/4RR1K w - - bm e6; id ""arasan18.129""; c0 ""Deep Rybka 4-Rick48-v4, WBEC Premier Div. 2011"";";
    //@"3r3k/2q2p1P/p7/Nr2p1P1/p3b2R/4B3/KbP2Q2/5R2 b - - bm f5; id ""arasan18.130""; c0 ""Arasan-Kec, freechess.org 2011"";";
    //@"8/2k2Bp1/2n5/p1P4p/4pPn1/P3PqPb/1r1BQ2P/2R1K1R1 b - - bm Nce5; id ""arasan18.131""; c0 ""Arasan-Rookie, freechess.org 2011"";";
    //@"rnq1nrk1/pb2bppp/1p2p2B/1N2N3/2B5/6Q1/PP3PPP/3R1RK1 w - - bm Nd6; id ""arasan18.132""; c0 ""Critter 1.0.1-Spike 1.4, ChessWar XVI 2011"";";
    //@"8/5kpp/8/8/8/5P2/1RPK2PP/6r1 w - - bm c4; id ""arasan18.133""; c0 ""Yandemirov-Shchukin, Chigorin Memorial, St. Petersburg 1997"";";
    //@"8/5k1K/2N2p2/2b1p1p1/1p2P1P1/1P3P2/P7/8 w - - bm Nd8+; id ""arasan18.134""; c0 ""Svidler-Jakovenko, Sochi FIDE GP 2008"";";
    //@"b4rk1/5p2/3q2p1/2p3n1/N2p1rBp/1PP1RP2/P3Q1PP/3R2K1 b - - bm h3; id ""arasan18.135""; c0 ""Trebreh33 (Deep Rybka 4)-MeisterGriller (Houdini 1.5a), playchess.com 2011"";";
    //@"8/3n4/1p1k1p2/p5p1/PP4P1/2B3K1/8/8 b - - bm b5; id ""arasan18.136""; c0 ""Spassky-Karpov, Leningrad cm(6) 1974"";";
    //@"1r1q2k1/2r3bp/B2p1np1/3P1p2/R1P1pP2/4B2P/P5PK/3Q1R2 b - - bm Ng4+; id ""arasan18.137""; c0 ""WHITE_RABBIT (Deep Rybka 4)-Blackborn, playchess.com 2011"";";
    //@"2r4k/1p2bpp1/pBqp1n1p/P3pP2/2r1P1P1/2N2Q1P/RPP1R3/5K2 b - - bm d5; id ""arasan18.138""; c0 ""Johnny 4.0-Scorpio 2.7, WBEC18 Premier 2011"";";
    //@"2r1rnk1/1p2pp1p/p1np2p1/q4PP1/3NP2Q/4B2R/PPP4P/3R3K w - - bm b4; id ""arasan18.139""; c0 ""Copie-Alvarez Rouchaud, Marcusi mem. corr 1996"";";
    //@"5qk1/1r1n3p/R4p1p/2pPpQ2/1r2P1B1/8/R5PK/8 w - - bm Ra8; id ""arasan18.140""; c0 ""Rookie-Arasan, freechess.org 2011"";";
    //@"rnb1k2r/1q2bppp/p2ppn2/1p6/3NP3/1BN3Q1/PPP2PPP/R1B2RK1 w kq - bm Nf5; id ""arasan18.141""; c0 ""Spark 1.0-Chronos 1.9.9, Swiss ChessGUI 2011 (G. Banks)"";";
    //@"1r1rkb2/2q2p2/p2p1P1B/P1pPp2Q/2P3b1/1P6/2B3PP/5R1K w - - bm Qxg4; id ""arasan18.142""; c0 ""Walsh-Rivas Romero, Latin Am zt15 sf2 corr 2000"";";
    //@"1n1rn3/3rbpk1/p1qpp2p/2p3p1/2P1NBP1/P1NR3Q/1P2PP1P/3R2K1 w - - bm Bxg5 Nxg5; id ""arasan18.143""; c0 ""Gustafsson-Postny, EU-chT Novi Sad 2009"";";
    //@"r4rk1/3b3p/p1pb4/1p1n2p1/2P2p2/1B1P2Pq/PP1NRP1P/R1BQ2K1 w - - bm Qf1; id ""arasan18.144""; c0 ""Ivanchuk-Kamsky, Tilburg 1992"";";
    //@"1r3rk1/4bpp1/p3p2p/q1PpPn2/bn3Q1P/1PN1BN2/2P1BPP1/1KR2R2 b - - bm Bxb3; id ""arasan18.145""; c0 ""Komodo 2.03-Spike 1.4, 29th Amateur Series Division 1, 2011"";";
    //@"2nb2k1/1rqb1pp1/p2p1n1p/2pPp3/P1P1P3/2B1NN1P/2B2PP1/Q3R2K w - - bm Nxe5; id ""arasan18.146""; c0 ""Spike 1.4-Booot 5.1.0, 29th Amateur Division 1, 2011"";";
    //@"3r2k1/p1qn1p1p/4p1p1/2p1N3/8/2P3P1/PP2QP1P/4R1K1 w - - bm Nxf7; id ""arasan18.147""; c0 ""Darkraider (Houdini 1.5a x64)-ABCDE, playchess.com 2011"";";
    //@"r2q1rk1/pb1nbp1p/1pp1pp2/8/2BPN2P/5N2/PPP1QPP1/2KR3R w - - bm Nfg5; id ""arasan18.148""; c0 ""Loop M1-P 4CPU-Deep Junior 10.1 4CPU, CEGT Quad 40/120 2007"";";
    //@"4rr2/3bp1bk/p2q1np1/2pPp2p/2P4P/1R4N1/1P1BB1P1/1Q3RK1 w - - bm Bxh5; id ""arasan18.149""; c0 ""Naum 4 x64 4CPU-Deep Fritz 11 4CPU, CEGT Quad 40/120 2009"";";
    //@"8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - bm g5 d4+; id ""arasan18.150""; c0 ""Kotov-Botvinnik, Soviet ch-22 Moscow 1955"";";
    //@"r4b1r/pp1n2p1/1qp1k2p/4p3/3P4/2P5/P1P1Q1PP/R1B2RK1 w - - bm Rb1; id ""arasan18.151""; c0 ""Tal-Simagin, Soviet ch-23 Leningrad 1956"";";
    //@"r4rk1/p1p3pp/1pPppn1q/1P1b1p2/P2Pn3/4PN2/1B2BPPP/R2Q1RK1 b - - bm Ng4; id ""arasan18.152""; c0 ""Korchnoi-Simagin, Soviet ch-27 Leningrad 1960"";";
    //@"r1bb1qk1/pp1p2p1/3Np2r/4p3/4B2P/2P5/PKP2PQ1/3R2R1 w - - bm Rd3; id ""arasan18.153""; c0 ""Houdini 2.0-Stockfish 2.1.1, Aser Huerga 2011"";";
    //@"rnbRrk2/2p5/1p2PB1p/pP4p1/8/P3R2P/2P2P2/6K1 w - - bm h4; id ""arasan18.154""; c0 ""Komodo 3-Rybka 4.1, Aser Huerga 2011"";";
    //@"8/5p2/3p2p1/1bk4p/p2pBNnP/P5P1/1P3P2/4K3 b - - bm d3; id ""arasan18.155""; c0 ""Bondarevsky-Petrosian, Soviet ch-19 Moscow 1951"";";
    //@"8/4nk2/1p3p2/1r1p2pp/1P1R1N1P/6P1/3KPP2/8 w - - bm Nd3; id ""arasan18.156""; c0 ""Grandelius-Raznikov, EU Youth Chess Ch B18, Albena 2011"";";
    //@"6k1/1bq1bpp1/p6p/2p1pP2/1rP1P1P1/2NQ4/2P4P/K2RR3 b - - bm Bd5; id ""arasan18.157""; c0 ""Hard CCC 2008 #6 (Vincent Lejeune)"";";
    //@"r3r1k1/1bqnbp1p/pp1pp1p1/6P1/Pn2PP1Q/1NN1BR2/1PPR2BP/6K1 w - - bm Rh3; id ""arasan18.158""; c0 ""Stockfish 2.1.1JA-Deep Saros ver. 2.3j, 2011 (Pavel Háse)"";";
    //@"1R1B1n2/2Rqbppk/p2p3p/3Pp3/5rPP/8/P3Q2K/8 b - - bm Qa4; id ""arasan18.159""; c0 ""Gaviota-Arasan, test game 2012"";";
    //@"3q4/4k3/1p1b1p1r/p2Q4/3B1p1p/7P/1P4P1/3R3K w - - bm b4; id ""arasan18.160""; c0 ""Stockfish 021113-Naum 4.6, nTCEC - Stage 4 - Season 2 2013"";";
    //@"8/5p1k/6p1/1p1Q3p/3P4/1R2P1KP/6P1/r4q2 b - - bm h4+; id ""arasan18.161""; c0 ""The King-Rybka 3i8 cluster, 28th Dutch CC, Leiden 2008"";";
    //@"7k/3q1pp1/1p3r2/p1bP4/P1P2p2/1P2rNpP/2Q3P1/4RR1K b - - bm Rxf3; id ""arasan18.162""; c0 ""Darkraider (Stockfish 2.1.1JA)-Pastorale(Houdini 1.5a), playchess.com 2011"";";
    //@"3r3r/k1p2pb1/B1b2q2/2RN3p/3P2p1/1Q2B1Pn/PP3PKP/5R2 w - - bm Rfc1; id ""arasan18.163""; c0 ""Naum 4.2 MP-Deep Hiarcs 13.2, SSDF 2011"";";
    //@"r1b3kr/pp1n2Bp/2pb2q1/3p3N/3P4/2P2Q2/P1P3PP/4RRK1 w - - bm Re5; id ""arasan18.164""; c0 ""Orcrist-Crafty, ICC 2011"";";
    //@"2r3k1/1q3pp1/2n1b2p/4P3/3p1BP1/Q6P/1p3PB1/1R4K1 b - - bm Rb8; id ""arasan18.165""; c0 ""Naum 4.2-Stockfish 2.2.1, Aser Huerga 2012"";";
    //@"rn2kb1r/1b1n1p1p/p3p1p1/1p2q1B1/3N3Q/2N5/PPP3PP/2KR1B1R w kq - bm Nxe6; id ""arasan18.166""; c0 ""Audino-Staroske, 1973 (anal. by Stockfish)"";";
    //@"r7/ppp3kp/2bn4/4qp2/2B1pR2/2P1Q2P/P5P1/5RK1 w - f6 bm Rxf5; id ""arasan18.167""; c0 ""Velimirovic-Romanishin, Odessa 1975"";";
    //@"8/2p4k/1p1p4/1PPPp3/1r6/R3K3/P4P2/8 w - - bm c6; id ""arasan18.168""; c0 ""Krzyzanowski-Pierzak, Sliwa mem email 2004"";";
    //@"1nr3k1/q4rpp/1p1p1n2/3Pp3/1PQ1P1b1/4B1P1/2R2NBP/2R3K1 w - - bm Qxc8+; id ""arasan18.169""; c0 ""Runting-Barnsley, Purdy Jubilee-A email 2003"";";
    //@"5bk1/2r2p2/b3p1p1/p2pP1P1/3N3R/qP2R3/5PKP/4Q3 w - - bm Nxe6; id ""arasan18.170""; c0 ""Zambor-Ohtake, W-ch26 sf12 email 2002"";";
    //@"5rk1/2p1R2p/r5q1/2pPR2p/5p2/1p5P/P4PbK/2BQ4 w - - bm d6; id ""arasan18.171""; c0 ""Observer(Houdini 2.0c Pro)-Numerobis(Fritz 13), playchess.com 2011"";";
    //@"r2q1r2/1b2bpkp/p3p1p1/2ppP1P1/7R/1PN1BQR1/1PP2P1P/4K3 w - - bm Qf6+; id ""arasan18.172""; c0 ""Asauskas-Malisauskas, Vilnius 2004"";";
    //@"b3r3/7k/2p3p1/2R4p/p3q1pP/2B3P1/3Q1P2/6K1 w - - bm Rc4; id ""arasan18.173""; c0 ""Ultrapower(Houdini 2.0b Pro)-Deep Abyss(Deep Rybka 4.1), playchess.com 2011"";";
    //@"1rr1b1k1/1pq1bp2/p2p1np1/4p3/P2BP3/2NB2Q1/1PP3PP/4RR1K w - - bm Rxf6; id ""arasan18.174""; c0 ""Glek-Lingnau, Dortmund op-A 1992"";";
    //@"r1r3k1/1q3p1p/4p1pP/1bnpP1P1/pp1Q1P2/1P6/P1PR1N2/1K3B1R b - - bm axb3; id ""arasan18.175""; c0 ""Great Ozzie(Komodo64 3)-Erdo(Critter 1.2), playchess.com 2011"";";
    //@"r1b2rk1/pppnq3/4ppp1/6N1/3P3Q/2PB4/P1PK2PP/3R3R w - - bm Nxe6; id ""arasan18.176""; c0 ""Leisebein-Woelfelschneider, BDG04-01 DVC 2001"";";
    //@"2r1r1k1/1p4bp/p3p1p1/qnnpP3/2p2PQP/2P1B1P1/1PB2N2/3R1RK1 w - - bm Bxg6; id ""arasan18.177""; c0 ""Komodo64 SSE Version 4-Rybka 4.1- SSE42 x64, Hamburg 2012 (Timo Klaustermeyer)"";";
    //@"3r1r1k/pp5p/4b1pb/6q1/3P4/4p1BP/PP2Q1PK/3RRB2 b - - bm Qxg3+; id ""arasan18.178""; c0 ""Apicella-Lautier, Clichy 2001"";";
    //@"r2r2k1/3bb1Pp/3pp1p1/p1q5/1p2PP2/P1N5/1PPQ4/1K1R1B1R w - - bm Nd5; id ""arasan18.179""; c0 ""Timman-Sisniega, Taxco izt 1985 (analysis by Timman)"";";
    //@"8/3r2R1/1p2kP2/p1p3P1/1nP4P/1K6/8/8 w - - bm Rg8 Rg6; id ""arasan18.180""; c0 ""Arasan-Scorpio, test game 2012"";";
    //@"8/2R5/3p4/3P4/3k3P/2p3K1/1r4P1/8 w - - bm Kf3; id ""arasan18.181""; c0 ""Flear-Legky, Le Touquet 1991 (anal. by Mueller)"";";
    //@"1k2qb1r/pp3ppb/2n1p3/2N1PP2/3B2P1/8/PP2Q3/2R2RK1 w - - bm Nxb7 Na6+; id ""arasan18.182""; c0 ""Stockfish 2.2.2JA SSE42-Rybka 4.1 SSE42, 2012 (Brent M)"";";
    //@"1r3rk1/2q4p/p1Np1bp1/1p1P4/n4pBP/2P2P2/PP1Q4/1K1R2R1 b - - bm Nxb2; id ""arasan18.183""; c0 ""Ohtake-Geist, Camara Mem-B email 2008"";";
    //@"2kr3r/pp4pp/4pp2/2pq4/P1Nn4/4Q3/KP2B1PP/2RR4 b - - am Qxg2; id ""arasan18.184""; c0 ""Eklund-Berthelsen, DSU Jubilee 100-B corr 2003"";";
    //@"5r2/1p4k1/pP1pP1pp/2rP2q1/4Qp2/3Bb3/P5PP/4RR1K w - - bm Rf3; id ""arasan18.185""; c0 ""Hybl-Juarez, WC35/pr08 ICCF 2011"";";
    //@"2rr1bk1/2q2p2/4pnp1/1P2p2p/p3P2P/5PP1/P1N1Q1K1/1NR2R2 b - - bm Bh6; id ""arasan18.186""; c0 ""Protector 1.4.0-Naum 4.2, Elite Quad 2012 (G. Banks)"";";
    //@"r2qr1k1/1b1pppbp/1p4p1/pP2P1B1/3N4/R7/1PP2PPP/3QR1K1 w - a6 bm Nf5; id ""arasan18.187""; c0 ""Godes-Morozov, Mirotvorsky mem-A corr8185 1981"";";
    //@"4k3/1R6/Pb3p2/1P1n4/5p2/8/4K3/8 w - - bm Kd3; id ""arasan18.188""; c0 ""Ding Liren-Wang Yue, ch-CHN 2012 Xinghua 2012"";";
    //@"r4nk1/2pq1ppp/3p4/p3pNPQ/4P3/2PP1RP1/Pr3PK1/7R w - - bm Ne3; id ""arasan18.189""; c0 ""Houdini 4 MP-Chiron 2 MP, SCCT Gladiators 2014"";";
    //@"r1b1k2r/ppqn1ppp/2pbpn2/4N3/2BP4/2N5/PPP2PPP/R1BQR1K1 w kq - bm Nxf7; id ""arasan18.190""; c0 ""Houdini 2.0c Pro x64 1CPU-Naum 4.2 1CPU, 2012 (Brent M)"";";
    //@"8/1r6/1PRP2pp/5p2/p4p2/P5k1/4K1P1/8 w - - bm Kd3; id ""arasan18.191""; c0 ""Seven of Seven (Stockfish 2.2.2 JA)-Halmatic (Deep Rybka 4.1), playchess.com 2012"";";
    //@"r2qrb1k/1p1b2p1/p2ppn1p/8/3NP3/1BN5/PPP3QP/1K3RR1 w - - bm e5; id ""arasan18.192""; c0 ""Spassky-Petrosian, World Chmp (19) 1969"";";
    //@"r2q1k1r/pp2n1pp/2nb1p2/1B1p3Q/N2P4/2P1B3/PP4PP/R4RK1 w - - bm Rxf6; id ""arasan18.193""; c0 ""Walhallathunder (Teodoriu Catalin)-rev786, 1st Masters Tournament, chess.com 2012"";";
    //@"4r1k1/6p1/bp2r2p/3QNp2/P2BnP2/4P2P/5qPK/3RR3 b - - bm Kh7; id ""arasan18.194""; c0 ""Komodo 4-Houdini 2.0c, Harry Schnapp Tribute 2012 (G. Banks)"";";
    //@"3rr1k1/pb3ppp/1p1p1bq1/3P4/1P2P3/P2Q1RBP/3N2P1/5RK1 w - - bm Rxf6; id ""arasan18.195""; c0 ""Bokar-Millstone, NAPZ-ch1 fin email 2001"";";
    //@"r7/p4k2/b1B1r3/2P2pBp/QPqp3P/P7/4P3/4R1K1 b - - bm Rd8; id ""arasan18.196""; c0 ""Stöckert-Shpakovsky, WCCC34SF06 ICCF 2010"";";
    //@"r2q1rk1/2pbnpb1/p2pp1pp/3nP1NP/1p1PNP2/3BB3/PPPQ2P1/2KR3R w - - bm Nf6+; id ""arasan18.197""; c0 ""Serban-Schuster, World Corr Ch 26 Final, 2010"";";
    //@"1r4k1/p7/3pR3/2pP1R1p/2P5/2P3q1/3n2P1/1rB2QK1 w - - bm Qe1 Qd1; id ""arasan18.198""; c0 ""Ingoc-Arasan, freechess.org 2012"";";
    //@"5k2/8/3pPp2/p1p3p1/Pp2PP2/1P3n2/7R/7K w - - bm Rh8+; id ""arasan18.199""; c0 ""Yilmaz-Lie, EU-chT (Men) 2011 (analysis)"";";
    //@"3R4/pp2r1pk/q1p3bp/2P2r2/PP6/2Q3P1/6BP/5RK1 w - - bm Rxf5; id ""arasan18.200""; c0 ""Zhak-Pavlov, ch-RUS sf ICCF 2010"";";
    //@"r3k3/1p4p1/1Bb1Bp1p/P1p1bP1P/2Pp2P1/3P4/5K2/4R3 w - - bm g5; id ""arasan18.201""; c0 ""Nakamura-Giri, FIDE Grand Prix, London 2012"";";
    //@"1r1rb1k1/5ppp/4p3/1p1p3P/1q2P2Q/pN3P2/PPP4P/1K1R2R1 w - - bm Rxg7+; id ""arasan18.202""; c0 ""Vescovi-Gschwendtner, Groningen op 1994 (Aemis Test Suite #30)"";";
    //@"1r1q1rk1/4bp1p/n3p3/pbNpP1PB/5P2/1P2B1K1/1P1Q4/2RR4 w - - bm Ne4; id ""arasan18.203""; c0 ""Rybka 4.1 SE42 x64-Komodo64 SSE Version 4, Clash of the Titans 2012"";";
    //@"r1br2k1/1p2bppp/pq6/2p1p3/1BPpBPn1/1P1N2P1/P1QPP2P/RN2R1K1 b - - bm Qh6; id ""arasan18.204""; c0 ""Quazar 0.4-Deep Hiarcs 14, 4CPU match 2012 (G. Banks)"";";
    //@"r1bq1rk1/pp2bppp/1n2p3/3pP3/8/2RBBN2/PP2QPPP/2R3K1 w - - bm Bxh7+; id ""arasan18.205""; c0 ""Trent-Deslandes, Cap d'Agde CCAS op 2008"";";
    //@"r6k/N1Rb2bp/p2p1nr1/3Pp2q/1P2Pp1P/5N2/P3QBP1/4R1K1 b - - bm Bh3; id ""arasan18.206""; c0 ""Persson-Gerhardt, Simon Webb mem email 2007"";";
    //@"r1b2rk1/1pq1nppp/pbn1p3/8/3N4/3BBN2/PPP1QPPP/3R1RK1 w - - bm Bxh7+; id ""arasan18.207""; c0 ""Leu-Muck, DDR-ch H137 corr 1973"";";
    //@"3r1rk1/1b2qp1p/1p3np1/1N1p4/6n1/2NBP1K1/PBQ2PP1/3RR3 b - - bm d4; id ""arasan18.208""; c0 ""KingOfJungle (Deep Fritz 13)-Branstonbear (Houdini 2.0c Pro), playchess.com 2012"";";
    //@"8/1B6/3r2p1/5p1p/5P1P/4k1P1/8/6K1 b - - bm g5; id ""arasan18.209""; c0 ""Felgaer-Dominguez, Capablanca mem Elite 2004""; c1 ""Speelman, Endgame Preparation, p. 95, 162""; c2 ""Speelman endgame test #55"";";
    //@"r3r2k/ppq3np/2p3p1/NPPp1bb1/P2Pnp2/3B1P2/2Q3PP/1RN1BRK1 b - - bm Ng3; id ""arasan18.210""; c0 ""Zambrana-Krush, 2nd American Continental 2003"";";
    //@"7k/5rp1/3q1p1p/2bNpQ1P/4P1P1/8/1R3PK1/8 w - - bm g5; id ""arasan18.211""; c0 ""Blackborn (Houdini 2.0c Pro)-Eroe Inrico (Gull II beta 2), playchess.com 2012"";";
    //@"4r3/4r3/1ppqpnk1/p3Rp1p/P2P1R1Q/2PB2P1/1P3P2/6K1 w - - bm Bxf5+; id ""arasan18.212""; c0 ""Jernjoffen (Naum 4)-Redboy (Cyclone 3.4), playchess.com 2009"";";
    //@"8/5q2/4kp1p/7R/6P1/5P2/6K1/8 w - - am Rxh6; id ""arasan18.213""; c0 ""Salov-Korchnoi, Wijk aan Zee 1997"";";
    //@"3R4/5p2/3b4/2q1p3/8/pP5Q/1k3P2/1B3K2 b - - bm Qd4; id ""arasan18.214""; c0 ""TrojanKnight-Arasan, freechess.org 2012"";";
    //@"r3nrk1/1pqbbppp/p2pp3/2n1P3/5P2/2NBBNQ1/PPP3PP/R4RK1 w - - bm Bxh7; id ""arasan18.215""; c0 ""Wedberg-Ionescu, Berliner Sommer 1998"";";
    //@"rnb2rk1/pp2qp1p/4p1pQ/3pP2P/5P2/3B4/P1P2BP1/R4RK1 w - - bm g4; id ""arasan18.216""; c0 ""Weldi2112 (Houdini 2.0c Pro)-Dinkelberger (Stockfish 2.3 JA), playchess.com 2012"";";
    //@"8/2N5/1P2p3/5bPk/1q3b2/3Bp2P/2P5/6QK b - - bm Kh4; id ""arasan18.217""; c0 ""Pinvoy (Houdini 1.5a)-Pawnattack (Houdini 2.0c Pro), playchess.com 2012"";";
    //@"1k1r1b1r/1p6/p4pp1/P1p1p3/2NpP1p1/1PPP2Pq/1B3P1P/2RQR1K1 b - - bm f5; id ""arasan18.218""; c0 ""ThomasTM (Komodo 8 64-bit)-Erdo (Komodo 8 64-bit), playchess All Engine Rapid Masters Q3 S 2014"";";
    //@"5r2/3rkp2/2R2p2/p2Bb2Q/1p2P2P/4q1P1/Pp6/1K1R4 b - - bm b3; id ""arasan18.219""; c0 ""Hotmorphy (Deep Shredder 9)-Mr Blue Sky (HIARCS 12), playchess.com 2012"";";
    //@"1kr4r/1p1bppb1/q1np1n2/p1p2PBp/2P5/P1NP2PP/1P1QN1B1/1R3R1K w - - bm b4; id ""arasan18.220""; c0 ""The Viper (Rybka 3 Dynamic x64)-Jernjoffen (Naum 4), playchess.com 2009"";";
    //@"r1b1k1r1/1p2np1p/p1n1pQp1/3p4/3NPP2/P2RB3/2PK2PP/q4B1R w q - bm Be2; id ""arasan18.221""; c0 ""Deep Junior 13.3 x64-Komodo 1176.00 x64 2014 (Clemens Keck)"";";
    //@"4r1k1/1p4p1/p1qBp1Qp/b1pnP3/8/5NP1/1P3PKP/3R4 w - - bm Rxd5; id ""arasan18.222""; c0 ""NVV5 (Houdini 2.0b Pro)-Deep Butterfly (Deep Fritz 13), playchess.com 2012"";";
    //@"3n4/5k2/8/2p1B1pp/1pP1Pp2/1P1K1P1P/8/8 b - - bm Kg6; id ""arasan18.223""; c0 ""Bronstein-Botvinnik, World Championship (m/10), Moscow 1951"";";
    //@"2r1k2r/pp1bb1pp/6n1/3Q1p2/1B1N4/P7/1q4PP/4RRK1 w k - bm Bxe7; id ""arasan18.224""; c0 ""Hannibal-Arasan, Variant-ICS, Amsterdam, NL 2013"";";
    //@"3b2k1/4qp2/2P4Q/3B3p/1P6/1K6/8/8 w - - bm Bc4; id ""arasan18.225""; c0 ""Murray-Aleshnya, WC30/ct06 ICCF 2010"";";
    //@"1r2brk1/6p1/1q2p1Pp/pN1pPPb1/np1N4/5Q2/1PP1B3/1K1R3R w - - bm f6; id ""arasan18.226""; c0 ""Aldrete Lobo-Maia, 9th Panamerican Team Chmp, ICCF 2007"";";
    //@"2rq1Nk1/pb3pp1/4p3/1p6/3b1Pn1/P1N5/1PQ3PP/R1B2R1K b - - bm f5; id ""arasan18.227""; c0 ""Aronian-Anand, Wijk aan Zee 2013"";";
    //@"r1b2rk1/1p4p1/p1n1p3/3p1pB1/NqP3n1/b2BP3/1PQN1P1P/1K4RR w - f6 bm Rxg4; id ""arasan18.228""; c0 ""Van Oosterom-Plomp, Korning mem 1998"";";
    //@"q2rn1k1/1b3p1p/1p4p1/2n1B1P1/r1PN3P/P4P2/4Q1B1/3RR1K1 w - - bm Bf6; id ""arasan18.229""; c0 ""Tarnowiecki-Timmerman, Millennium Email 2000"";";
    //@"8/1Kp2rkp/1pR5/pPb3p1/P2p2P1/3R1P2/8/8 w - - bm Rd1; id ""arasan18.230""; c0 ""Umansky-Rittner, World Champions Jubilee 2001"";";
    //@"3n1bk1/3nrp1p/3p1QpP/1q1P4/1p2N3/4BN2/1P3PP1/4R1K1 w - - bm Ra1; id ""arasan18.231""; c0 ""Van Oosterom-Bang, Elite Grandmasters Jubilee ICCF 2002"";";
    //@"8/5pk1/7p/P4RpP/6P1/6K1/5P2/r7 w - - bm Kh2 Kg2; id ""arasan18.232""; c0 ""Leko-Anand, Linares 2003"";";
    //@"r2q1rk1/2p2ppp/pb1p1n2/n3p3/P2PP3/2P2NN1/R4PPP/2BQ1RK1 w - - bm Bg5; id ""arasan18.233""; c0 ""Carlsen-Beliavsky, Corus-B Wijk aan Zee 2006"";";
    //@"nqr3k1/3b1p2/p2p2pp/1p1Pp1N1/1P2P3/1B5P/P2Q1PP1/5RK1 w - - bm Ne6; id ""arasan18.234""; c0 ""Anand-Carlsen, Morelia/Linares 2007"";";
    //@"1r2rbk1/1p1n1p2/p3b1p1/q2NpNPp/4P2Q/1P5R/6BP/5R1K w - h6 bm Ng3; id ""arasan18.235""; c0 ""Carlsen-Anand, Corus Wijk aan Zee 2008"";";
    //@"2r4k/1prnqp1p/p2p2p1/P2Pn1P1/1PPQ4/4B1R1/4B2P/2R4K w - - bm Bf4; id ""arasan18.236""; c0 ""Stockfish 14022619 x64-Houdini 4 x64, test game 2014"";";
    //@"1qr3k1/4Qpp1/p3p2p/1p2b3/2n1B3/2P3P1/PP5P/1K1RR3 b - - bm b4; id ""arasan18.237""; c0 ""Shirov-Kramnik, Linares 1997"";";
    //@"5b2/1b2qp1k/2pp1npp/1p6/1P2PP2/r1PQ2NP/2B3P1/3RB1K1 w - - bm e5; id ""arasan18.238""; c0 ""Steve Wraith (Deep Rybka 4 X3)-Fulcrum (Deep Rybka 4 x64), playchess.com 2010"";";
    //@"r3k1r1/p2n1p2/q1bBpn2/2P4p/1P2P1p1/2N1Q3/5PPP/3R1RK1 w q - bm e5; id ""arasan18.239""; c0 ""Kolesar-Schroeder, European TC VII Final ICCF 2008"";";
    //@"8/8/8/R7/5p2/2p5/5k2/1K3b1B w - - bm Bb7; id ""arasan18.240""; c0 ""Richard Becker, NONA 2007, miniatures, 2007 Prize"";";
    //@"r1b2r1k/4qp1p/p2ppb1Q/4nP2/1p1NP3/2N5/PPP4P/2KR1BR1 w - - bm Nc6; id ""arasan18.241""; c0 ""Kholmov-Bronstein, URS-ch32 Kiev 1964"";";
    //@"rqb1rnk1/5pb1/p1npp1p1/1p4Pp/4PP1Q/PNN1B2R/1PP1B2P/5RK1 w - - bm Bxh5; id ""arasan18.242""; c0 ""Smirnov-Kurnosov, WUCC op Istanbul 2004"";";
    //@"8/2k5/2PrR1p1/7p/5p1P/5P1K/6P1/8 w - - bm Rxd6; id ""arasan18.243""; c0 ""Topalov-Short, Madrid Magistral 1997""; c1 ""mate in 36"";";
    //@"8/4bBpp/3p4/P6P/2PN2p1/3k1b2/P7/6K1 w - - bm h6; id ""arasan18.244""; c0 ""Timman, 1c Corus {m} 2008"";";
    //@"8/8/8/2p1p3/8/pK6/1N6/1k4N1 w - - bm Nc4; id ""arasan18.245""; c0 ""V. Vlasenko, Sp.Prize, A.Guljaev & R.Kofman MT, 2009"";";
    //@"8/8/2B3r1/PP1PkpNp/2b4P/8/7K/8 w - - bm Nf7+; id ""arasan18.246""; c0 ""Mamedyarov-Bacrot, Baku FIDE GP 2008"";";
    //@"2r2rk1/1b1nbp2/1q1p2p1/RpnPp1Np/2p1P2P/2P1B1N1/1PB1QPP1/R5K1 w - - bm Nxh5; id ""arasan18.247""; c0 ""Angler01 (Stockfish 231113SL 6)-Numerobis(Komodo 6 64-bit), playchess.com 2013"";";
    //@"r1b1r1k1/pp3ppp/4p1n1/q1ppP1B1/6QP/P1PB3R/2P1KPP1/R7 w - - bm h5; id ""arasan18.248""; c0 ""Bologan-Vaganian, EU Club Cup 2006"";";
    //@"2nq2rb/3pr2k/1pp1p2p/p3Pp1B/3P1P2/PPN5/5Q1P/R5RK w - - bm Ne4; id ""arasan18.249""; c0 ""Kasparov-Jussupow, URS-ch49 Frunze 1981"";";
    //@"8/k3qrpR/1p1p4/p2QpPp1/P1P1P1K1/1P6/8/8 w - - bm b4; id ""arasan18.250""; c0 ""Keres-Euwe, Netherlands (m/6) 1940"";";
    #endregion

    #region Fields
    private Boolean disposed = false;
    private Parser? parser;
    #endregion

    #region Properties
    protected Parser? Parser {
      get => parser;
      set {
        parser?.Dispose();
        parser = value;
      }
    }

    internal GameState? State { get; set; }

    //[UCI]Diagnostic Info Enabled
    public static Boolean IsDebug { get; set; }
    public Boolean IsVerbose { get; }

    //[UCI]Registered Engine
    public static Boolean IsRegistered { get; set; }
    protected static Boolean IsRegistrationChecked { get; set; }

    protected static String? RegistrationCode { get; set; }
    protected static String? RegistrationName { get; set; }

    internal static String? DefaultEPD { get; set; }
    internal static String? DefaultFEN { get; set; }
    #endregion
  }
}
