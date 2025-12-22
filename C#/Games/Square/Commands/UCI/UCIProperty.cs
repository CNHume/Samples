//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2014-09-07 CNHume]Created File
//

namespace Commands;

using Engine;                           // For GameState

partial class UCI {
  //
  // Since 2020-02-20: Timing Results have been obtained using an i7-9700K CPU at 3.60GHz w 8-cores,
  // in a Dell XPS 8930 workstation with 32 GB RAM, running Windows 10 Pro.
  //
  #region FEN Constants
  private const String sDefaultFEN =
  #region Perft
  //
  //"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" //[2025-12-09 Perft1 (startpos) in 10.004 sec @13.032 MHz over 130.364 Mnode]
  //"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"; //[2025-12-09 Perft2 in 16.398 sec @12.54 MHz over 205.629 Mnode]
  //"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"; //[2025-12-09 Perft3 in 20.446 sec @10.703 MHz over 218.84 Mnode]
  //"r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"; //[2025-12-09 Perft4 in 66.368 sec @11.561 MHz over 767.28 Mnode]
  //"n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1"; //[2025-12-09 Perft5 in 9.252 sec @9.469 MHz over 87.606 Mnode]
  //
  #endregion                            // Perft

  #region Abbreviation Tests
  //
  //"4BR2/3N2nk/8/3N2PN/8/8/7K/8 w - - 0 1";    // ShowFrom
  //"4BR2/6nk/8/3N2PN/8/8/7K/8 w - - 0 1";      // ShowFile
  //"4BR2/3N2nk/8/3N2P1/8/8/7K/8 w - - 0 1";    // ShowRank
  //"8/7p/8/8/4N1K1/2q5/6k1/1N6 w - - 0 1";     // ShowFile || ShowRank
  //"8/8/3Q3Q/8/8/5q2/Q6Q/2Qbk1K1 w - - 0 1";
  //"8/6Q1/1Q6/5b2/B7/6r1/7Q/K1k5 w - - 0 1";
  //"4k3/8/8/8/4q3/2b5/3N1N2/4K3 w - - 0 1";    // Pin Test
  //
  #endregion                            // Abbreviation Tests

  #region Castling Tests
  //
  //"r3k3/8/8/8/8/8/8/4K2R w Kq - 0 1"; // Castling Test Position
  //"bnrbkrqn/pppppppp/8/8/8/8/PPPPPPPP/BNRBKRQN w FCfc - 0 1"; //[Chess960]
  // moves f2f4 c7c5 g1f2 d8b6 OO OOO
  //
  #endregion                            // Castling Tests

  //
  //"";
  //"r4r1k/p5p1/1p1R3p/q1nQ4/8/7B/P4PPP/5RK1 w - - 0 26";   // Rameshbabu Praggnanandha v Ian Nepomniachtchi 2024 FIDE Candidates R5 2024-04-09
  // 26. Qe5 Qxa2 27. Bf5 Kg8 (27... Rxf5 28. Qxf5) 28. Bb1 Qf7 29. Qb2 Qf4 30. Qa2+ Rf7 31. Rfd1 Rf8 32. f3 Qa4 33. Qxa4 Nxa4 34. Ba2 Nc5
  // moves d5e5 a5a2 h3f5 h8g8 f5b1 a2f7 e5b2 f7f4 b2a2 f8f7 f1d1 a8f8 f2f3 f4a4 a2a4 c5a4 b1a2 a4c5
  //"5rk1/p4rp1/1p1R3p/2n5/8/5P2/B5PP/3R2K1 w - - 0 35";    // Rameshbabu Praggnanandha v Ian Nepomniachtchi 2024 Line
  //
  //"8/8/3K1k2/5p2/5P2/4R3/5n2/8 w - - 0 1"; // Alice Lee + Wesley So v Cristian Chirila + Fabiano Caruana 2024-03-01 Endgame
  // 1. Ra3 Ng4 2. Ra8 Kf7 3. Rd8 Kf6 4. Rf8+ Kg6 5. Ke6 Ne3 6. Rf7 Nc2 7. Rf6+ Kg7 8. Kxf5 Nd4+ 9. Ke5 Nf3+ 10. Ke6 Nd4+ 11. Ke7
  // moves e3a3 f2g4 a3a8 f6f7 a8d8
  //"3R4/5k2/3K4/5p2/5Pn1/8/8/8 b - - 0 3"; // Alice Lee + Wesley So v Cristian Chirila + Fabiano Caruana 2024-03-01 Endgame Line
  //[2024-03-02 22-ply in 6:04:40.8 @1.661 MHz over 36.3345 Gnode] eval 9.75 after:
  // 3... Ne3 4. Ke5 Kg6 5. Rd6+ Kg7 6. Rd3 Nc2 7. Kxf5 Nb4 8. Rd7+ Kh6 9. Rd6+ Kh5 10. Ke5 Nc2 11. f5 Ne1 12. Rd1
  //[2024-04-28 18-ply in 18:18 @1.281 MHz over 1.407 Gnode] eval 6.8 after:
  // 3... Kf6 4. Rf8+ Kg6 5. Ke6 Ne3 6. Rh8 Ng4 7. Rg8+ Kh5 8. Rg5+ Kh4 9. Kxf5 Nf2 10. Rg2 Nd3 11. Rc2 Ne1 12. Rc3 Ng2 13. Rf3 Ne1 14. Rf3c3
  //[2024-04-28 22-ply in 2:06:27.87 @1.24 MHz over 9.4088 Gnode] eval 7.0 after:
  // 3... Kf6 4. Rf8+ Kg6 5. Ke6 Ne3 6. Rh8 Kg7 7. Rh5 Kg8 8. Rh3 Nd1 9. Kxf5 Kf7 10. Rh7+ Ke8 11. Rh3 Nf2 12. Rf3 Nh3 13. Ke5?! [13. Rxh3]
  // 13... Kd7 14. f5 Ng1 15. Rf1
  //"rn3rk1/1pqbbppp/p2pp3/P7/2BNP1n1/2N1B3/1PP1QPPP/R4RK1 w - - 0 12"; // Magnus Carlsen v Vincent Keymer 2023-10-05 European Chess Club Cup (R5)
  //[2023-10-06 16-ply in 1:24:48 @1.448 MHz over 7.319 Gnode] eval 0.92 after:
  // 12. Nf5 Bf6 13. Bb6 Qc6 14. Bd5 exd5 15. exd5 Qxc3 16. bxc3 Bxf5 17. Bd4 Nd7 18. Rab1 Rab8 19. Qf3 g6 20. Bxf6 Ndxf6
  //"8/8/6k1/7p/2K2p1P/1p3P2/8/8 w - - 0 1";  // Magnus Carlsen v Aryan Tari Endgame
  // moves c4b3 g6f5
  //"8/8/8/5k1p/5p1P/1K3P2/8/8 w - - 0 2";    // Magnus Carlsen v Aryan Tari Endgame Line
  //[2023-08-08 24-ply in 5:59 @1.315 MHz over 471.97 Mnode] eval 3.8 after:
  // 2. Kc4 Ke6 3. Kd4 Kd6 4. Ke4 Ke7 5. Kxf4 Kf6 6. Ke4 Ke6 7. f4 Kf6 8. f5 Kg7 9. Ke3 Kh6 10. Kf4 Kg7 11. Kg5 Kf7 12. f6 Kf7f8
  // 13. Kg5xh5 Kf8f7 14. Kh5g5 Kf7f8
  //"r1b1qk2/1p2b1pQ/pn5p/3n4/7B/4R2P/BP3PP1/6K1 w - - 0 1"; // Stockfish 16 v Caissa 1.11 2023-08-04
  //[2023-08-08 13-ply in 1:31 @1.734 MHz over 157.9 Mnode] eval 2.95 after:
  // 1. Bf6 Qf7 2. Qh8+ Qg8 3. Bxg7+ Kf7 4. Rxe7+ Kxe7 5. Qxg8 Bf5 6. Qxa8 Nxa8 7. Bxd5 Bc8 8. Bxh6 Nb6 9. Bg8
  //"8/8/8/5R2/2k1pb2/3p4/8/3K4 b - - 0 1";   // Carlsen v Duda Endgame
  // moves f4d6 f5f6 d6e5 f6f5 c4d4 f5f8 e4e3 f8d8 d4e4 d8e8
  //"4R3/8/8/4b3/4k3/3pp3/8/3K4 b - - 0 6";   // Carlsen v Duda Endgame Position with History
  //[2023-05-29 17-ply in 14:08.4 @1.114 MHz over 945 Mnode] eval -9.25 after:
  // 6... Kf4 7. Rd8 e2+ 8. Kd2 Bc3+ 9. Kxc3 e1=Q+ 10. Kc4 Qe4+ 11. Rd4 Qxd4+ 12. Kxd4 d2 13. Kc5 Ke3 14. Kc6 Ke2 15. Kb5 Ke1 16. Kb4 d1=Q
  //"4R3/8/8/4b3/4k3/3pp3/8/3K4 b - - 0 1";   // Carlsen v Duda Endgame Position sans History
  //[2023-06-30 17-ply in 13:51 @1.112 MHz over 923.9 Mnode] eval -9.62 after:
  // 1... Kf4 2. Re6 Bc3 3. Re7 Bb4 4. Re6 Kf3 5. Rf6+ Ke4 6. Re6+ Kf4 7. Kc1 Bc3 8. Re8 e2 9. Kb1 Be5 10. Rd8 e1=Q+
  // moves e4f4
  //"4R3/8/8/4b3/5k2/3pp3/8/3K4 w - - 0 7";   // Carlsen v Duda Endgame Line
  //[2023-06-17 19-ply in 2:03:20 @1.354 MHz over 10.02 Gnode] eval -11.85 after:
  // 7. Re6 Bc7 8. Ra6 Kf5 9. Ra2 Bg3 10. Rb2 Ke4 11. Rb4+ Kf3 12. Rb1 Bf4
  // moves e8e6 e5c7 e6a6 f4f5
  //"8/2b5/R7/5k2/8/3pp3/8/3K4 w - - 0 9";    // Carlsen v Duda Endgame Continuation
  //[2023-08-17 16-ply in 7:25.15 @1.172 MHz over 521.6 Mnode] eval -9.75 after:
  // 9. Ra2 Bg3 10. Rg2 Kf4 11. Rg1 Bf2 12. Rh1 Ke4 13. Re1 Bxe1 14. Kxe1 e2 15. Kd2 Kf4 16. Kxd3 e1=Q
  //
  //"5r1k/R3N1p1/5p1p/3P3P/8/2P3Q1/p4PPK/1bq5 w - - 0 42"; // Ding Liren v Ian Nepomniachtchi 2023 WCC R6
  // Mobility Disabled
  //[2023-09-04 16-ply in 6:03 @1.435 MHz over 520.87 Mnode] eval 12.95 after:
  // 42. Qc7 Qg5 43. Ng6+ Qxg6 44. hxg6 Rg8 45. Qf7 Bxg6 46. Qxg6 a1=Q 47. Rxa1 Rb8 48. Ra7 Rg8 49. Rf7 f5 50. Qxf5
  //
  //"5r2/R1Q1N1pk/5p1p/3P3P/8/2P5/p4PPK/1bq5 w - - 0 43"; // Ding Liren v Ian Nepomniachtchi 2023 WCC R6 #10
  // Mobility Enabled w 16Mx6 XPM w 96Mx2 XP w 32Mx4 QXP
  //[2024-05-12 19-ply in 14:36 @1.529 MHz over 1.339 Gnode] #10 after:
  // 43. Ng6 Rg8 44. Qf7 Qf4+ 45. Nxf4 a1=Q 46. Rxa1 Bf5 47. Ng6 Rd8 48. Ra7 Bd7 49. Rxd7 Rxd7 50. Qxd7 Kg8 51. Qc8+ Kh7 52. Qh8#
  // Mobility Enabled w 8Mx6 XPM w 48Mx2 XP w 16Mx4 QXP
  //[2024-03-08 19-ply in 1:11:29 @1.544 MHz over 6.622 Gnode] #11 after:
  // 43. Ng6 Qf4+ 44. Qxf4 Rg8 45. Ne7 Rd8 46. Qc7 Re8 47. Ng6 Rg8 48. Qf7 a1=Q 49. Rxa1 Rb8 50. Ra7 Rg8 51. Qxg8+ Kxg8 52. Ra8+ Kf7 53. Rf8#
  // Mobility Enabled, sans BestMoves
  //[2024-02-22 18-ply in 6:47.3 @1.641 MHz over 668.3 Mnode] #10 after:
  // 43. Ng6 Rg8
  // Mobility Disabled
  //[2024-03-24 19-ply in 15:52 @2.063 MHz over 1.9641 Gnode] eval 22.7 after:
  // 43. Ng6 Rg8 44. Qf7 Qf4+ 45. Nxf4 Be4 46. Rxa2 Bxg2 47. Nxg2 Rb8 48. d6 Rc8 49. d7 Rb8 50. Qg6+ Kg8 51. Re2 Rf8 52. Re8 Kh8 53. d8=Q
  //[2023-07-08 19-ply in 37:19 @1.346 MHz over 3.0146 Gnode] #10 after:
  // 43. Ng6 Rg8 44. Qf7 a1=Q (44... Qf4+ 45. Nxf4 a1=Q 46. Rxa1 Bf5 47. Ng6 Rd8 48. Ra7 Bd7 49. Rxd7 Rxd7 50. Qxd7 f5 51. Qc8 f4 52. Qh8#)
  // 45. Rxa1 Qf4+ 46. Nxf4 Bf5 47. Ng6 Rb8 48. Nf8+ Rxf8 49. Qxf8 Bh3 50. gxh3 f5 51. Ra7 f4 52. Qxg7#
  // Mobility Weight = 100 cp
  //[2023-10-02 19-ply in 39:51 @1.294 MHz over 3.0955 Gnode] #10 after:
  // 43. Ng6 Rg8 44. Qf7 a1=Q 45. Rxa1 Qf4+ 46. Nxf4 Bf5 47. Ng6 Rd8 48. Ra7 Bd7 49. Rxd7 Rxd7 50. Qxd7 Kg8 51. Qc8+ Kh7 52. Qh8#
  //
  //"6rk/R1Q1N1p1/5p1p/3P3P/8/2P5/p4PPK/1bq5 w - - 0 43"; // Ding Liren v Ian Nepomniachtchi 2023 WCC R6 Line
  //[2023-07-08 20-ply in 1:40:50 @1.735 MHz over 10.498 Gnode] eval 14.7 after:
  // 43. Nxg8 Qg5 44. Nxf6 Qxf6 45. Qb8+ Kh7 46. Ra8 Qh4+ 47. Kg1 Qxh5 48. Qg8+ Kg6 49. Qe8+ Kg5 50. f4+ Kxf4 (50... Kh4 51. Qe1+ Kg4 52. Qe2+ Kxf4 53. Qxh5 a1=N 54. Rxa1 [Bh7])
  // 51. Qxh5 a1=R 52. Rxa1 Bc2 53. Qf7+ Bf5 54. Qxg7
  //
  //"3r4/p1QP1pk1/1p2p1p1/2p2nP1/2P4q/P1P5/3R1P2/4KB2 w - - 0 32"; // Ding Liren v Ian Nepomniachtchi 2023 WCC R8
  //[2023-04-24 20-ply in 3 days 12:45:58 @1305.6 MHz over 398.4 Gnode] eval 2.5 after:
  // 32. Qxd8 Qe4+ 33. Be2 Qh1+ 34. Bf1 Qe4+@ 35. Re2 Qb1+ 36. Kd2 Qa2+ 37. Kd3 Qb1+ 38. Rc2 Qxf1+ 39. Kd2 Nd6 40. Qf6+ Kh7 41. Qf4 Nxc4+
  // 42. Qxc4 Qxc4 43. d8=Q b5
  //
  // The Giucco Piano: Aitken Variation presents a complex position after:
  // 1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. c3 Nf6 5. d4 exd4 6. cxd4 Bb4+ 7. Nc3 Nxe4 8. OO Nxc3 9. bxc3 Bxc3 10. Ba3
  //"r1bqk2r/pppp1ppp/2n5/8/2BP4/B1b2N2/P4PPP/R2Q1RK1 b kq - 0 10"; // Giucco Piano: Aitken Variation
  // 10... d5 (10... Bxa1? 11. Re1+ Ne7 12. Rxe7+ Qxe7 13. Bxe7 Kxe7 14. Qxa1 +-) 11. Bb5 Bxa1 12. Re1+ Be6
  //"r2qk2r/ppp2ppp/2n1b3/1B1p4/3P4/B4N2/P4PPP/b2QR1K1 w kq - 0 13"; // Giucco Piano: Aitken Variation Lines
  // 13. Qc2! (13. Qa4 Qd7
  // (13... Rc8? 14. Bxc6+ bxc6 15. Ne5 Qf6!?
  // (15... Qd6 16. Bxd6 cxd6 17. Nxc6 Bc3 18. Nxa7+ Kd8 19. Rxe6 fxe6 20. Nxc8 Kxc8 21. Qa8+ Kd7 22. Qxh8 Bxd4 +-)
  // 16. Qxc6+ Kd8 17. Rxa1 a6 18. Rb1 Qf5 19. Re1 Re8)
  // 14. Ne5 Nxe5 15. Bxd7+ Nxd7 16. Rxa1 Kd8)
  // 13... Qd7 14. Ne5 Nxe5 15. Bxd7+ Nxd7 16. Rxa1 OOO 17. Bd6 c6 18. Bf4
  //"2rqk2r/p1p2ppp/2p1b3/3p4/Q2P4/B4N2/P4PPP/b3R1K1 w k - 0 15"; // Giucco Piano: Aitken Variation Attack prior to 15... Qf6!?
  //
  //"8/8/4kpp1/3p1b2/p6P/2B5/6P1/6K1 b - - 0 47" // Veselin Topalov v Alexey Shirov 1998-03-04 [Too Deep to find Bh3!!]
  //
  //"8/2B5/8/8/2k5/3p4/5K2/q7 w - - 0 61"; // Hash = EA58B15C62619E25
  //"8/2B5/8/8/2k5/3p4/5K2/r7 w - - 0 61"; // Hash = 7FFFA6F98CBB4D46
  //"8/2B5/8/8/2k5/3p4/5K2/b7 w - - 0 61"; // Hash = 5DB8479F08C8DFEF
  //"8/2B5/8/8/2k5/3p4/5K2/n7 w - - 0 61"; // Hash = 721CAEAAD8483250
  //
  // moves f5h3 g2h3 e6f5 g1f2 f5e4
  //"8/8/5pp1/3p4/p3k2P/2B4P/5K2/8 w - - 3 50" // Veselin Topalov v Alexey Shirov 1998-03-04 Line
  //[2023-03-07 24-ply in 39:02:58 @1.088 MHz over 152.893 Gnode] eval -2.1 after:
  // 50. Bb4 Kd3 51. Bc5 d4 52. Ke1 f5 53. Kd1 Ke3 54. Kc1 f4 55. Kd1 Kf3? (55... f3) 56. Bxd4 Kg3 57. Be5 Kxh4 58. Bxf4 Kxh3 59. Bc1 Kg2 60. Ke1 Kg1 61. Be3+ Kh1
  //"8/8/5p2/3p4/p3k3/2B5/5K2/8 w - - 0 50"; // Veselin Topalov v Alexey Shirov 1998-03-04 Simplified
  //[2023-03-09 24-ply in 6:21:09 @1.074 MHz over 24.554 Gnode] eval -9.25 after:
  // 50. Bb2 d4 51. Bc1 Kd3 52. Bb2 f5 53. Ke1 Kc4 54. Ba1 f4
  // moves c3b2 d5d4 b2c1 e4d3 c1b2 f6f5 f2e1 d3c4 b2a1 f5f4
  //[55. Bb2 f3 56. Bc1 Kc3 57. Bd2+ Kc2 58. Bb4 d3 59. Kf2 a3 60. Bxa3 d2 61. Kxf3 d1=Q+ 62. Kf4 Qf1+ 63. Ke4 Qd3+ 64. Kf4 Qd3xa3]
  //"3R3r/5pbk/1pB1p1pp/4P3/5n1P/r1p2N2/P4PP1/2K4R w - - 0 1"; // Carlsen Mate #10
  //[2023-04-04 13-ply in 3.242 sec @1.261 MHz over 4.089 Mnode] after:
  // 1. Ng5+ hxg5 2. hxg5+ Bh6 (2... Nh5?! 3. Rxh5+ gxh5? 4. Be4+ f5 5. exf6# {ep})
  // 3. Rxh6+ Kg7 4. Rdxh8 Nd3+ 5. Kb1 c2+ 6. Kxc2 Rxa2+ 7. Kxd3 Ra3+ 8. Kd4 Rd3+ 9. Kxd3 b5 10. R6h7#
  // moves f3g5 h6g5 h4g5 f4h5 h1h5 g6h5
  //"3R3r/5pbk/1pB1p3/4P1Pp/8/r1p5/P4PP1/2K5 w - - 0 4"; // Carlsen ep Mate #2 [2-ply]
  //"8/8/5K2/4n3/4k1B1/7P/8/8 w - - 0 2" // Fischer v Taimanov Losing
  //[2023-01-02 17-ply in 3:42 @1.228 MHz over 272.8 Mnode] eval 6.15 after:
  // 2. Bc8 Kf4 3. h4 Kg3 4. h5 Kh4 5. h6 Kh5 6. Kg7 Ng6 7. Bg4+ Kg5 8. h7 Nh8 9. Bf5 Kxf5 10. Kxh8 Kf6 11. Kh8g8 Kf6e5
  //"8/8/5K2/3kn3/6B1/7P/8/8 b - - 0 1"; // Fischer v Taimanov Draw
  //[2023-01-01 17-ply in 5:34.6 @1.141 MHz over 381.8 Mnode] eval 1.48 after:
  // 1... Nd3 2. h4 Nf4 3. Kf5 Ne6 4. Be2 Kd6 5. Bc4 Nd4+? {losing} (5... Nf8! {only move}) 6. Kf6
  //"8/4k3/5nKP/8/8/1B6/8/8 w - - 0 1"; // Fischer v Taimanov Draw Test Position
  //"6k1/1p3ppp/3B2b1/1P6/p1PNr3/7P/1n3PP1/2R3K1 w - - 0 1"; // Arjun Erigaisi v Nihal Sarin Line
  // 1. c5  Nd3 ({game continuation} 1... Rxd4 2. c6 bxc6 3. Rxc6 Rc4 4. Bc5 h6 5. b6 Rc1+ 6. Kh2 +-)
  // 2. Rc4 Nb2 3. Rc3 Nd1 4. Rc1 Rxd4 5. c6 bxc6 6. b6 Rxd6 7. b7 Rd8
  //"3r2k1/1P3ppp/2p3b1/8/p7/7P/5PP1/2Rn2K1 w - - 0 8"; // Arjun Erigaisi v Nihal Sarin Prep
  //[20-ply in 444,971.9 sec = 5d 3:36:12 @1.3157 MHz over 585 Gnode] eval 0.83 after:
  // 8. f3! Nc3 9. Rxc3 Bf5 10. Rc4 Rd1+ 11. Kf2 Rb1 12. Rxa4 Rb2+ 13. Ke3 Rb3+ 14. Kd4 Kf8 15. Ra8+ Ke7 16. b8=Q Rxb8 17. Rxb8 Kd6 18. Rb6 h6 19. Rb8
  // 8. f3! Rf8? 9. g4 a3 10. Ra1 +-
  // 8. f3! Nc3! 9. Rxc3 {forced} Kf8 10. Rxc6 Bf5 11. Ra6 Rd1+ 12. Kf2 Rb1 13. Ra8+ Ke7 14. b8=Q Rxb8 15. Rxb8 Be6 16. Ra8 Bb3 +=
  //"1k1r4/1P2q3/1Q6/6p1/6Pp/7P/8/2R3K1 w - - 0 1"; // Promotion Fork [4-ply in 75 ms @121.77 KHz over 9,133 node] eval 3.25
  // 1. Rc8+ Rxc8 2. Qa7+ Kxa7 3. bxc8=N+ Ka6 4. Nxe7
  //"3r3r/pQpk1pp1/5n2/4n1p1/2Pqp1b1/3P2P1/PBP2PBP/1R3RK1 b - - 0 16"; // Richard Rapport v Shakhriyar Mamedyarov 2022-10-15 -#13
  //[16-ply in 4:13:12 @1.323 MHz over 20.095 Gnode] eval -2.47 after:
  // 16... Rxh2 17. Qb5+ c6 18. Qb7+ Ke6 19. Kxh2 Nf3+ 20. Bxf3 Rh8+ 21. Kg1 Bxf3 22. Qxc6+ Qd6 23. Qxd6+ Kxd6 24. Ba3+ Kc6 25. Rb6+ axb6 26. Rb1 Rh8h1#
  //[17-ply in ~19:17:55 @1.361 MHz over 73.9 Gnode] eval -2.67 after:
  // 16... Rxh2 17. Qb5+ c6 18. Qb7+ Ke6 19. Kxh2 Rh8+ 20. Kg1 Nf3+ 21. Bxf3 Bxf3 22. Qxc6+ Qd6 23. Qxd6+ Kxd6 24. Ba3+ Kc6 25. Rb6+ axb6 26. Bb2 Be2
  // moves h8h2 b7b5 c7c6 b5b7 d7e6 f1d1
  //"3r4/pQ3pp1/2p1kn2/4n1p1/2Pqp1b1/3P2P1/PBP2PBr/1R1R2K1 b - - 0 19"; // Richard Rapport v Shakhriyar Mamedyarov 2022-10-15 Line #-10
  //[18-ply in 16:53:25 @563.3 KHz over 34.25 Gnode] -#10
  // 19... Rdh8 (19... Rxh2+ 20. Kxg2 Bh3+ 21. Kg1 Nf3+ 22. Kh1 Qxf2
  // (22... Bf1! 23. Qxc6+ Kf5 24. Qb5+ Kg6 25. Qxg5+ Kxg5 26. Bc1+ Kg4 27. Rb5 Rh8+ 28. Bh6 Rxh6+ 29. Rh5 Rxh5#)
  // 23. Qxc6+ Kf5 24. Qc5+ Qxc5 25. Bd4 Bf1! 26. dxe4+ Kg4 27. Rb8 Rxb8 28. Rxf1 Rh8+ 29. Kg2 Rh2#)
  // 20. Qc8+ Rxc8 21. Bxd4 Rch8 22. f4 Bf3 23. Bxf3 exf3 24. Kf1
  // (24. f5+ Kxf5 25. g4+ Nfxg4 26. Kf1 Rg2 27. a3 Rh1 (27... Nh2+ 28. Ke1 Re2#) 28. Bg1 Rhxg1#)
  // 24... Rg2 25. Bg1 Rh1 26. f5+ Kxf5 27. g4+ Nfxg4 28. Rb5 Rhxg1#
  //
  //"3q4/k1p2br1/1pQ5/p2pR3/P2P4/1P1B4/K1P5/8 w - - 0 39"; // Fabiano Caruana v Ruslan Ponomariov 2014-07-13 #12 [11-ply in 3.253 sec @1.255 MHz over 4.08 Mnode] eval 10.35 after:
  // 39. Re7 Be8 40. Rxe8 Qxe8 41. Qxe8 c5 42. dxc5 bxc5 43. Qd8 c4 44. Qxa5+ Kb8 45. bxc4
  //"4Q3/k5r1/1p6/p1pp4/P2P4/1P1B4/K1P5/8 w - - 0 42"; // Fabiano Caruana v Ruslan Ponomariov 2014-07-13 #9 Line (from move 42)
  // 42. Qc6! cxd4 43. Qxd4 Re7 44. Qd8 Rb7 45. Be4 b5 46. axb5 Rxb5 47. Qc7+ Ka6 48. Qc6+ Rb6 49. Qc8+ Ka7 50. Qa8#
  //
  //"8/pR4pk/1b2p3/2p3p1/N1p5/7P/PP1r2P1/6K1 b - - 0 1"; // Martin Ortueta Esteban v. Jose Sans Aguado 1933
  //[2023-03-15 14-ply in 1:32.72 @1.247 MHz over 115.66 Mnode] eval -2.4 after:
  // 1... Rxb2 2. Nxb2 c3 3. Rxb6 c4 4. Na4 c2 5. Rxe6 c1=Q+ 6. Kh2 Qf4+ 7. Kg1 c3 8. Ra6 c2
  //"1r4k1/1p3pbp/1P1P4/p1P2p2/4p3/8/1P2B1PP/3R3K w - - 0 33"; // Carlsen v. Grischuk 2009-03-05
  //[2022-09-16 16-ply in 5:53 @1.271 MHz over 448.4 Mnode] eval 2.8 after:
  // 33. Ba6 Bf6 34. Bxb7 Rxb7 35. c6 Rxb6 36. Rc1 Rxc6 37. Rxc6 Kg7 38. d7 Be7 39. Rc8 e3 40. Re8 Kg7f6
  //[2022-09-16 17-ply in 1:20:28 @1.2 MHz over 5.795 Gnode] eval 2.65 after:
  // 33. Ba6 Bf6 34. Bxb7 Rxb7 35. c6 Rxb6 36. Rc1 Rxc6 37. Rxc6 Kg7 38. Rc8 a4 39. d7 f4 40. h3 f3 41. gxf3 exf3 42. d8=Q Bxd8 43. Rxd8
  //
  //"r1bq1rk1/pp2nppp/2n1p3/3pP3/1b1P4/2NB1N2/PP3PPP/R1BQK2R w KQ - 0 9"; // Greek Gift from French Advance
  //[2023-01-08 14-ply in 35:53 @1.479 MHz over 3.185 Gnode] eval 1.63 after:
  // 9. Bxh7+! Kxh7 10. Ng5+ Kg6 11. h4 Qa5 12. h5+ [12. Qd3+! f5 13. h5+ Kh6 14. Nxe6+ f4 15. Nxf8 Bf5 16. Bxf4+ g5 17. hxg6+ {ep} Kg7 18. Bh6+ Kg8 19. Qe3 Nxg6 20. Nxg6 Bxg6 21. Qg3!]
  // 12... Kh6 13. Nxe6+ Kh7 14. Nxf8+ Kg8 15. h6 Bxc3+ 16. bxc3 gxh6 17. Bc1xh6 Qa5xc3+ 18. Bh6d2 Qc3xd4
  //[2022-11-19 15-ply in 48:16 @1.452 MHz over 4.206 Gnode] eval 1.62 after:
  // 9. Bxh7+ Kxh7 10. Ng5+ Kg6 11. h4 Qa5 12. h5+ Kh6 13. Nxe6+ Kh7 14. Nxf8+ Kg8 15. h6 Bxc3+ 16. bxc3 gxh6 17. Bxh6 Qxc3+ 18. Kf1 Nxd4 19. Bd2 Qd3+
  //[2022-09-03 16-ply in 19:51:54 @1.415 MHz over 101.2 Gnode] eval 1.77 after:
  // 9. Bxh7+ Kxh7 10. Ng5+ Kg6 11. h4 Qb6? 12. a3?! [12. Qg4 Bxc3+ 13. bxc3 Nf5 14. h5+ Kh6 15. Nxf7+ Kh7 16. Qg6+ Kg8 17. Ng5 Rd8 18. Qf7+ Kh8 19. h6 Nxh6 20. Rxh6+ gxh6 21. Qh7#]
  // 12... Bxc3+ 13. bxc3 Re8 14. h5+ Kh6 15. Qd3 Nf5
  //"r1bq1r2/pp2npp1/2n1p1k1/3pP1N1/1b1P3P/2N5/PP3PP1/R1BQK2R b KQ - 0 11"; // Greek Gift from French Advance Line [2023-01-09 16-ply in 2:40:15 @1.508 MHz over 14.5 Gnode] eval 2.85 after:
  // 11... Qa5 12. Qd3+ Nf5 13. g4 f6 14. h5+ Kh6 15. Nxe6+ Kh7 16. Nxf8+ Bxf8 17. exf6 gxf6 18. gxf5 Ne7 19. h6 Bxf5 20. Qb5 Qxb5 21. Nxb5
  //"7k/6p1/3P3p/p7/P3Q1P1/8/6PK/3q4 w - - 0 46"; // Jordan Van Foreest v Mamedyarov #16 Line 2022 Oslo Esports Cup 2022-04-27
  //[2022-08-25 18-ply in 4:31.7 @1.407 over 382.3 Mnode] eval 9.85 after:
  // 46. Qe7 Qxg4 47. d7 Qh5+ 48. Kg3 Qg6+ 49. Kf4 Qxg2 50. d8=Q+ Kh7 51. Qd3+ Qg6 52. Qf5 h5 53. Qxg6+ Kxg6 54. Qg5+ Kf7
  // 55. Qxh5+ g6 56. Qxa5 Ke6 57. Qb6+
  //[2022-11-20 20-ply in 30:49.73 @1.537 MHz over 2.843 Gnode] eval 11.85 after:
  // 6. Qe7 Qxg4 47. d7 Qf4+ 48. Kh3 Qf5+ 49. Kg3 Qd3+ 50. Kh2 g5 51. d8=Q+ Qxd8 52. Qxd8+ Kg7 53. g4 Kf7 54. Qh8 Ke7
  // 55. Qxh6 Kf7 56. Qxg5 Kf8 57. Qxa5 Ke8 58. g4g5
  //
  // Enabling QuietMate avoided a quiet() stalemate anomaly [19-ply trace in 22:10] maintains eval 9.95 after:
  // 52. Qf5 Qxf5+ 53. Kxf5 h5 54. Kg5 h4 55. Kxh4 Kg6 56. Qg5+ Kf7 57. Qxa5
  // Disabling QuietMate exhibited the anomaly [19-ply trace in 35:28 @1.226 MHz] eval 11.0 after:
  // 49... Kh7 50. Qe4 Qxe4+ 51. Kxe4 Kg6 52. d8=Q h5 53. Qxa5 Kh6 54. Qb6+ Kg5 55. Qd8+ Kg4 56. Qd7+ Kh4 57. Qxg7 stalemate
  //"8/6Q1/8/7p/P3K2k/8/6P1/8 b - - 0 57"; // stalemate
  //
  //"8/6B1/1N4kP/2p5/2P5/1p6/2r5/1K6 w - - 0 56"; // Jordan Van Foreest v Liem Quan Le Line1 2022 Oslo Esports Cup 2022-04-28
  //"8/3N2Bk/7P/2p5/2P5/1p6/2r5/1K6 w - - 0 57"; // Jordan Van Foreest v Liem Quan Le #21 Line2 2022 Oslo Esports Cup 2022-04-28
  //"8/8/8/8/5b2/5k2/3p3p/5N1K b - - 0 1"; // Underpromotion -#11 [2023-02-25 17-ply in 1:41.46 @1.494 MHz over 151.618 Mnode]
  // 1... d1=B 2. Nxh2+ Kg3 3. Kg1 Be3+ 4. Kf1 Bh5 5. Ke1 Kxh2 6. Kf1 Bg4 7. Ke1 Kg3 8. Kf1 Be3d2
  //"2r5/5pk1/6p1/6P1/1K6/8/3R4/8 b - - 0 66"; // Vidit Gujrathi v Rameshbabu Praggnanandhaa Endgame 2022 Tata Steel Masters R10 2022-01-26
  //"3r2k1/2pqnpp1/1p5p/p2P4/P2p1rP1/2P1N1P1/1PQ2P2/3RR1K1 b - - 0 23"; // Nepomniachtchi v Carlsen 2021 WCC R11 2021-12-10 [~22-ply]
  // moves d4e3 g3f4 d7g4 g1f1 g4h3 f1g1
  //"3r2k1/2p1npp1/1p5p/p2P4/P4P2/2P1p2q/1PQ2P2/3RR1K1 b - - 0 26"; // Nepomniachtchi v Carlsen 2021 WCC R11 2021-12-10 Line
  // moves e3f2 c2f2 d8d6 f2f1 d6g6 g1f2 h3h2 f2e3 g6g3 e3d4 e7f5 d4e4 h2c2 d1d3 f5d6 e4d4
  //"6k1/4bpp1/1p1p4/5R1P/4PQ2/5P2/r4q1P/2R4K w - - 0 49"; // Carlsen v Karjakin 2016 WCC R16 2016-11-30 #8
  //[12-ply in 17.27 sec @1.516 MHz over 26.18 Mnode] #8 after:
  // 49. Rc8+ Bf8 50. Rxf8+ Kxf8 51. Rxf7+ Ke8 52. Rf8+ Kd7 53. Qf5+ Kc6 54. Rc8+ Kb7 55. Qd7+ Ka6 56. Ra8#
  //"2rq1rk1/pb1n1ppN/4p3/1pb5/3P1Pn1/P1N5/1PQ1B1PP/R1B2RK1 b - - 0 16"; // Aronian v Anand 2013-01-16 Tata Steel
  //[2023-02-27 16-ply in 10:55:50 @949.6 KHz over 37.366 Gnode] eval -0.53 after:
  // 16... Nde5 17. Bxg4 Bxd4+ 18. Kh1 Nxg4 19. Nxf8 Kxf8 20. Qh7 Nf2+ 21. Kg1 Nd1+ 22. Kh1 Nxc3 23. bxc3 Qd5 24. Qh8+ Ke7 25. Qh4+ Bf6 26. Qf2 Bxc3
  //"5R2/8/8/4k3/4p3/2r3P1/5P2/5K2 b - - 0 1"; // Firouzja v Mamedyarov
  //"8/8/8/6N1/8/7R/1K2PRn1/3q2k1 w - - 0 1"; // Zugzwang Threat [7-ply in 0.67 sec] eval 5.85
  //[2022-12-19 16-ply in 15:22.7 @1.39 MHz over 1.2824 Gnode] eval 7.75 after:
  // 1. Rh1+ Kxf2 2. Rxd1 Kxe2 3. Rd5 Ne1 4. Nh3 Nc2 5. Kxc2 Ke3 6. Ng1 Ke4 7. Rg5 Kf4 8. Nh3+ Kf3 9. Ra5 Kg3
  //[2022-03-13 16-ply in 10:53 @1.328 MHz over 866.64 Mnode] eval 6.15 after:
  // 1. Rh1+ Kxh1 2. Nf3 Qg1 3. Nxg1 Ne3 4. Nh3 Nd1+ 5. Ka1 Nxf2 6. Nxf2+ Kg2 7. e4 Kxf2 8. e5 Kf1 9. e6 Ke2 10. Kb2 Ke1 [11. e7]
  //"kr6/ppq3b1/2pNQ1p1/7p/7P/1R4P1/P4PB1/3n2K1 w - - 0 1"; // Awonder Liang v Gunay Mammadzada 2021-04-08 #10
  // Mobility Disabled
  //[2023-07-29 16-ply in 13:42 over 1.28 Gnode @1.558 MHz] eval 13.0
  //[2023-07-29 17-ply in 23:34 @1.433 MHz over 2.026 Gnode] #11
  //[2023-07-29 18-ply in 49:11 @1.434 MHz over 4.232 Gnode] #10 after:
  // 1. Rxb7 Rxb7 2. Qe8+ Qb8 3. Qxc6 Nc3 4. Nxb7 Ne2+ 5. Kh2 Nd4 6. Qd7 Qxg3+ 7. fxg3 Nf3+ 8. Bxf3 Be5 9. Qc8+ Bb8 10. Na5#
  // Mobility Weight = 100 cp
  //[2023-08-07 17-ply in 47:54 @1.539 MHz over 4.422 Gnode] eval 13.0
  //[2023-08-07 18-ply in 1:10:48 @1.463 MHz over 6.2143 Gnode] #10 after:
  // 1. Rxb7 Rxb7 2. Qe8+ Qb8 3. Qxc6 Nc3 4. Nxb7 Ne2+ 5. Kh2 Nd4 6. Qd7 Qxg3+ 7. fxg3 Nf3+ 8. Bxf3 Be5 9. Qc8+ Bb8 10. Na5#
  //"rn3rk1/pbppq1pp/1p2pb2/4N2Q/3PN3/3B4/PPP2PPP/R3K2R w KQ - 0 11"; // Edward Lasker v George Alan Thomas 1912-10-29 #7
  //[2022-11-20 10-ply 22.233 sec @1.432 MHz over 31.84 Mnode]
  //"rn3r2/pbppq1p1/1p2pN2/8/3P2NP/6P1/PPP1BP1R/R3K1k1 w Q - 5 18"; // Edward Lasker v George Alan Thomas 1912-10-29 #1
  //"1BK1NNBk/4Q1pp/2Q4Q/Q4Q2/3Q4/1Q4Q1/4Q3/R6R w - - 0 1"; // 218 Move Position
  //"R6R/4Q3/1Q4Q1/3Q4/Q4Q2/2Q4Q/4Q1pp/1BK1NNBk w - - 0 1"; // 218 Move Position Reflected
  //"8/8/8/3K4/4Q3/5p2/5k2/8 b - - 0 4"; // Q v BP #8
  //"8/8/8/8/5K2/8/4Qp2/6k1 w - - 0 8"; // Q v BP #4
  //"8/1KQ5/8/8/8/5p2/6k1/8 w - - 0 1"; // Q v BP loss for b
  //"8/6K1/5P2/8/8/8/1kq5/8 b - - 0 1"; // Q v BP loss for w
  //"8/8/8/6Q1/4K3/7k/5p2/8 w - - 0 8"; // Q v BP stalemate
  //"3k4/1P6/2K5/8/4N3/8/7b/8 b - - 0 1"; // Magnus Carlsen v. Ian Nepomniachtchi - MC Invitational (2020)
  // 1... Bb8? (1... Bc7! 2. Nc5 Bb8) 2. Nc5 Bh2 (2... Ke8 3. Na6 Ba7 4. Kc7 Ke7 5. Nb4 (5. Nb8?! Bg1 6. Kc8 Kd6 7. Nc6 Kxc6 8. b8=Q)
  // 5... Ke6 6. Nc6 Bc5 7. Kc8 Bd6 8. Nd4+ Kd5 9. Nb5 Bg3 10. Nc7+ Kc6 11. b8=Q) (2... Ba7 3. Na6 Ke8 4. Kc7 {xposing}) 3. Ne6+ 1-0
  //"1b1k4/1P6/2K5/8/4N3/8/8/8 w - - 0 2"; // After 1... Bb8?
  //"8/2N5/5R1p/2pP4/1pP3pP/1P2k3/r3n3/7K b - - 0 55"; // Xiong v Nakamura (0-1) 2019-03-31 US Championship R11 -#8
  //[2024-06-24 12-ply in 8.846 sec @1.421 KHz over 12.573 Mnode] -#8 after:
  // 55... Nd4 56. d6 Nf3 57. Nc7d5+ Ke3f2 58. Rf6xf3+ Kf2xf3 59. Nd5e3 Ra2a1+ 60. Ne3f1 Ra1xf1+ 61. Kh1h2 g4g3+ 62. Kh2h3 Rf1h1#
  //[2022-08-25 15-ply to find #-9 in 1:11 @1.497 MHz over 106.12 Mnode]
  // 55... h5 56. d6 Ra1+ 57. Kg2 Rg1+ 58. Kh2 g3+ 59. Kh3 Rh1+ 60. Kg2 Rh2+ 61. Kf1 g2+ 62. Ke1 Rh1+ 63. Rf1 gxf1=Q#
  //"r1bnknr1/1pp1qp2/p3p1p1/3pP1Np/3P3R/2NB1RQ1/PPP2PP1/2K5 w q - 0 1"; // Stockfish 8 v Alpha Zero (1-0) [Too Deep to find Bc4!]
  // See https://www.youtube.com/watch?v=Fwzq7qK6MMQ for Stockfish 8 variations
  // For example: moves d3c4 d5c4 d4d5 f8d7 h4c4 c7c6 c3e4 c6d5 e4d6 e7d6 e5d6 d5c4 g3h4 g8g7 g2g4 d7e5 g5f7 d8c6 f7e5 c6e5 h4f6 g7f7 f6h8 e8d7 f3f7 e5f7 h8h7 d7d6 h7f7
  //"8/8/6b1/3k4/8/1N5P/p7/3K4 b - - 0 77"; // Fedoseev v Carlsen 0-1
  //[2022-12-02 18-ply in 2:31 @1.275 MHz over 192 Mnode] eval -8.37 after:
  // 77... Kc4 78. Na1 Kc3 79. Kc1 Bd3 80. h4 Bg6 81. h5 Bf5 82. h6 Bh7 83. Kd1 Kb2 84. Kd2 Kxa1 85. Kc1 Bd3 86. Kd2 Kb2 87. Kxd3 a1=Q
  //[2022-12-03 20-ply in 2:18:28.6 @1.318 MHz over 10.95 Gnode] eval -11.55 after:
  // 77... Kc4 78. Na1 Kc3 79. Kc1 Bb1 80. h4 Bd3 81. Kd1 Kb2 82. h5 Kxa1 83. Kd2 Bb1 84. Kc3 Bh7 85. Kd4 Kb1 86. Ke5 a1=Q+ 87. Ke6 Bg8+ 88. Kf5 Bb3 89. Kf5g5
  //"8/n7/P7/8/2n5/8/2k5/K7 b - - 0 1"; // 2N v P -#11 [16-ply in 36.83 @1.489 MHz over 54.85 Mnode]
  // 1... Kb3 2. Kb1 Nb2 3. Kc1 Kc3 4. Kb1 Nd3 5. Ka2 [5. Ka1 Kc2 xposing to 7... Kc2] Kb4
  // 6. Kb1 [6. Ka1 Kb3 7. Kb1 Nb5 8. Ka1 Na3 9. a7 Ne1 10. a8=Q Nec2#] 6... Kb3 7. Ka1 Kc2 8. Ka2 Nb5
  // 9. Ka1 [9. a7 Nc1+ [or 9... Nb4+ 10. Ka1 Nd4] 10. Ka1 xposing to 10. a7] 9... Nc1 10. a7 Nc3 11. a8=Q Nb3#
  //"5k2/8/5pK1/5PbP/2Bn4/8/8/8 b - - 0 68"; // Carlsen v Caruana 2018-11-09 WCC R6 London
  // 68... Bh4! 69. Bd5 Ne2 70. Bf3 (70. Kh7 Bg5 71. Bf3 Ng3! 72. Kg6 (72. Bg4 Kf7 73. Kh8 Be3 74. Kh7 Bc5 75. h6 (75. Kh8?! Bf8 76. Kh7??)
  // 75... Bf8 76. Bh3 Ne4 77. Bg2 Ng5+ 78. Kh8 Bxh6 79. Bd5+ Kf8 80. Be4 Bg7#) 72... Kg8-+)
  // 70... Ng1 71. Bg4 (71. Bd5 Bg5 72. Kh7 Ne2 73. Bf3 Ng3 74. Bg4 Kf7 75. Kh8 Bc1 76. Kh7 Ba3) 71... Kg8-+
  //"8/8/4R3/5pk1/8/3B4/7p/2nK4 w - - 0 1"; // Blindfold Study given to Wesley So by Sagar Shah
  //[1. Be4 fxe4 2. Re5+ Kg4 3. Rxe4+ Kg3 4. Re1 Nd3 5. Rf1 Kg2 6. Ke2 Nf4+ 7. Ke1 Nh3 8. Rh1 Kxh1 9. Kf1=]
  // moves d3e4 f5e4 e6e5 g5g4 e5e4 g4g3 e4e1 c1d3 e1f1 g3g2 d1e2 d3f4 e2e1 f4h3 f1h1 g2h1 e1f1
  //"8/8/8/8/8/7n/7p/5K1k b - - 0 9" // Blindfold Study Solution: 50-Move Rule Draw
  //"8/8/8/8/8/7n/7p/5K1k b - - 0 1"; // Blindfold Study Test Position: 50-Move Rule Draw
  //"8/3k4/1Kn2p2/2P1p1p1/4P1Pp/2B2P1P/8/8 b - - 0 50";   // Maxime Vachier-Lagrave v Magnus Carlsen 2024-12-18 Champions Chess Tour Finals (R4)
  // Magnus Carlsen claimed 3-fold repetition after:
  // moves c6d8 b6b5 d8c6 b5a6 d7c7 a6b5 c7d7 b5b6 c6d8 c3e1 d8c6 e1c3
  // The arbiters incorrectly denied the claim.
  //"k1K5/7p/PB4pP/1P3pP1/5P2/3pP3/p1p5/rbQ5 w - - 0 2"; // Quiescent Mate Test
  //"k1K5/7p/PBN3pP/1P3pP1/4pP2/2p1P3/pp6/r5Q1 w - - 0 1"; // Solve #4 [go mate 4 over 613,162 nodes]
  //"7k/8/5N1P/8/2p5/2N5/8/3K3R w - - 0 1"; // Solve #4 [go mate 4 over 451,773 nodes]
  //
  //"8/1B6/p7/1p1p4/2p2n2/P1P1k3/1KP5/8 b - - 0 64"; // Caruana v Hou Yifan Grenke Chess Classic R6 2018-04-06
  //[2023-07-05 19-ply in 5:54.67 @1.2775 MHz over 453.1 Mnode] eval -2.35 after:
  // 64... Kd2 65. Bxa6 Nd3+! 66. Kb1 Ne1 67. Bxb5 Nxc2 (67... Kxc3 68. a4 Nxc2 69. Kc1 Nd4 70. Be8 Nb3+ 71. Kd1 d4 72. Ke2 d3+ 73. Ke1 Kd4)
  // 68. Ba4 Ne3 69. Kb2 Kd3 70. Be8 Nd1+ 71. Kc1 Nxc3 72. Bg6+ Ke3 73. Kc2 d4 74. Bf7 Nb5 75. a4 d3+ 76. Kb1
  //[2023-07-05 21-ply in 29:31 @1.178 MHz over 2.086 Gnode] eval -3.25 after:
  // 64... a5 65. Ba6 Kd2 66. Bxb5 Ne2 67. Be8 Nxc3 68. Bf7 Nd1+ 69. Kb1 Ne3 70. Bg6 d4 71. Be4 Kc3 72. Kc1 d3 73. cxd3 cxd3 74. Bxd3 Kxd3 75. a4
  // moves e3d2 b7a6 f4d3 b2b1 d3e1 a6b5
  //"8/8/8/1B1p4/2p5/P1P5/2Pk4/1K2n3 b - - 0 67"; // Caruana v Hou Yifan Grenke Chess Classic Line
  //[2023-06-29 20-ply in 17:08 @1.1446 MHz over 1.177 Gnode] eval -7.55 after:
  // 67... Kxc3 68. Kc1 Nxc2 69. a4 Nd4 70. Be8 Nb3+ 71. Kd1 Kb2 72. Ke2 d4 73. Kf2 Nc5 74. Bg6 d3 75. a5 d2 76. Bh5 c3 77. a6 Nxa6 78. Bf3 Na6b4
  // Mobility Enabled w 16Mx6 XPM w 96Mx2 XP w 32Mx4 QXP
  //[2024-05-13 20-ply in 12:11 @1.2545 MHz over 916.81 Mnode] eval -7.15 after:
  // 67... Kxc3 68. Kc1 Nxc2 69. a4 Nd4 70. Kd1 Kb3 71. Be8 c3 72. Bg6 Kxa4 73. Bf7 Nf5 74. Kc2 d4 75. Kd3 Nh4 76. Be8+ Kb4 77. Bb5?! Nf3 78. Ba6 Nf3e5+
  //
  //"8/8/4k2p/7p/5P2/6P1/6K1/8 w - - 0 1";  // Distant Opposition from Doluhanova v Roumegous 2017
  //[2023-08-17 24-ply in 15:16 over 1.122 Gnode @1.225 MHz] eval 1.85 after:
  // 1. Kf2 Kf6 2. Ke3 Ke6 3. Ke4 Kf6 4. f5 Kg7 5. Ke5 Kf7 6. f6 Ke8 7. Ke4 Kf8 8. Kf4! Ke8 9. Ke5 Kd7 10. Kf5 Kd6 11. g4?! Kd7 12. gxh5 Kd6 13. Kf5g6
  //[2023-08-17 28-ply in 2:21:46.51 over 9.5643 Gnode @1.124 MHz] eval 3.5 after:
  // 1. Kf2 Kf6 2. Ke3 Kf5 3. Kf3 Ke6 4. Ke4 Kf6 5. f5 Kf7 6. Ke5 Ke7 7. f6+ Ke8 8. Ke4 Kf8 9. Kf4! Ke8 10. Ke5 Kf7 11. Kf5 Ke8 12. Kg6 Kf8
  // 13. Kxh6 Kf7 14. Kg5 Kf8 15. Kxh5
  //"1k6/R7/K7/8/8/8/2p5/8 w - - 0 1"; // Miracle Draw 6-ply
  //
  //"4Qb1k/6pp/8/5r2/pn1B4/5N2/1Pq3PP/5RK1 w - - 0 37"; // Hou Yifan v Kacper Piorun 2017-09-04
  //[2022-11-20 13-ply in 1:43.4 @1.572 MHz over 162.6 Mnode] eval 4.5 after:
  // 37. g4! Qc6 38. Qxc6 Nxc6 39. gxf5 Nxd4 40. Nxd4+-
  //"r2qr1k1/pb3pbp/1p4p1/8/3N4/BPN3P1/P2Q3P/R2R1K2 b - - 0 21"; // Robert Byrne v Fischer 1963-12-18
  //"8/2k5/K7/1p6/3B4/8/P7/8 w - - 0 62"; // 2017-02-24 Nakamura v Grischuk Wrong Bishop Line
  //[17-ply in 12.23 sec @1.288 MHz over 15.76 Mnode] to find 62. Be5+
  //"Nn6/8/1pPk4/1K1bpB2/1p6/8/8/8 w - - 0 1"; // Fischer Endgame
  //"4k3/2R5/2n1r3/3R4/7P/5BP1/7K/4q3 w - - 0 54"; // 2017-01-21 Carlsen v Giri Line
  //"4r1k1/5pp1/1b4p1/3n4/3p2qP/3Q2P1/1P1B1P2/2N1R1K1 b - - 0 35"; // 2017-01-27 Carlsen v Adhiban Line
  //"5r2/4p1bk/3pQ1pp/Rp5q/1P1p4/3P2P1/3BPP1P/6K1 b - - 0 1"; // Forced Draw [9-ply]
  //"4k3/8/8/8/8/8/4P3/4K3 b - - 0 1";
  //"8/8/2k5/7p/8/2K5/8/8 w - - 0 1";   // Inside Games
  //"k7/7p/8/2nKb3/8/8/8/8 w - - 0 1";  // Wrong Colored Bishop [7-ply]
  //"1R3N2/5k2/8/6P1/8/4K3/8/5r2 w - - 0 62"; // Kramnik v Vachier-Lagrave 2013-08-27
  // 62. Nd7 Rf5 63. Rf8+ Kg6 64. Rg8+ Kf7 65. Ke4 Ra5 66. Rf8+ Kg7
  //[66... Kg6?! 67. Ne5+! Kxg5 68. Rf5+ Kh6 [68... Kh4 69. Ng6+] 69. Ng4+ Kg6 70. Rxa5]
  // 67. Kf4 Ra4+ 68. Kf5 Rd4 69. Rd8 Rd5+ 70. Kg4 Kf7 71. Rf8+ Kg7 72. Rg8+ Kf7 73. Nf6
  //"r1r3k1/2q1bp1p/4b1pQ/p2pP3/P2B1R2/2PB4/6PP/5R1K w - - 0 25"; // Kamsky v D'Costa 2016-02-13 #14 Capelle La Grande [double rook sacrifice]
  //"r4rk1/p4pb1/bp5p/2pNqPp1/6P1/7Q/PPP3P1/2KR2BR w - - 0 1"; // Ninov v Colovic 2015
  //"4rbk1/1ppr1p2/5R2/pP6/2Q5/P7/1B2q1PP/5R1K w - - 0 30"; // Giri v Sunilduth Lyna, Narayanan 2015-12-20 #6
  //[9-ply in 7 sec @1.451 MHz over 10.19 Mnode]
  //"6rk/5p2/Q2pP2p/pNp1b2n/P3P1Nq/5R1P/3P2P1/6K1 b - - 0 30"; // Carlsen v Grischuk LCC R9 2015-12-13
  //"1b1Q4/1P6/6k1/1B6/5p2/6PK/5q2/8 w - - 0 66"; // Carlsen v Topalov 2015-06-16 [0-1 R1 Time Forfeit]
  //[2022-09-01 15-Ply in 36:28 @1.314 MHz over 2.874 Gnode] eval 4.9
  //[2022-09-01 16-Ply in 48:24.6 @1.337 MHz over 3.885 Gnode] eval 8.05
  // 66. Bd3+! Kf7 67. Bc4+ Kg6 68. Qg8+ Kh6 69. Qf8+ Kg5 70. Qg7+ Kf5 71. g4+ Ke4 72. Qg6+ Kd4 73. Qb6+ Kxc4 74. Qxf2 Bd6 75. g5
  //
  // 66. Bd3+! Kf7 67. Bc4+ Kg6 68. Qg8+ Kf6 (68... Kh6 69. Qf8+ Kg5 70. Qg7+ Kf5 71. g4+ Ke4 72. Qg6+ Kd4 {xposing})
  // 69. Qf7+ Kg5 70. Qg7+ Kf5 71. g4+! Ke4 72. Qg6+ Kd4 (72... Kf3 73. Qc6+ Ke3 74. Qc5+ Kf3 75. Qd5+ Ke3 76. Qd3#)
  // 73. Qb6+ Kxc4 74. Qxf2 Kb5 75. Qa2 Kc5 76. Qa6
  //
  //"5Q2/5p1p/1pPr2p1/6k1/8/3pP2P/2q2PP1/3R1K2 w - - 0 44"; // Khismatullin v Eljanov 2015-03-06
  //[12-ply in 1:19.3 @1.317 MHz] eval 0.20
  //[13-ply in 3:52.4 @1.323 MHz] eval 0.87
  //"r1qr2k1/1p3pp1/2pbbn2/p3N3/3P3Q/P5PB/1B3P2/R3R1K1 w - - 0 1"; // Taimanov v Kuzminykh 1950
  //[13-ply in 1:40 @1.472 over 161.9 Mnode] eval 2.35
  //[14-ply in 5:59.1 @1.3865 MHz over 497.9 Mnode] eval 2.35
  //[15-ply in 15:59 @1.387 MHz over 1.33 Gnode] eval 2.55
  //"5rk1/5ppp/p1Q1p3/1r6/q2b4/4B1P1/P2RPP1P/1R4K1 w - - 0 26"; // Kasparov v Ribli 1989
  //[2023-08-31 20-ply in 1:54:41 @1.366 MHz over 9.401 Gnode] eval 0.0
  // 26. Rxb5 Bxe3 27. Rd8 Bxf2+ 28. Kxf2 Qxb5 29. Qd6 Qb5f5+ 30. Kg1 Qb1+
  // 31. Kg2 Qe4+ 32. Kh3! {with 14-ply needed to avoid checks} 32... Qf5+
  //"3R1rk1/5ppp/p2Qp3/8/8/6P1/P3P2P/1q4K1 w - - 0 31"; // Kasparov v Ribli 1989 Ending
  //[2023-08-31 21-ply in 24:41 @1.141 MHz over 1.691 Gnode] eval 4.08 after:
  // 33. g4 Qf1+ 34. Kg3 Qe1+ (34... Qg1+ 35. Kf3 Qf1+ {xposing}) 35. Kf3 Qf1+ 36. Ke3 Qh3+ 37. Kd4 Qxg4+ 38. Kc3 h5
  // (38... Qh3+ 39. Kb2 h6 40. Qxf8+ Kh7 41. Qh8+ Kg6 42. Rg8)
  // 39. Rxf8+ Kh7 40. Rxf7 Qxe2 41. Qg3 Qg4 42. Qxg4 hxg4 43. Re7 g3 44. hxg3
  //"r1b1r1k1/ppq2p1p/3b2pQ/3pn3/8/2P4P/PPBN1PP1/R1B1R1K1 b - -"; // Hit Rate 1 15.8% hits [plus 1.1% Qxnt] at 12-ply, expecting ~25%
  //"6k1/5p1p/P1pb1nq1/6p1/3P4/1BP2PP1/1P1Nb2P/R1B3K1 b - -"; // Hit Rate 2 26.6% at 12-ply [plus 4% Qxnt], exceeding ~30% Hits
  //"8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - -"; //[Fine 70]87% Hits at 37-ply, should solve by 26-ply
  //"8/1pr5/PR6/1P4kp/2K3p1/6P1/8/8 w - - 0 53"; // Karjakin v Morozevich 2012-11-29
  //"r5k1/1pq1Nppn/2pr4/p7/3P3Q/P5PB/1B3P2/R5K1 b - - 0 4";
  //"r2r2k1/1pq2ppn/2pbR1N1/p7/3P3Q/P5PB/1B3P2/R5K1 w - - 0 3";
  //"r1qr2k1/1p3pp1/2pbbn2/p3N3/3P3Q/P5PB/1B3P2/R3R1K1 w - - 0 1";
  //"1K1n2q1/1p4pR/1prPPpPn/1RB1p1rP/8/1B2p3/2p2b2/1k2N3 w - - 0 1";
  //"8/8/4N3/p4K2/8/3k1n2/8/8 w - - 0 1";
  //
  //"3k4/6p1/7p/1Pp1Np2/p3nPP1/3K4/7P/8 b - - 0 56"; // Hao v Carlsen 2012-07-30
  //[2022-11-19 19-ply in 12:38:22.5 @1.2 MHz over 54.6 Gnode] eval -2.45 after:
  // 56... Nf2+ 57. Kc3 Nxg4 58. Nd3 Nf6 59. Kb2 Ne4 60. Ka3 Kc7 61. Kxa4 Kb6 62. Ne5 Nc3+ 63. Kb3 Nxb5 64. Kc4 Na3+ 65. Kd5 g5
  // 66. Nd7+ Kc7 67. Nxc5 gxf4
  //
  // 56... fxg4! 57. Kxe4 a3 58. b6 Kc8 59. b7+ Kxb7 60. Nd3 a2 61. Nxc5+ Kc6 62. Nb3 h5-+
  // (56... Nf2+ 57. Kc4 Nxg4 58. b6 Kc8 59. Nc6 a3 60. Kb3 Kb7 61. Nd8+ Kxb6 62. Ne6
  // Nxh2 63. Nxg7 Nf3 64. Kxa3 Nd4 65. Kb2 c4 66. Nh5 Kc5 67. Kc3 Nb5+ 68. Kd2 c3+
  // 69. Kd3 Kb4 70. Ng3 Kb3 71. Nf1 Na3 72. Ne3 c2 73. Nxc2 Nxc2-+; 56... Nd6 57. gxf5 Nxb5
  // 58. Kc4 a3 59. Kb3 Ke7 60. Nc4 h5 61. Ne3 Kf6 62. h4 Nd4+ 63. Kxa3 Nf3-+)
  //
  //"2kr4/3q1R2/p1pp4/4p3/2P1n2r/4Q3/PBP4P/5R1K b - - 0 23"; // Harmon-Vellotti v Troff 2014-06-26 -#5 [6-ply in 0.2 sec]
  //"8/8/5pk1/6p1/1pp5/3q1P2/1P1N2P1/2K1R3 b - - 0 38"; // Gelfand v Inarkiev 2016-07-12 -#6 11-ply
  // underpromotion line: moves b4b3 c1d1 c4c3 e1e2 c3d2 e2e1 d3c2 d1e2 d2d1n e2f1 c2f2
  //"8/5k1N/4q1p1/3pB1Q1/r7/5P2/5KP1/8 w - - 0 44"; // Carlsen v Giri (1-0) 2016-07-22
  //[11-ply in 9.04 sec @1.188 MHz over 10.737 Mnode]
  //"2nq1nk1/5p1p/4p1pQ/pb1pP1NP/1p1P2P1/1P4N1/P4PB1/6K1 w - - 0 28"; // Fischer v Panno 1970
  //[2022-11-15 17-ply in 15:29.2 @1.5294 MHz over 1.421 Gnode] eval 2.45 after:
  // 28. Be4! Be8 29. hxg6 hxg6 30. Nh5 gxh5? 31. Bh7+ Nxh7 32. Nxh7 f6 33. Nxf6+ Kf7 34. Nxh5 Ke7 35. Qg7+ Bf7 36. Qf6+ Ke8 37. Ng7+ Kd7 38. Qxf7+ Qe7
  //[2024-04-25 20-ply in 3:39:39.58 @1.523 MHz over 20.068 Gnode] eval 3.00 after:
  // 28. Be4! Nd6 29. hxg6 hxg6 30. exd6 dxe4 31. Qh8+ Kxh8 32. Nxf7+ Kg7 33. Nxd8 Bd3 34. Nc6 Bb1 35. Nxa5 Bxa2 36. Nxe4 Bb1 37. Nc5 Bc2 38. g5
  //
  //"4b1q1/7k/3PpQp1/p2p4/1p1P2P1/1P1B4/P4P2/6K1 b - - 0 37"; // Fischer v Panno 1970 Line
  //"2r1q1k1/r4p1p/b3pBp1/n3P1QP/p2p3R/P5P1/2p2PB1/R5K1 w - - 0 2"; // Fischer Mate, 8-ply for #7
  //"r1bqr1k1/pp3ppp/2nB4/1N6/2Bn4/8/PP4PP/2RQR1K1 w - - 0 24"; // Kramnik v Meier 2012-07-22
  //"r1bqr3/pp5Q/2nB1kp1/1N6/3n4/8/PP4PP/2R1R1K1 w - - 0 4"; // Kramnik v Meier 2012-07-22 White Choice
  //"r1bqr3/ppN4Q/2nB1kp1/8/3n4/8/PP4PP/2R1R1K1 b - - 0 4"; // Kramnik v Meier 2012-07-22 Black Choice
  //"5k2/3n1p2/4p1rp/2q1PN2/1Q6/8/1P3PP1/4R1K1 w - - 0 1"; // Pin and Tempo
  //"8/2Q2pk1/7p/1r2Pp2/8/8/5PPK/8 w - - 0 8"; // Pin and Tempo1
  //"8/5pk1/7p/1PQ1Pp2/8/8/5PPK/r7 b - - 0 6"; // Pin and Tempo2
  //"1Q6/5pk1/8/4Pp1p/8/8/5PPK/8 w - - 0 12"; // Pin and Tempo3, #7 in 11-ply
  //"q1r3k1/5p1p/6pB/1p6/2bN4/2P1Q2P/5P2/r2BR1K1 w - - 0 35"; // Caruana v Gustafsson 2012-07-17
  //"q1r3k1/4Qp1p/6pB/1p6/3N4/2P4P/5P2/r2B1RK1 b - - 0 36"; // Caruana v Gustafsson 2012-07-17 Line
  //"5rk1/5p1p/5Qp1/1p6/3N4/2P4P/5P2/r2B1RK1 w - - 0 39"; // Caruana v Gustafsson 2012-07-17 #8
  //[2024-08-05 13-ply in 17.282 sec @1.754 MHz over 30.312 Mnode]
  //[2024-06-24 13-ply in 19.911 sec @1.621 MHz over 32.273 Mnode] bestLine
  //[2024-03-08 13-ply in 15.236 sec @2.115 MHz over 32.225 Mnode] bestLine
  //[2024-04-27 13-ply in 18.881 sec @1.605 MHz over 30.312 Mnode]
  // Mobility Enabled w 16Mx6 XPM w 96Mx2 XP w 32Mx4 QXP
  //[2024-05-12 13-ply in 17.088 sec @1.774 MHz over 32.312 Mnode] #8 after:
  // 39. Nf5 gxf5 40. Kh2 Re8 41. Bb3 Ra7 42. Rg1+ Kf8 43. Qg7+ Ke7 44. Rd1 Rd7 45. Qxf7+ Kd8 46. Qxd7#
  //"8/p3q1kp/1p2Pnp1/3pQ3/2pP4/1nP3N1/1B4PP/6K1 w - - 5 30"; // Botvinnik v Capablanca 1938 AVRO R11
  // Mobility Weight = 50 cp
  //[2023-08-03 15-ply in 3:43 @1.426 MHz over 318.1 Mnode] eval 2.25 after:
  // 30. Ba3!! Qe8 31. Qc7+ Kh8 32. Be7 Kg7 33. Qxa7 g5 34. Bd8+ Kg6 35. Qxb6 Nc1 36. Qb1+ Nd3 37. e7 Ng4 38. Nf1
  // Mobility Weight = 100 cp
  //[2023-08-03 15-ply in 5:33 @1.415 MHz over 455.625 Mnode] eval 2.45 after:
  // 30. Ba3!! Qe8 31. Qc7+ Kh8 32. Be7 Kg7 33. Qxa7 Nd2 34. Bd8+ Kf8 35. Bxf6 Qxe6 36. Be5 h5 37. Bg7+ Ke8 38. h4 Qe6e3+
  //[2024-04-28 16-ply in 7:09 @1.357 MHz over 582.445 Mnode] eval 3.25 after:
  // 30. Ba3!! Qe8 31. Qc7+ Kg8 32. Be7 Kg7 33. Qxa7 Na5 34. Bd8+ Kf8 35. Bxf6 Qxe6 36. Be5 Qf7 37. Qa7xb6 Na5b7 38. Ng3f1 Qf7d7
  // Mobility Weight = 200 cp
  //[2023-07-11 15-ply in 8:26 @1.369 MHz over 692.86 Mnode] eval 2.5 after:
  // 30. Ba3!! Qe8 31. Qc7+ Kh6 32. Be7 Kg7 33. Qxa7 g5 34. Qc7 Kg6 35. Bxf6 Kxf6 36. Qe5+ Kg6 37. Qxd5 Nd2
  // Capablanca played 30... Qxa3? 31. Nh5+ gxh5 32. Qg5+ Kf8 33. Qxf6+ Kg8 34. e7
  // (34. Qf7+ Kh8 35. g3! Nxd4!? 36. e7 Qc1+ 37. Kg2 Qc2+ 38. Kh3 Qf5+ 39. Qxf5 Nxf5 40. e8=Q+)
  // 34... Qc1+ 35. Kf2 Qc2+ 36. Kg3 Qd3+ 37. Kh4 Qe4+ 38. Kxh5 Qe2+ 39. Kh4 Qe4+ 40. g4 Qe1+ 41. Kh5
  // moves b2a3 e7e8 e5c7 g7h6 a3e7 h6g7 c7a7
  //"4q3/Q3B1kp/1p2Pnp1/3p4/2pP4/1nP3N1/6PP/6K1 b - - 0 33"; // Botvinnik v Capablanca 1938 AVRO R11 Line
  //[2023-07-11 16-ply in 15:07 @1.47 MHz over 1.332 Gnode] eval 4.1 after:
  // 33... Nd2 34. Bd8+ Kf8 35. Bxf6 Qxe6 36. Be5 h5 37. Qg7+ Ke8 38. Qh6 Ne4 39. Nxh5 Qg4 40. Nf6+ Nxf6 41. Qh8+ Kd7 42. Bxf6 Qd1+ 43. Kf2
  //"8/5B2/8/8/5KNk/8/8/8 b - - 0 13";  // KBN v K #9 [2022-11-20 15-ply in 18.37 sec @1.5246 MHz over 28 Mnode]
  //"8/5b2/8/8/5knK/8/8/8 w - - 0 13";  // KBN v K -#9 [2022-11-20 15-ply in 16.78 sec @1.467 MHz over 24.615 Mnode]
  //"8/8/3n1b2/8/7K/1k6/8/8 w - - 0 67"; // Paehtz v Hou Yifan (0-1) 2016-10-08 Isle of Man [KBN v K Endgame] Deeper
  //"8/8/3n1b2/8/5K2/3k4/8/8 w - - 4 69"; // Paehtz v Hou Yifan (0-1) 2016-10-08 Isle of Man [KBN v K Endgame] Easier
  //"8/8/3n4/8/3b4/8/4k3/7K b - - 9 71"; // Paehtz v Hou Yifan KBN -#21
  //[23-ply in 4:18:07 @1.095 MHz over 16.957 Gnode] to find #29
  //"8/8/8/8/8/5k2/3n3b/4K3 b - - 0 79"; // Paehtz v Hou Yifan KBN -#12
  //[18-ply in 33.4 sec @1.6 MHz over 53.4 Mnode]
  //"8/8/7K/4k2P/6b1/6n1/8/8 w - - 0 1"; // KBN v K Endgame Test #26
  // 1. Kg6 Nxh5 2. Kh7 Nf4 3. Kg7 Be6 4. Kh7 Kf5 5. Kh6 Bg8 6. Kg7 Bc4 7. Kh6 Ng6 8. Kh5 Bg8 9. Kh6 Kf6 10. Kh5 Ne5
  // 11. Kh4 Kf5 12. Kg3 Ng4 13. Kf3 Bc4 14. Kg3 Bd5 15. Kh3 Kf4 16. Kh4 Be6
  // 17. Kh5 Bf7+ 18. Kh4 Ne3 19. Kh3 Be6+ 20. Kh4 Nf5+ 21. Kh3 Kf3 22. Kh2 Ne3 23. Kg1 Kg3 24. Kh1 Kf2 25. Kh2 Nf1+ 26. Kh1 Bd5#
  //"8/8/8/4nk1b/8/6K1/8/8 w - - 0 14"; // KBN v K Endgame Test Line #12
  //[2023-07-26 17-ply in 30.56 sec @1.554 MHz over 47.494 Mnode]
  //"6k1/p4R1p/1p5q/5Q2/2Pb4/8/P6P/7K w - - 0 1"; // Reinfeld Combo #357
  //"8/5p2/1P4p1/7p/r2kp2P/2RbN1P1/5P1K/8 w - - 0 61"; // Radjabov v Karjakin 2012
  //"5r1k/2P4p/1q2Np2/3r4/6p1/2Q1Rb2/1p5P/4R1K1 w - - 0 42"; // Nakamura v Adams 2011 Line
  //[2022-08-19 13-ply in 3:55 @1.488 MHz over 349.6 Mnode] eval 3.70 after:
  // 42. Nxf8 b1=Q 43. c8=Q Rd8 44. Qxd8 Qxe1+ 45. Qxe1 Qxd8 46. Nd7 Kg7 47. Re8 Qc7 48. Re7+ Kg8 49. Nxf6+
  //"3r2k1/8/5RPK/6NP/2b5/8/8/8 w - - 0 66"; // Caruana v Aronian 2014-02-03 Zurich R5 #8
  //[2022-08-19 15-ply in 6:18.51 @1.253 MHz over 474.2 Mnode]
  // to find 66. Nh7 Re8 67. Rc6 Be6 68. Rc7 Rf8 69. Rg7+ Kh8 70. Nxf8 Ba2 71. Nd7 Bb1 72. Nf6 Bxg6 73. Rg8#
  //"1k5r/1r1B2pp/1PQ5/4pp2/R7/3q3P/5PP1/6K1 w - - 0 1"; // Mavo's Nice Tactics!
  //[12-ply in 18 sec @1.472 MHz] eval 7.85
  //[16-ply in 6:46.6 @1.469 over 597 Mnode] eval 9.1
  // 1. Bxf5 Qd1+ 2. Kh2 Qxa4 3. Qxa4 Rxb6 4. Be4 Kc8 5. Qa7 Rf6 6. Qa8+ Kd7 7. Qxh8 Rxf2 8. Qxg7+ Ke6 9. Qg8+ Ke7 10. Qd5 Kf6 11. Qd6+ Kg5 12. Qxe5+
  //"1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PPK/8 w - - 0 4"; // Mavo Nice Mate1 #6
  //[2022-09-01 10-ply in 2.788 sec @1.259 MHz over 3.511 Mnode]
  // 4. Ra6 Rd8 5. Qa4 Ra7 6. Rxa7 Rd5 7. Ra8+ Kb7 8. Bxd5+ Kxb6 9. Qc6#
  //"1k1q3r/1r4pp/1PQ5/4pB2/R7/7P/5PP1/6K1 w - - 0 2"; // Mavo Nice Mate2 [longer]
  //"1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PP1/6K1 w - - 0 3"; // Mavo Nice Mate3 #8
  //[12-ply in 14.17 sec @1.577 MHz]
  //"r1bk3r/2p2ppp/1pK5/p2pp3/8/P7/1PPP2PP/R1BQ2NR b - - 0 1"; // Hamppe v Meitner, 7-ply
  //"8/4Qpk1/5np1/2p4p/2Pp4/P3p1P1/4q2P/5RK1 b - - 0 39"; // Aronian v Caruana Line
  //[17-ply in 2:45.54 @1.141 MHz over 94.77 Mnode] eval 3.63
  //"8/1R3pk1/6p1/P1p4p/8/3pp1P1/7P/5K2 b - - 0 46"; // Aronian v Caruana Rook Behind
  //"r6k/p3p2p/2b3p1/2p3B1/3b3Q/6PP/Pr6/R4K2 w - - 0 31"; // Johannessen v Fischer Line
  //"4r2k/p3B2p/6p1/2p5/P6Q/1b4PP/2r5/b2K4 w - - 0 37"; // Johannessen v Fischer -#8
  //[2023-01-07 13-ply in 37.268 sec @1.582 MHz over 58.97 Mnode]
  // Mobility Enabled w 16Mx6 XPM w 96Mx2 XP w 32Mx4 QXP
  //[2024-05-12 13-ply in 54.956 sec @1.719 MHz over 94.446 Mnode] -#8 after:
  // 37. g4 Bc3 38. Bf6+ Bxf6 39. Qxf6+ Kg8 40. Qf3 Rc3+ 41. Kd2 Rxf3 42. h4 Rf2+ 43. Kd3 Rfe2 44. Kc3 R8e3#
  //"1Q3b2/5pk1/2p3p1/1p1bN2p/4n2P/8/r5P1/6K1 b - - 0 35"; // Byrne v Fischer 35 -#9
  //[2024-05-12 13-ply in 2:20.7 @1.683 MHz over 236.751 Mnode]
  // 35... Bc5+ 36. Kh2 Nd2 37. Kh1 Ra1+ 38. Kh2 Nf1+ 39. Kh1 Ng3+ 40. Kh2 Bf2 41. Qf8+ Kxf8 42. Nd7+ Ke7 43. Nc5 Rh1#
  //[2022-11-03 14-ply in 3:00.5 over 275.52 Mnode @1.526 MHz]
  // 35... Bc5+ 36. Kh2 Nd2 37. Kh1 Ra1+ 38. Kh2 Nf1+ 39. Kh1 Ng3+ 40. Kh2 Bf2 41. Qf8+ Kxf8 42. Nxg6+ fxg6 43. Kh3 Rh1#
  //"r3r1k1/pp3pbp/1qp3p1/2B5/2BP2b1/Q1n2N2/P4PPP/3R1K1R b - - 0 17"; // Byrne v Fischer 17
  //[14-ply in 8:23 over 713 Mnode @1.418 MHz] eval -1.47
  //"r4rk1/pp2Bpbp/1qp3p1/8/2BPn1b1/Q1P2N2/P4PPP/3RK2R b K - 0 15"; // Byrne v Fischer 15
  //[14-ply in 15:52.5 over 1.353 Gnode @1.42 MHz] eval -1.07
  //"r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13"; // Byrne v Fischer 13
  //"r2q1rk1/pp2ppbp/1np2np1/2Q3B1/3PP1b1/2N2N2/PP3PPP/3RKB1R b K - 0 11"; // Byrne v Fischer 11 >11-ply
  //"8/K1P5/8/3q4/4k3/8/8/8 b - - 0 1";
  //"8/P1K5/8/3q4/4k3/8/8/8 w - - 0 1";
  //"r1b1kb1r/ppp2ppp/2n5/1B1q4/4p3/2P5/PP1PQPPP/RNB1K2R w KQkq - 0 8"; // Fischer v Shipman 1971-08-08
  // moves d2d4
  //"r1b1kb1r/ppp2ppp/2n5/1B1q4/3Pp3/2P5/PP2QPPP/RNB1K2R b KQkq d3 0 8"; // Fischer v Shipman 1971-08-08 EPIllegal
  //"2q2r1k/3brp2/7Q/8/2BPp3/6b1/1B4P1/6K1 b - d3 0 1"; // EPIllegal
  //"k1n1K3/p7/8/B2pP3/8/8/6B1/8 w - d6 0 1"; // ep Mate [3-ply]
  //"7k/b1n3q1/8/2PpP3/3K4/8/8/8 w - d6 0 1"; // EPIllegal, formerly FEN Inconsistent for #6
  //[2023-07-15 10-ply in 5.99 sec @1.368 MHz over 8.197 Mnode]
  // 1. Kc3 Bxc5 2. Kd3 Qg2 3. e6 Nxe6 4. Kc3 Qe2 5. Kb3 Nd4+ 6. Kc3 (6. Ka4 Qa2# (6... Qa6#) (6... Qb5#)) 6... Qc2#
  //"2q1rrk1/5p2/6p1/8/2B5/8/2Q3P1/6K1 w - - 0 1"; // Draw3QxG6 [7-ply] eval 0.0
  //"2r2rk1/8/2b4Q/3pP3/2q5/6B1/5P2/5R1K w - d6 0 1"; // Draw3EPIllegal
  // moves h6g6 g8h8 g6h6 h8g8 h6g6 g8h8 g6h6 h8g8 // Annotates Draw upon 4... Kh8g8=
  //"2r2rk1/8/2b4Q/3pP3/2q5/6B1/5P2/5RK1 w - d6 0 1"; // Draw3EPLegal
  // moves h6g6 g8h8 g6h6 h8g8 h6g6 g8h8 g6h6 h8g8 h6g6 // Annotates Draw upon 5. Qh6g6+=
  // However, the following test case requires one more move to do so:
  // moves h6g6
  // moves g8h8 g6h6 h8g8 h6g6
  // moves g8h8 g6h6 h8g8 h6g6
  // moves g8h8                         // Finally annotates Draw upon 5... Kh8g8=
  // moves h6g6
  // moves g8h8 g6h6 h8g8 h6g6 g8h8 g6h6 h8g8 h6g6 // Annotates Draw upon 5. Qh6g6+=
  //"2r2rk1/8/7Q/3pP3/2q1n3/6B1/5P2/5RK1 w - d6 0 1"; // Draw3NEP
  //"2r2rk1/8/7Q/3pP3/2q1n3/6B1/5P2/5RK1 w - - 0 1"; // Draw3N0
  //"7r/8/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1"; // Perpetua0
  //"8/8/5Q1k/pP2p3/Pp1bP3/2n5/2P3KP/3q4 b - - 0 1"; // Perpetua Draw3
  //[2022-09-18 21-ply in 7:30 @1.312 MHz over 590.566 Mnode] eval 0.0
  // moves h6h5 f6f5 h5h4 f5h3 h4g5 h3f5 g5h4 f5h3 h4g5 h3f5 g5h6 f5f6 h6h7 f6f7 h7h8 f7f8 h8h7 f8f7 h7h8 f7f8 h8h7 f8f7, or
  // moves h6h5 f6f7 h5g4 f7f5 g4h4 f5f6 h4h5 f6f5 h5h6 f5f6 h6h7 f6f7 h7h6 f7f6 {postponing a draw is not favored, once inevitable}
  //"r4rk1/pp2Bpbp/1qp3p1/8/2BPn1b1/Q1P2N2/P4PPP/3RK2R b K - 0 15";
  //"3r2k1/1p3pp1/p1p5/8/1P2r3/P3PqBp/2Q2P1P/R3RK2 b - - 0 29"; // 1964 Bielicki v Smyslov (0-1)
  //[2022-08-28 14-ply in 6:27 @1.518 MHz over 587.6 Mnode] eval -9.20
  //"3r2k1/1p3pp1/p1p5/8/1Pr5/P3PqBp/1Q3P1P/R3RK2 b - - 0 30"; // 1964 Bielicki v Smyslov (0-1) -#9
  //[2022-08-28 16-ply in 3:28 @1.528 MHz over 317.7 Mnode]
  //"6k1/1p3pp1/p1p5/8/1P4r1/P3PqBp/1Q3P1P/3rRK2 w - - 0 32";
  //"3r2k1/1p3pp1/p1p5/8/1P4r1/P3PqBp/1Q3P1P/R3RK2 w - - 0 31"; // 1964 Bielicki v Smyslov (0-1) Final
  //"6k1/1p3pp1/p1p5/8/1P6/P3PqBp/2R2P1P/3rRK2 w - - 0 32"; // 1964 Bielicki v Smyslov (0-1) Line, 15-ply
  //"6k1/1p3pp1/p1p5/4B3/1P3K2/P3P2p/2R2PqP/3r4 b - - 0 37";
  //"6k1/1p3pp1/p1p5/4B3/1P6/P3Pq1p/2R2P1P/3rRK2 b - - 0 32"; // Line, after Be5
  //"r3kbnr/pp2pppp/8/1N6/3n4/5P2/PP3P1P/R1B1KB1R b KQkq - 0 10"; // c3 Sicilan w 4... d5
  //"N5nr/pp1kppbp/6p1/1B6/8/4BP2/PP3P1P/n2K3R b - - 0 15";
  //
  //"rnb3nr/pppp1k1p/3b2q1/7Q/5B2/8/PPP3PP/RN3R1K w - - 0 14"; // Jensen v Urkedal 2013 #10
  //[2023-04-04 16-ply in 35:57 @1.547 MHz over 3.336 Gnode] to find #11
  //[2023-04-04 17-ply in 4:21:30 @1.47 MHz over 23.071 Gnode]
  // 14. Bxd6+ Nf6 15. Rxf6+ Kg7 16. Rxg6+ hxg6 17. Qe5+ Kg8 18. Qe8+ Kg7 19. Qe7+ Kh6 20. Bf4+
  //"rnb4r/pppp1k1p/3B1nq1/7Q/8/8/PPP3PP/RN3R1K w - - 0 15"; // Jensen v Urkedal 2013 #9
  //[13-ply in 15.24 sec @1.797 MHz over 27.4 Mnode]
  //"rnb4r/pppp2k1/3B2p1/7Q/8/8/PPP3PP/RN5K w - - 0 17"; // Jensen v Urkedal 2013 #7
  //[9-ply in 1.3 sec @915 KHz over 1.186 Mnode]
  //"rnb4r/pppp2k1/3B2p1/4Q3/8/8/PPP3PP/RN5K b - - 0 17"; // Jensen v Urkedal 2013 #6
  //[12-ply not 8-ply? in 1.8 sec @1.287 MHz over 2.315 Mnode]
  // 17. Qe5+ Kg8 (17... Kh7 18. Qe7+ Kg8 (18... Kh6 19. Qh4+?! (19. Bf4+!) 19... Kg7 20. Be5+ Kf7 21. Qf6+ Ke8 22. Qxh8+)
  // 19. Be5! Rh7 20. Qe8#) 18. Qe8+ Kg7 19. Qe7+ Kh6 20. Bf4+! g5 21. Qxg5+ Kh7 22. Be5 Rg8 23. Qh5#
  //
  //"2rq1rk1/3nR1pp/p7/1ppp2N1/3P4/1P5P/P1Q2PP1/R5K1 b - - 0 24";   // Bobby Fischer v Laszlo Barczay 1967-10-16 Final
  //"2rq1rk1/4R1pp/p4n2/1ppp2N1/3P4/1P5P/P1Q2PP1/R5K1 w - - 0 25";  // Bobby Fischer v Laszlo Barczay 1967-10-16 Puzzle
  //[2023-08-27 14-ply in 3:56.4 @1.463 MHz over 345.76 Mnode] eval 4.3 after:
  // 25. Rxg7+ Kxg7 26. Ne6+ Kf7 27. Nxd8+ Rfxd8 28. dxc5 Rc7 29. Rc1 Rdc8 30. b4 a5 31. a3 axb4 32. axb4 Rd8 33. f4 d4
  //

  #region Berkeley Games
  //
  //"2r1rnk1/1p4bp/p2pp1p1/q5P1/2PN1P1Q/1P1nB2R/P5BP/5RK1 w - - 0 1"; // Chesney (2435) v Craig Mar (2516), Lera 1989.
  //[14-ply to find 1. f5!]
  //"4n3/6k1/8/4PP1P/5K2/8/8/8 w - - 0 56";   // Hume v Musselman 2009-08-13 Endgame
  //[2025-02-02 13-ply in 2.373 sec @693.26 KHz over 1.6452 Mnode] eval 9.75 after:
  // 56. Kg5 Nc7 57. e6 (57. h6+ Kf7 58. e6+ Kf8 59. Kg6 Nd5 60. f6 Nf4+ 61. Kf5 Ng6 62. Kxg6 Ke8
  // 63. e7 Kd7 64. h7 Kc6 65. h8=Q Kb5 66. Qb8+ Kc5 67. e8=Q Kc4 68. Qe4+ Kc3 69. Qbb4#)
  // 57... Kg8 58. h6 Kh7 59. e7 Ne8 60. f6 Nxf6 61. Kxf6 Kxh6 62. Ke5 Kg5 63. e8=Q
  //"r2r2k1/p4ppp/2q2n2/8/N2n4/P4PP1/QN2P1KP/R3R3 b - - 0 24"; // Wood v Sowell 2014-07-16
  //
  #endregion                            // Berkeley Games

  #region Philidor Studies
  //"4Q3/6rk/5K2/8/8/8/8/8 w - - 0 1"; // Q v R Philidor #10 [15-ply 65.55 sec @1.34 MHz over 87.9 Mnode]
  //"4Q3/6rk/5K2/8/8/8/8/8 b - - 0 1"; // Q v R Philidor #7 [10-ply 1.59 sec @994 KHz over 1.58 Mnode]
  //
  //"3k4/4r3/3K4/3B4/8/8/8/5R2 w - - 0 1";// R v RB Philidor
  // 1. Rf8+ Re8 2. Rf7 Re2 3. Rh7 Re1 4. Rb7 Rc1 5. Bb3 Rc3 6. Be6 Rd3+ 7. Bd5 Rc3 8. Rd7+ Kc8 (8... Ke8? 9. Rg7)
  // 9. Rh7 Kb8 10. Rb7+ Kc8 11. Rb4! Kd8 (11... Rd3 12. Ra4) 12. Bc4 Kc8 13. Be6+ Kd8 14. Rb8+ Rc8 15. Rxc8#
  //
  //"3k4/1R6/3K4/8/8/1Br5/8/8 w - - 0 1"; // R v RB Philidor Line from 5... Rc3 #16
  //[2024-07-04 18-ply in 34:25.67 @1.384 MHz over 2.859 Gnode] #16 after:
  // 1. Be6 Rd3+ 2. Bd5 Rc3 3. Rd7+ Kc8 4. Rg7 Kb8 5. Rb7+ Kc8 6. Rb4 Rd3 7. Ra4 Rxd5+ (7... Rb3? 8. Bxb3) 8. Kxd5 Kb7
  // 9. Kc5 Kc7 10. Ra7+ Kd8 11. Kd5 Kc8 12. Kd5d6 Kc8b8 13. Ra7c7 Kb8a8 14. Kd6c5
  // moves b3e6 c3d3 e6d5 d3c3 b7d7 d8c8 d7f7 c8b8 f7b7 b8c8 b7b4 c3d3 b4a4 d3d5 d6d5
  //
  //[2022-08-25 16-ply in 12:19 @1.354 MHz over 1 Gnode] to find #19
  // 1. Be6 Rd3+ 2. Bd5 Rc3 3. Rd7+ Kc8 4. Rh7 Kb8 5. Rb7+ Kc8 6. Rb4 Rd3 7. Ra4 Rxd5+ 8. Kxd5 Kb7 9. Kc5 Kc7 10. Ra1 Kb7 11. Rg1 Kc7
  //[2022-09-17 19-ply in 24:42 @1.341 MHz over 1.9876 Gnode] to find #18
  // 1. Be6 Rd3+ 2. Bd5 Rc3 3. Rd7+ Kc8 4. Rh7 Kb8 5. Rb7+ Kc8 6. Rb4 Rd3 7. Ra4 Rxd5+ 8. Kxd5 Kc7 9. Rb4 Kd7 10. Rb7+ Kc8 11. Re7 Kd8
  // 12. Rg7 Ke8 13. Rg7g5
  #endregion                            // Philidor Studies

  #region Puzzles
  //
  //"5k2/5p2/4P3/4P3/5K2/8/8/8 w - - 0 1" // White to Play Pawn Endgame [23-ply in 36.082 sec @1.461 MHz over 52.734 Mnode] eval 7.15
  //"8/8/1KP5/3r4/8/8/8/k7 w - - 0 1";  // White to Win
  //"1k1K4/rr6/8/8/8/8/RR6/1Q6 w - - 0 1";      // #2 [4-ply in 0.044 sec @97.286 KHz over 4,295 nodes]
  //
  //"7K/7p/k1P5/8/8/8/8/8 b - - 0 1"; // Réti Draw Prelude [11-ply]
  //"7K/8/k1P5/7p/8/8/8/8 w - - 0 1"; // Réti Draw Endgame [10-ply]
  //"8/8/4Q3/5K2/8/8/2p5/k7 w - - 0 1"; // Q v BP draw
  //"8/8/8/4QK2/8/8/p7/2k5 b - - 0 1"; // Q v RP draw
  //
  // Daniel Naroditsky Endgame Lessons:
  //"8/1pPK3b/8/8/8/5k2/8/8 w - - 0 1"; // White to Draw, using Réti Concept
  //[2023-03-09 14-ply in 4.14 sec @1.029 MHz over 4.262 Mnode] eval -0.85 after:
  // 1. Kc8! b5 2. Kd7 Bf5+ 3. Kd6 b4 4. Ke5! Bh3 5. Kd4 Bc8 6. Kc4 Ke2 7. Kxb4 Kd1 8. Ka3
  //"8/6kp/Q2p4/3P4/8/6qP/3pBrP1/6RK b - - 0 37";
  //[2023-03-23 7-ply in 0.831 sec @564 KHz over 468.7 Knode] eval 0.0 after:
  // 37... Kh6 38. Qb5 d1=Q 39. Bxd1 Qxh3+ 40. gxh3 Rh2+ 41. Kxh2 stalemate
  //
  //"2B5/8/8/4N2P/8/6K1/p7/5k2 w - - 0 1"; // Race to the Bottom
  //[2025-04-13 8-ply in 1.421 sec @380.866 KHz over 541.215 Knode] eval: 6.75 after:
  // 1. Ba6+ Ke1 2. Nf3+ Kd1 3. Nd4 Kc1 4. Bc4 a1=Q 5. Nb3+ Kb1 6. Nxa1 Kxa1
  //"8/8/4p3/B2p4/3P4/8/pk1K4/8 w - - 0 1"; // Process of Elimination
  //[2025-04-11 24-ply] eval 3.25 after:
  // 1. Bc3+ Ka3 2. Kc2 Ka4 3. Kb2 Kb5 4. Kxa2 Ka6 5. Be1 Kb5 6. Kb3 Ka6 7. Kc2 Kb5 8. Kd2 Ka4 9. Ke3 Ka3
  // 10. Bf2 Ka2 11. Kf4 Ka1 12. Ke5 Kb1 13. Kxe6
  // moves a5c3 b2b1 c3a1 b1a1 d2c1
  //
  //"2k5/8/8/7p/8/8/6P1/5K2 w - - 0 1"  // White to Win Endgame
  //[2025-09-30 26-ply in 40:29:39.205 @1.393 MHz over 203.066 Gnode] eval 7.15 after:
  // 1. Kf2 h4 2. Kg1 Kc7 3. Kh2 Kd6 4. Kh3 Ke5 5. Kxh4 Kf5 6. Kh5 Kf6 7. Kg4 Kg6 8. Kg4h4
  //
  //"7K/8/8/8/8/2p4N/8/2k5 w - - 0 1";  // White to Draw
  "3k1b2/7K/4PP2/8/8/8/8/8 w - - 0 1";  // White to Zugzwang
  // 1. Kh8 Ke8 2. Kg8 Bh6 3. e7 Bg5
  // moves h7h8 d8e8 h8g8 f8h6 e6e7 h6g5
  //[2025-07-03 19-ply in 11:00.4 @1.236 MHz over 816.036 Mnode] #16 after:
  // 4. Kg7 Bh4 5. f7+ Kxe7 6. f8=Q+ Ke6 7. Qf7+ Ke5 8. Qh5+ Kd4 9. Qxh4+ Kc3 10. Kf6 Kb2 11. Qf2+ Kb3 12. Ke5 Kc3 13. Kd5 Kd3
  // 14. Qe1 Kc2 15. Kd4 Kb2 16. Qd1
  //"4k1K1/4P3/5P2/6b1/8/8/8/8 w - - 0 4";
  //"8/8/3p4/4p2B/4P3/8/4K1kp/8 w - - 0 1";     // Can White win?
  // 1. Bf3+ Kg1 2. Bh1! Kxh1 3. Kf1 d5 4. exd5 e4 5. d6 e3 6. d7 e2+ 7. Kxe2 Kg1
  // [7... Kg2 8. d8=Q h1=Q 9. Qg5+] 8. d8=Q h1=Q 9. Qd4+ Kh2 10. Qh4+ Kg2 11. Qg4+ Kh2 12. Kf2
  // moves h5f3 g2g1
  //"8/8/3p4/4p3/4P3/5B2/4K2p/6k1 w - - 0 2"    // Can White win? Bishop Sacrifice Line
  //[2024-06-06 17-ply in 1.97 sec @851.135 KHz over 1.6766 Mnode] eval 9.75 after:
  // 2. Bh1! Kxh1 3. Kf1 d5 4. exd5 e4 5. d6 e3 6. d7 e2+ 7. Kxe2 Kg1 8. d8=Q h1=Q 9. Qd4+ Kg2
  // [9... Kh2 10. Qh4+ Kg2 11. Qg4+ Kh2 12. Kf2] 10. Qg4+ Kh2 11. Kf2 Qf3+ 12. Kxf3 Kh1
  //[2024-07-29 26-ply in 21:10 @1.42748 MHz over 1.81266 Gnode] #16 after:
  // 2. Bh1 Kxh1 3. Kf1 d5 4. exd5 e4 5. d6 e3 6. d7 e2+ 7. Kxe2 Kg2 8. d8=Q Kh3 9. Qg5 h1=Q 10. Qh5+ Kg3 11. Qxh1 Kf4 12. Qd5 Kg4
  // 13. Qe5 Kh3 14. Qf4 Kg2 15. Qh4 Kg1 16. Ke2f3
  // moves h5f3 g2g3
  //"8/8/3p4/4p3/4P3/5Bk1/4K2p/8 w - - 0 2"     // Can White win? King Opposition Line
  //[2024-07-29 26-ply in 24:18.7 @1.0683 MHz over 1.558 Gnode] eval 3.25 after:
  // 2. Ke3 Kh3 3. Kf2 Kh4 4. Kg2 Kg5 5. Kxh2 Kf4 6. Kg2 Kg5 7. Kf1 Kf4 8. Ke2 Kg3 9. Bh5 Kg2 10. Kd3 Kg3 11. Bg6 Kh4 12. Bf5 Kg5
  // 13. Kc4 Kf4 14. Kd5 Ke3 [15. Kxd6]
  //"3k3r/6R1/7p/5B2/8/8/8/3K4 w - - 0 1";      // KRB v KRP [2025-02-01 17-ply in 4:28:29.5 @1.0373 MHz over 16.71 Gnode] eval 5.0 after:
  // 1. Rd7+ Ke8 2. Ra7 Rf8 3. Bg6+ Kd8 4. Bf7 Rxf7 5. Rxf7 Kc8 6. Rh7 Kb8 7. Rxh6 Ka7 8. Rh5 Ka6 9. Rd5 Kb6 10. Rf5 Kb6a6
  // moves g7d7 d8e8 d7a7 h8f8 f5g6 e8d8 g6f7
  //"3k1r2/R4B2/7p/8/8/8/8/3K4 b - - 0 4";      // KRB v KRP Line [2025-02-01 12-ply in 25.965 sec @1.272 MHz over 33.034 Mnode] eval 5.0 after:
  // 4... h5 5. Ke1 h4 6. Kf1 h3 7. Kg1 h2+ 8. Kh1 Rxf7 9. Rxf7 Ke8 10. Rf5 Kd7 11. Kxh2
  //"4B2k/6q1/5N2/8/3B4/8/8/1K6 w - - 0 1";     // KBBN v KQ [2025-01-27 7-ply in 0.711 sec @297 KHz over 211,067 nodes] eval 6.75 after:
  // 1. Bc6 Qg6+ 2. Be4 Qg7 3. Ka1 Qh6 4. Ng4+ Qg7 5. Bxg7+ Kh8xg7
  //"2r5/6P1/8/3B2k1/8/6P1/8/K7 w - - 0 1";
  //"k7/pN6/8/3B1r2/5B1K/8/8/8 w - - 0 1";      // Forked Bishops [2024-04-24 9-ply in 1.079 sec @610 KHz over 658.144 Knode] eval 4.75 after:
  // 1. Bg2 Rxf4+ 2. Kg3 Rf8 3. Nc5+ Kb8 4. Nd7+ Kc7 5. Nxf8 a5 6. Ng6 a4 7. Nf4
  //"5q1k/1RR5/8/8/8/8/4K3/8 w - - 0 1";        // KRR v KQ by Henri Rinck 1916
  // 1. Rh7+! Kg8 2. Rhe7 Kh8 3. Rbc7! Qg8 4. Kf1! Qf8+ 5. Rf7 Qg8 6. Kf2 Qb8 7. Rh7+ Kg8 8. Rcg7+ Kf8 9. Rh8+ Kxg7 10. Rxb8+-
  // moves c7h7 h8g8 h7e7 g8h8 [b7c7 f8g8]
  //"8/8/8/2P4B/8/5K2/3k4/5b2 w - - 0 1";       // KBP v KB % NM Nelson Lopez
  //"2b5/2P5/2K5/7B/1k6/8/8/8 w - - 0 7"        // KBP v KB Line % NM Nelson Lopez
  //[2024-01-07 18-ply in 1:27 @1.434 MHz over 124.8 Mnode] eval 9.75 after:
  // 7. Kb6 Bh3 8. Bf3 Bf5 9. Bb7 Bh3 10. Ka7 Kc5 11. Kb8 Bf5 12. Ba6 Bh3 13. Bc8 Bf1 14. Bf5 Ba6 15. Bd3 Bxd3 16. c8=Q+
  //"8/p7/P7/8/8/4k3/4P3/4K3 w - - 0 1";        // White to Play and Win % NM Nelson Lopez
  //[2023-12-19 25-ply in 50:10.243 @1.154 MHz over 3.4743 Gnode] eval 6.95 after:
  // 1. Kf1 Kd4 2. Kf2 Ke4 3. e3 Kf5 4. Kf3 Ke5 5. e4 Kd6 6. Kf4 Ke6 7. e5 Kd7 8. Kf5 Ke7 9. e6 Ke8 10. Ke4 Ke7 11. Ke5 Ke8 12. Ke5d6
  //[2023-12-20 26-ply in 3:03:29.622 @1.1785 MHz over 12.975 Gnode] eval 9.0 after:
  // 1. Kf1 Kd4 2. Kf2 Ke4 3. e3 Kf5 4. Kf3 Ke5 5. e4 Kf6 6. Kf4 Ke6 7. e5 Ke7 8. Kf5 Kd7 9. e6+ Ke7 10. Ke5 Ke8 11. Kd6 Kd8 12. Kd6c6 Kd8e7 13. Kc6b7
  //"8/8/8/8/8/4K2p/4N2k/8 w - - 0 1";    // KN v KP #7
  //[2024-06-18 13-ply in 1.335 sec @655.18 KHz over 874.9 Knode] #7 after:
  // 1. Kf3 Kh1 2. Kf2 Kh2 3. Nc3 Kh1 4. Ne4 Kh2 5. Nd2 Kh1 6. Nf1 h2 7. Ng3#
  //"8/8/8/8/6pN/7p/7p/1KR3nk w - - 0 1"; // #6 1927 by Janis Behting % NM Nelson Lopez
  //[2023-08-29 11-ply in 7.26 sec @1.284 MHz over 9.32 Mnode] #6 after:
  // 1. Rf1 g3 2. Nf5 Kg2 3. Ne3+ Kh1 4. Rc1 g2 5. Nd1 Ne2 6. Nf2#
  //"8/8/8/7n/8/7N/3kp1K1/5n2 w - - 0 1"; // White to Draw 1937 by Alexander Herbstmann and Leonid Kubbel % NM Nelson Lopez
  //[2023-08-28 14-ply in 18.25 sec @1.555 MHz over 28.38 Mnode] eval -1.0 example:
  // 1. Ng1 Ne3+ 2. Kh3 e1=N 3. Nf3+ Nxf3 stalemate
  //
  //"n1QBq1k1/5p1p/5KP1/p7/8/8/8/8 w - - 0 1";  // Justice Delayed #12 [2024-02-18 21-ply in 14:39:23.7 @1.68655 MHz over 88.9887 Gnode]
  // 1. Bc7 Qxc8 2. gxf7+ Kh8 3. Be5 Qc5 4. Bb2 Nc7 5. Ba1 a4 6. Bb2 a3 7. Ba1 a2 8. Bb2 a1=Q 9. Bxa1 Nb5 10. Ke6+ Nc3 11. Bxc3+ Qxc3 12. f8=Q#
  //"n1QBq1k1/5p1p/5KP1/8/8/8/8/8 w - - 0 1";   // Swift Justice #8
  //[2024-06-28 14-ply in 31.895 sec @1.47 MHz over 42.512 Mnode] #8 after:
  // 1. Bc7 Qxc8 2. gxf7+ Kh8 3. Be5 Qc5 4. Ba1 Nc7 5. Bb2 Nb5 6. Ke6+ Nc3 7. Bxc3+ Qxc3 8. f8=Q#
  //[2024-02-17 14-ply in 32.921 sec @1.467 MHz over 48.3 Mnode] #8
  //
  //"5r2/1P5p/2N4K/8/8/5kP1/8/8 w - - 0 1";     // Composed by Sergueï Kaminer
  //[2023-06-06 14-ply in 1:25 @1.151 MHz over 97.9 Mnode]
  //"3q4/p1p5/3r4/2N1np1p/QB5k/3P4/5PKP/8 w - - 0 1"; // Study by Sergueï Kaminer 1926
  //[2023-04-29 11-ply in 36.816 sec @1.543 MHz over 56.813 Mnode] eval 3.7 after:
  // 1. Be1+ Kg5 2. Ne6+ Rxe6 3. Qh4+ Kg6 4. Qxd8 Nf7 5. Qg8+ Kf6 6. Ba5 c6 7. Qh7 Rd6 8. Bc3+
  //[2023-04-30 16-ply in 48:03 @1.498 MHz over 4.32 Gnode] eval 5.9 after:
  // 1. Be1+ Kg5 2. Ne6+ Rxe6 3. Qh4+ Kg6 4. Qxd8 Nf7 5. Qg8+ Kf6 6. Bc3+ Ke7 7. Bb4+ Nd6 8. Qc8 Rg6+ 9. Kf1 Kf6 10. Qxc7 Nf7 11. Qxa7 Ng5
  //
  //"8/3P3k/n2K3p/2p3n1/1b4N1/2p1p1P1/8/3B4 w - - 0 1"; // Mike Anderson's Famous [Gijs van Breukelen] Study
  // See "Solution to a truly remarkable study" by Frederic Friedel, 2018-02-12 https://en.chessbase.com/post/solution-to-a-truly-remarkable-study
  // This study was composed by Gijs van Breukelen and presented to the players at a Super Tournament in Brussels, 1987.  First solved by Mikhail Tal.
  // 1. Nf6+ Kg7 2. Nh5+ Kg6 3. Bc2+!! Kxh5 Here Stockfish 12 shows +3.66 4. d8=Q Kg4
  //[4... Nf7+? 5. Ke6 Nxd8+ 6. Kf5! e2 7. Be4 e1=N 8. Bd5 c2 9. Bc4 c1=N 10. Bb5 Nc6 11. Bxc6 Nc7 12. Ba4 Ne2
  //[12... Nc2 13. Bxc2 Ne2 14. Bd1 -- 15. Bxe2#] 13. Bd1 Nf3 14. Bxe2 -- 15. Bxf3#]
  // 5. Bd1+ Kxg3 6. Qe8 c4+ 7. Kc6 Bc5 8. Qe5+ +-
  //
  // Position prior to 4... Nf7+? after moves g4f6 h7g7 f6h5 g7g6 d1c2 g6h5 d7d8q:
  //"3Q4/8/n2K3p/2p3nk/1b6/2p1p1P1/2B5/8 b - - 0 4";
  // 4... Nf7+? 5. Ke6 Nxd8+ 6. Kf5!
  // Hint position after moves g5f7 d6e6 f7d8 e6f5:
  //"3n4/8/n6p/2p2K1k/1b6/2p1p1P1/2B5/8 b - - 0 1";
  //[2022-11-17 17-ply in 8:26:53.6 @1.392 MHz over 42.34 Gnode] #9 after:
  // 6... e2 7. Be4 e1=N 8. Bd5 c2 9. Bc4 c1=N 10. Bb5 Nc6 11. Bxc6 Nc7 12. Ba4 Ne2 13. Bd1 Nf3 14. Bxe2 Nb5 15. Bxf3#
  //
  //"3Q4/8/7p/5p1P/2p2P2/3b4/K1pk4/8 w - - 0 1"; // Anand v Carlsen Stalemate Global Chess League 2023-06-28
  //[6-ply over 25.448 Knode] eval 0.0 after:
  // 1. Qb6 c1=Q 2. Qe3+ Kd1 3. Qe2+ Bxe2 stalemate
  // moves d8d4
  //"8/8/7p/5p1P/2pQ1P2/3b4/K1pk4/8 b - - 0 1"; // Anand v Carlsen Remarkable Endgame Global Chess League 2023-06-28
  //[2023-06-28 16-ply in 4:01.28 @1.3333 MHz over 312.15 Mnode] eval -4.5 after:
  // 1... c1=N+ 2. Ka3 c3 3. Qb4 Ne2 4. Qa5 Be4 5. Ka2 Ke1 6. Ka3 Kf2 7. Qb6+ Ke1 8. Qe3 c3c2
  //
  //"1k6/ppp2Rp1/n4b1p/8/5P2/qPQ5/P1P2P2/1K6 w - - 0 1"; // Interference Puzzle
  //"1B6/8/7P/4p3/3b3k/8/8/2K5 w - - 0 1"; // Promotion Puzzle
  //[2023-01-07 16-ply in 53.434 sec @1.23 MHz over 65.72 Mnode] eval 5.05
  //"5k2/8/3P1K2/7b/2B5/8/8/8 w - - 0 1"; // Underpromotion #10 [12-ply in ~2.5 sec over 3.15 Mnode]
  // 1. d7 Be8 2. d8=B Bf7 3. Be7+ Kg8 4. Bxf7+ Kh7 5. Bh6+ Kh8 6. Bb1 Kg8 7. Kg6 Kh8 8. Bf5
  //
  //"3nk3/p2R4/3P1Kp1/8/5p2/3bp3/3p4/8 w - - 0 1"; // Long Puzzle [2023-01-16 17-ply in 5:18 @1.272 MHz over 402.36 Mnode] eval 0.00 after:
  // 1. Re7+ Kf8 2. d7 Kg8 3. Re8+ Kh7 4. Rxd8 d1=Q 5. Rh8+ Kxh8 6. d8=Q+ Kh7 7. Qc7+ Kh6 8. Qxf4+ Kh7 9. Qc7+ Kh6 10. Qf4+@ Kh7@
  // 11. Qc7 +@ Kh6@ 12. Qf4+=
  // moves d7e7 e8f8 d6d7 f8g8 e7e8 g8h7 e8d8 d2d1Q d8h8 h7h8 d7d8Q h8h7 d8c7 h7h6 c7f4 h6h7 f4h2 h7g8
  //"6k1/p7/5Kp1/8/8/3bp3/7Q/3q4 w - - 0 10"; // Long Puzzle Line [2023-01-16 23-ply in 4:12:41 @1.407 MHz over 21.34 Gnode] eval 9.13 after:
  // 10. Qb8+ Kh7 11. Qxa7+ Kh6 12. Qxe3+ Kh7 13. Qh3+ Kg8 14. Qe6+ Kh7 15. Qd7+ Kh6 16. Qh3+ Qh5 17. Qe3+ g5 18. Qxd3 Qe8
  // 19. Qh3+ Qh5 20. Qf5 g4 21. Qf4+ Kh7 22. Qc7+ Qf7+ 23. Qxf7+
  //[2023-01-16 24-ply in 40:28:21 @1.247 MHz over 181.7548 Gnode] #17 after:
  // 10. Qb8+ Kh7 11. Qxa7+ Kh6 12. Qxe3+ Kh7 13. Qa7+ Kh6 14. Qe3+@ Kh7@ 15. Qh3+ Kg8 16. Qc8+ Kh7 17. Qd7+ Kh6
  //"6k1/3b3p/2pP4/p5q1/4QN2/6P1/PP5P/1K1R4 b - - 0 1"; // Puzzle [White to win after: ...Bf5.  Hint: The key move and the "follow up" transpose.]
  //
  //"8/8/3P4/8/1pP5/pP1K4/1k6/8 w - - 0 1"; // 2023 Airthings Masters Puzzle
  //[21-ply in 20:56:31 @1.065 MHz over 80.26 Gnode] eval 4.2 after:
  // 1. d7 a2 2. d8=B Kxb3 3. Bf6 Ka3 4. Ba1 Ka4 5. Kc2 b3+ 6. Kc3 Ka5 7. Kxb3 Ka6 8. Be5 Ka5 9. Bc3+ Ka6 10. Kxa2 Kb6 11. Bd4+ Ka5 12. c5 Ka5b5 13. Ka2b2
  //"8/8/8/7n/8/4n3/3kp1K1/6N1 w - - 0 1";      // 3 Knights Stalemate [2023-06-10 17-ply in 50.33 @1.6146 MHz over 81.265 Mnode]
  // 1. Kh3 e1=Q (1... Nf4+ 2. Kh2 Ng4+ 3. Kh1 Nf2+ 4. Kh2 e1=N 5. Nf3+ Nxf3+ 6. Kg3 Ke3 stalemate)
  // 2. Nf3+ Kc1 3. Nxe1 Nd1 4. Nc2 Nb2 5. Na1 Ng3 6. Nc2 Nd1 7. Kg2 Nb2 8. Kxg3 Nd3 9. Na1
  //
  //"5rk1/1rP3pp/p4n2/3Pp3/1P2Pq2/2Q4P/P5P1/R3R1K1 b - - 0 1"; // Unzicker v Averbach [10-ply in 16.8 sec @1.474 MHz over 24.77 Mnode]
  //"7k/R7/P7/5r1P/2K5/8/8/8 b - - 1 50";   // Vancura Position from Caruana v Carlsen Freestyle R1 2022-11-22
  //"8/8/8/8/8/2K4B/5k1P/8 w - - 0 1";      // J. Vancura 1922, Ceske Slovo (care of Frederic Friedel) [11-ply in 0.867 sec to find 1. Bd7!]
  //[2022-08-20 5-ply in 332 ms @486 KHz over 162,881 nodes] eval -4.0
  // 1. Bd7 Kf3 2. h4 Ke4 3. h5 Ke5 4. h6 Kf6 5. Be8 Ke7 6. h7 Kxe8 7. h8=Q+
  //"r2qkbnr/ppp2ppp/2np4/4p2b/2B1P3/2N2N1P/PPPP1PP1/R1BQK2R w KQkq - 0 6"; // Légal Trap
  //"8/K6N/8/2N5/1n6/6Q1/6pn/7k w - - 0 1"; // Chekhov's Gun [White to Win in 4, Zwischenzug followed by Zugzwang] 8-ply in 0.68 sec
  //"5rk1/pp4pp/4p3/2R3Q1/3n4/2q4r/P1P2PPP/5RK1 b - - 0 1"; // Stefan Levitsky v Frank Marshall, Breslau 1912, "The Gold Coin Game"
  //"5k1r/p1Br1pb1/6q1/1pp2Npp/3n4/P2Q3P/1P3PP1/3RR2K w - - 0 29"; // 2023-08-03 Carlsen v Pantsulaia #5
  // Mobility Disabled
  //[2023-08-06 6-ply in 0.56 sec @536.1 KHz over 300 Knode]
  // Mobility Weight = 100 cp
  //[2023-08-03 6-ply in 0.863 sec @648.3 KHz over 559.7 Knode]
  // Mobility Weight = 200 cp
  //[2023-08-06 6-ply in 1.066 sec @780 KHz over 831.1 Knode]
  //"rnb3kr/ppp2ppp/1b6/3q4/3pN3/Q4N2/PPP2KPP/R1B1R3 w - - 0 2"; // Confusion #4
  //[2023-06-22 5-ply in 0.977 sec @637.14 KHz over 622.49 Knode]
  //"8/8/8/8/4k3/8/8/2BQKB2 w - - 0 1"; // Pal Benko for Bobby Fischer #3
  //[6-ply in 0.5 sec over 260,802 nodes]
  //"5K1k/5B2/4NN2/8/8/8/5p2/5r2 w - - 0 1"; // A. Grunenwald 1960 #3 [7-ply 0.31 sec]
  //"7k/4K1pp/7N/8/8/8/8/B7 w - - 0 1"; // Puzzle #3 [7-ply in 134 ms @216.4 KHz over 29 Knode]
  //"2R5/kb6/1N2n2p/r4pp1/P7/3P4/1R6/6K1 w - - 0 43"; // Nodirbek Abdusattorov v Vasif Durarbayli 2021-07-24 #3 4-ply
  //"r1bq1r2/pp2n1p1/4p1k1/3pPpN1/1b1n2QP/2N5/PP3PP1/R1B1K2R w KQ f6 0 13"; // EP #3 4-ply
  // moves h4h5 g6h6 g5e6 g7g5
  //"8/8/8/2k5/2P5/2N5/2N3Q1/3K4 w - - 0 1";    // Mrs. W.J. Baird (1859-1924) #3
  //"4br1b/2PpBkPR/3p3P/3P1N2/8/8/8/6K1 w - - 0 1"; // Wolfgang Pauly, Deutsche Schachzeitung 1901 #2 [4-ply]
  // 1. Ng3 Rg8 (1... Kg6 2. gxf8=N#) (1... Kg8 2. gxh8=R#) 2. gxh8=N#
  //"8/1N2N3/2r5/3qp2R/QP2kp1K/5R2/6B1/6B1 w - - 0 1"; // Meltwater Champions Chess Tour Finals 2022 at the SF Ferry Building #2
  //[4-ply in 122 msec over 25.1 Knode] #2 after 30 lines:
  // 1. Qa8 Qd4 (1... otherwise either 2. Nd6#; or 2. Nc5#) 2. Re3#
  //"K4BB1/1Q6/5p2/8/2R2r1r/N2N2q1/kp1p1p1p/b7 w - - 0 1"; // Bristol Times & Mirror 1927 #2
  //[4-ply in 170 ms over 37.8 Knode] #2 after 39 lines:
  // 1. Qf3 Kb3 (1... Qxf3 2. Re4#; 1... Rxf3 2. Rg4#; 1... Rfg4 2. Rxg4#; 1. b1b8 2. Rc2#; 1... otherwise 2. Nb4#) 2. Nc1#
  //"k1b5/pppN4/1R6/8/Q6K/8/8/8 w - - 0 1";     // #2 [4-ply 3,541 node]
  //"kbK5/pp6/1P6/8/8/8/8/R7 w - - 0 1";        // Paul Morphy, age 10 #2 [4-ply in 31 ms over 677 nodes]
  //"1Bb3BN/R2Pk2r/1Q5B/4q2R/2bN4/4Q1BK/1p6/1bq1R1rb w - - 0 1"; // #1 [2-ply 979 nodes]
  //"3rkb1r/ppp2ppp/8/5q2/2b1N3/8/PPP2PPP/R1B1R1K1 w k - 0 13"; // Scotch Gambit #1 [2-ply 179 nodes]
  //
  #endregion                            // Puzzles
  #endregion                            // FEN Constants

  #region EPD Constants
  private const String sDefaultEPD =
  //
  // Bratko-Kopec Test Suite: 24 positions
  //
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
  @"r1bq1r1k/p1pnbpp1/1p2p3/6p1/3PB3/5N2/PPPQ1PPP/2KR3R w - - bm g4; id ""arasan18.1""; c0 ""J. Polgar-Berkes, Budapest Hunguest Hotels 2003"";";
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
  #endregion                            // EPD Constants

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

  protected GameState State { get; init; }

  //[UCI]Diagnostic Info Enabled
  public static Boolean IsDebug { get; set; }
  public Boolean IsVerbose { get; init; }

  //[UCI]Registered Engine
  public static Boolean IsRegistered { get; set; }
  protected static Boolean IsRegistrationChecked { get; set; }

  protected static String? RegistrationCode { get; set; }
  protected static String? RegistrationName { get; set; }

  internal static String? DefaultEPD { get; set; }
  internal static String? DefaultFEN { get; set; }
  #endregion
}
