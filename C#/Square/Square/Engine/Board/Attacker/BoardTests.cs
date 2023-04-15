//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split UnitTests into their own file
//
// Conditionals:
//
//#define Magic
#define TestRotation
//#define TestDiagIndexers
//#define TestWhiteSquares
#define TestRankPiece
#define TestOrthAttacks
#define TestDiagAttacks
#define TestKingAttacks
#define TestKnightAttacks

/*
 Rank
 8 A8 B8 C8 D8 E8 F8 G8 H8
 7 A7 B7 C7 D7 E7 F7 G7 H7
 6 A6 B6 C6 D6 E6 F6 G6 H6
 5 A5 B5 C5 D5 E5 F5 G5 H5
 4 A4 B4 C4 D4 E4 F4 G4 H4
 3 A3 B3 C3 D3 E3 F3 G3 H3
 2 A2 B2 C2 D2 E2 F2 G2 H2
 1 A1 B1 C1 D1 E1 F1 G1 H1
   a  b  c  d  e  f  g  h

 File
 a A1 A2 A3 A4 A5 A6 A7 A8
 b B1 B2 B3 B4 B5 B6 B7 B8
 c C1 C2 C3 C4 C5 C6 C7 C8
 d D1 D2 D3 D4 D5 D6 D7 D8
 e E1 E2 E3 E4 E5 E6 E7 E8
 f F1 F2 F3 F4 F5 F6 F7 F8
 g G1 G2 G3 G4 G5 G6 G7 G8
 h H1 H2 H3 H4 H5 H6 H7 H8
   1  2  3  4  5  6  7  8

 A1H8
               8
             7  A8
           6  A7  B8
         5  A6  B7  C8
       4  A5  B6  C7  D8
     3  A4  B5  C6  D7  E8
   2  A3  B4  C5  D6  E7  F8
 1  A2  B3  C4  D5  E6  F7  G8
  A1  B2  C3  D4  E5  F6  G7  H8
 a  B1  C2  D3  E4  F5  G6  H7
   b  C1  D2  E3  F4  G5  H6
     c  D1  E2  F3  G4  H5
       d  E1  F2  G3  H4
         e  F1  G2  H3
           f  G1  H2
             g  H1
               h
 A8H1
               8
             7  H8
           6  H7  G8
         5  H6  G7  F8
       4  H5  G6  F7  E8
     3  H4  G5  F6  E7  D8
   2  H3  G4  F5  E6  D7  C8
 1  H2  G3  F4  E5  D6  C7  B8
  H1  G2  F3  E4  D5  C6  B7  A8
 h  G1  F2  E3  D4  C5  B6  A7
   g  F1  E2  D3  C4  B5  A6
     f  E1  D2  C3  B4  A5
       e  D1  C2  B3  A4
         d  C1  B2  A3
           c  B1  A2
             b  A1
               a
 */
namespace Engine;

using static Logging.Logger;

partial class Board {
  #region Unit Test
  protected void testPawnAttacks() {
    foreach (var side in Side) {
      LogLine($"{side.Parameter.SideName} Pieces:\n");
      WriteOrth(side.Piece);
      LogLine();

      LogLine($"{side.Parameter.SideName} Pawn Attacks:\n");
      WriteOrth(side.PawnA1H8Atx | side.PawnA8H1Atx);
      LogLine();
    }
  }

  internal static void testOffsets() {
    printSquares("RankOffset", RankOffset);
#if TestRotation && !Magic
    printSquares("OffsetOrth", OffsetOrth);
    printSquares("OffsetA1H8", OffsetA1H8);
    printSquares("OffsetA8H1", OffsetA8H1);
#endif
  }

  protected void testRotations() {
#if TestRotation && !Magic
    writeOrthRotations("BitRank", BitRank);
    writeOrthRotations("BitFile", BitFile);
    writeDiagRotations("BitA1H8", BitA1H8);
    writeDiagRotations("BitA8H1", BitA1H8);
#endif
  }

  protected void testPieceMasks() {
    var bFlip = State.IsFlip;
    testOrth("RankPiece", RankPiece, false, bFlip);
#if TestRotation && !Magic
    testOrth("FilePiece", FilePiece, true, bFlip);
    testDiag("A1H8Piece", A1H8Piece, false);
    testDiag("A8H1Piece", A8H1Piece, true);
#endif
  }

  protected void testAtxMasks(Sq sq) {
    var n = (Int32)sq;
#if TestDiagIndexers
    testDiagIndexes("A1H8Diag", diagA1H8);
    testDiagIndexes("A8H1Diag", diagA8H1);
#endif
#if TestWhiteSquares
    LogLine("WhiteSquare\n");
    WriteOrth(WhiteSquare);
    LogLine();
#endif
#if TestRankPiece
    LogLine("RankPiece\n");
    WriteOrth(RankPiece);
    LogLine();
#endif
#if TestOrthAttacks
    LogLine($"RayOrth({sq})\n");
    WriteOrth(RayOrth(n));
#endif
#if TestDiagAttacks
    LogLine($"RayDiag({sq})\n");
    WriteOrth(RayDiag(n));
#endif
#if TestKingAttacks
    LogLine("AtxKing\n");
    WriteOrth(AtxKing[n]);
#endif
#if TestKnightAttacks
    LogLine("AtxKnight\n");
    WriteOrth(AtxKnight[n]);
#endif
  }
  #endregion
}
