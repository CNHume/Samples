//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2013-03-16 CNHume]Split Start into this file
//
// Conditionals:
//
#define HashCastlingRights
#define TestFEN
//#define TestBinomials
//#define Flip960

namespace Engine {
  using Command;

  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  using Test;

  using static Command.Parser;
  using static Logging.Logger;
  using static System.String;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    #region Constants
    protected const String sOrthodoxStartEPD = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - hmvc 0; fmvn 1;";
    protected const String sOrthodoxStartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    #endregion

    #region Serialization Methods
    private Tabiya? findTabiya() {
      // Match on the Position Prefix, ignoring variability in the Half Move Clock and Full Move Number values:
      var sPrefix = ToString(PositionType.Prefix);
      return TestPositions
        .FirstOrDefault(tabiya => tabiya.FEN is not null && tabiya.FEN.StartsWith(sPrefix));
    }

    protected PerfCase[] getTestCases() {
      var tabiyaFound = findTabiya();
      if (tabiyaFound is null)
        throw new PerftException("Test Position Not Found");

      var testCases = tabiyaFound.PerfCases;
      if (testCases is null)
        throw new PerftException("No Perft Cases Found");

      return testCases;
    }

    protected String? getName(String? sId = default) {
      var tabiyaFound = findTabiya();
      return tabiyaFound is null ?
        sId : tabiyaFound.Name;
    }

    private void setName(String? sId = default) {
      Name = getName(sId);
    }

    private void setNameIfLegal(String? sId = default) {
      //[Note]Calling IsLegal() affects FlagsTurn and FlagsDraw
      if (!IsLegal())
        LogInfo(Level.error, "Illegal Setup");
      setName(sId);
    }

    public void ParseEPD(String sPrefix, Dictionary<String, List<String>?> operations) {
      var scanner = new Scanner(sPrefix);
      try {
        var sHalfMoveCount = GetSingleValue(operations, "hmvc", "0");
        var sFullMoveNumber = GetSingleValue(operations, "fmvn", "0");
        var bWTM = ParsePosition(scanner, out String sEnPassant);
        Init(bWTM, sEnPassant, sHalfMoveCount, sFullMoveNumber, operations);
      }
      catch (PositionException ex) {
        LogInfo(Level.error, ex.Message);
      }
      finally {
        scanner.Close();
      }

      var sValue = GetSingleValue(operations, "id");
      var sId = IsNullOrEmpty(sValue) ?
        sValue : VerbatimLiteralToString(sValue);

      setNameIfLegal(sId);
    }

    public void ParseFEN(String sPrefix, String sHalfMoveClock, String sFullMoveNumber) {
      var scanner = new Scanner(sPrefix);
      try {
        var bWTM = ParsePosition(scanner, out String sEnPassant);
        Init(bWTM, sEnPassant, sHalfMoveClock, sFullMoveNumber);
      }
      catch (PositionException ex) {
        LogInfo(Level.error, ex.Message);
      }
      finally {
        scanner.Close();
      }

      setNameIfLegal();
    }

    public void SetEPD(String? sEPD = null) {
      if (IsNullOrEmpty(sEPD)) sEPD = sOrthodoxStartEPD;

      using (var parser = new Parser(sEPD)) {
        parser.SetupToken.Expect();
        var operations = parser.ParseOperations();
        ParseEPD(parser.SetupToken.Value, operations);
      }

      //verifyEPD(sEPD);
    }

    public void SetFischerRandom(UInt16 wChess960) {
      //
      // Chess960, a.k.a. Fischer Random Chess, is a Chess Variant invented and advocated by Bobby Fischer.
      // The Pawns are set up along the 2nd and 7th ranks in the same maner as they are in traditional, or
      // Orthodox Chess.
      //
      // The Pieces are set up randomly, subject to the following pair of constraints:
      //
      // 1) The King and Rooks are placed randomly; but the Rooks must appear on opposite sides of the King.
      // 2) There must be both one light-squared and one dark-squared Bishop.
      //
      //[Note]The initial Piece placements for both sides should be mirror images of each other, just as
      // they are in Orthodox Chess.  So, each side will have the same piece standing on a file.
      //
      // To see that there are a total of 960 permutations for the Pieces, first note that there are four
      // squares for each of the two Bishops, for a total of 16 possible arrangements.  Then six possible
      // squares remain for the Queen.
      //
      // This leaves five possible squares for the two Knights.  Since Knights are not distinguished from
      // each other, there are 5 choose 2 = C(5, 2) = 10 possible combinations for their placement.
      //
      // Three empty squares then remain on which to place the King and the two Rooks, without choice.
      // Thus, there are 4 * 4 * 6 * 10 = 960 permutations.
      //
      // The 960 permutations occur in pairs, whose elements are mirror images of each other.  In one case,
      // the color of the squares on which the Queens stand will match the color of the Queens; and in the
      // other case the color of the squares on which the Queens stand will be opposite that of the Queens.
      //
      // We might refer to the case where "Queens get their color" as the normal representation and to its
      // reflection as the alternative representation for each of the 480 pairs.
      //
#if DEBUG
      LogLine($"SetFischerRandom({wChess960})");
#endif
      var nBishopDark = wChess960 % 4;
      wChess960 /= 4;
      var nBishopLite = wChess960 % 4;
      wChess960 /= 4;
      var nKnights = wChess960 % 10;
      wChess960 /= 10;
      var nQueen = wChess960 % 3;
      wChess960 /= 3;
      var bFlip = IsOdd(wChess960);

      setup960(bFlip, nQueen, nKnights, nBishopDark, nBishopLite);
    }

    //
    // To improve on the brute force approach of combination encoding below,
    // consider "A maximally-dense encoding for n-choose-k" by Malcolm Rowe.
    //
    // See: https://www.farside.org.uk/201311/encoding_n_choose_k
    //
    private static void choose2(Int32 N, Int32 nChoice, out Int32 n1, out Int32 n2) {
      n2 = n1 = 0;
      for (var index = 0; n1 < N - 1; n1++)
        for (n2 = n1 + 1; n2 < N; n2++, index++) {
          if (index == nChoice) return;
        }
    }

    /// <summary>N choose k</summary>
    /// <remarks>The number of ways k unordered items can be chosen from a set of N elements without replacement.</remarks>
    /// <param name="N">Magnitude of the set of elements</param>
    /// <param name="k">Number of items to be chosen</param>
    public static UInt64 Binomial(UInt32 N, UInt32 k) {
      if (N < k)
        throw new ArgumentException($"N = {N} < k = {k}", nameof(k));

      if (k > N - k)
        k = N - k;

      var C = 1UL;
      for (var i = 0UL; i < k; i++) {
        C *= (N - i);
        C /= (i + 1);
      }

      return C;
    }

    public static Boolean BinomialCase(UInt32 N, UInt32 k, UInt64 qAssert) {
      var C = Binomial(N, k);
      var bEqual = C == qAssert;
      Debug.Assert(bEqual, $"Binom({N}, {k}) = {C} != {qAssert}");
      return bEqual;
    }

    [Conditional("TestBinomials")]
    public static void TestBinomials() {
      BinomialCase(0, 0, 1);
      BinomialCase(1, 0, 1);
      BinomialCase(1, 1, 1);
      BinomialCase(2, 1, 2);
      BinomialCase(4, 2, 6);
      BinomialCase(5, 2, 10);
      BinomialCase(6, 2, 15);
      BinomialCase(6, 3, 20);
      BinomialCase(7, 2, 21);
      BinomialCase(7, 3, 35);
      BinomialCase(8, 3, 56);
      BinomialCase(8, 4, 70);
    }

    /// <summary>Swap two entities of type T.</summary>
    protected static void Swap<T>(ref T e1, ref T e2) {
      (e1, e2) = (e2, e1);
    }

    //
    // Return the last of nEmpty squares:
    //
    private Int32 findEmpty(Boolean bFlip = false, Int32 nEmpty = 0) {
      var qpPiece = Side[White].Piece;
      var qp = bFlip ? BIT7 : BIT0;

      for (var n = 0; n <= nEmpty; n++) {
        if ((qp & qpPiece) != 0)
          nEmpty++;                     // Extend the search

        if (bFlip)
          qp >>= 1;
        else
          qp <<= 1;
      }

      return bFlip ? nFiles - 1 - nEmpty : nEmpty;
    }

    //
    // The following variation on findEmpty() limits the choice to squares of the same color:
    //
    private Int32 findEmpty2(Boolean bFlip = false, Int32 nEmpty = 0, Boolean bOdd = true) {
      const Int32 nBy = 2;
      var qpPiece = Side[White].Piece;
      var qp = bFlip ? BIT7 : BIT0;
      var n = 0;

      if (bOdd) {
        nEmpty++;
        n++;

        if (bFlip)
          qp >>= 1;
        else
          qp <<= 1;
      }

      for (; n <= nEmpty; n += nBy) {
        if ((qp & qpPiece) != 0)
          nEmpty += nBy;                // Extend the search

        if (bFlip)
          qp >>= nBy;
        else
          qp <<= nBy;
      }

      return bFlip ? nFiles - 1 - nEmpty : nEmpty;
    }

    private void setup960(Boolean bFlip, Int32 nQueen, Int32 nKnights, Int32 nBishopDark, Int32 nBishopLite) {
#if DEBUG
      LogLine($"setup960(Queen: {nQueen}, KnightPair: {nKnights}, BishopLite: {nBishopLite}, BishopDark: {nBishopDark})");
#endif
      TestBinomials();                  //[Conditional]

      Clear();                          // Clear Board

      #region Setup the Dark and Lite-Squared Bishops
      if (bFlip) {
        nBishopDark = nPerNibble - 1 - nBishopDark;
        nBishopLite = nPerNibble - 1 - nBishopLite;
        Swap(ref nBishopDark, ref nBishopLite);
      }

      var nBishopDark2 = 2 * nBishopDark;
      var nBishopLite2 = 2 * nBishopLite + 1;
#if DEBUG
      hasColor(nBishopDark2, DarkSquare, "Dark Bishop");
      hasColor(nBishopLite2, LiteSquare, "Lite Bishop");
#endif
      setupPiece(vB6, nBishopDark2);
      setupPiece(vB6, nBishopLite2);
      #endregion

      #region Setup the Queen
      var nQueen2 = 2 * nQueen;
      var nQueenSquare = findEmpty2(bFlip, nQueen2);
#if DEBUG
      hasColor(nQueenSquare, LiteSquare, "Queen", "her");
#endif
      setupPiece(vQ6, nQueenSquare);
      #endregion

      #region Setup the Knights
      const Int32 nKnightEmpty = 5;
      choose2(nKnightEmpty, nKnights, out Int32 nKnight1, out Int32 nKnight2);
#if DEBUG
      LogLine($"choose2(N1: {nKnight1}, N2: {nKnight2})");
#endif
      //
      // Find square for the wider choice first:
      //
      if (bFlip) {
        nKnight1 = 3 - nKnight1;
        nKnight2 = 4 - nKnight2;
      }

      var nKnight2Square = findEmpty(false, nKnight2);
#if DEBUG
      var sqKnight2 = (sq)nKnight2Square;
#endif
      //
      // Occupy square before making the second choice:
      //
      setupPiece(vN6, nKnight2Square);

      var nKnight1Square = findEmpty(false, nKnight1);
#if DEBUG
      var sqKnight1 = (sq)nKnight1Square;
#endif
      setupPiece(vN6, nKnight1Square);
      #endregion

      #region Setup Queenside Rook, King, and Kingside Rook
      var nRookOOO = findEmpty(bFlip);
      setupPiece(vR6, nRookOOO);

      setupPiece(vK6, findEmpty(bFlip));

      var nRookOO = findEmpty(bFlip);
      setupPiece(vR6, nRookOO);
      #endregion

      #region Setup the Pawns
      setupPawns();
      #endregion
#if DEBUG
#if Flip960
      var setup = PositionSetup(bFlip);
      var sense = bFlip ? "Flipped" : "Normal";
      LogLine($"{sense} Setup = {setup}");
#else
      var setup = PositionSetup();
      LogLine($"Setup = {setup}");
#endif
#endif
      #region Grant Castling Rights and Init the Position
      //
      // With the Pieces and Pawns in place, the position can now be initialized:
      //
      const Boolean bChess960 = true, bWhiteMovesFirst = true;
      InitCastleRules(nRookOOO, nRookOO, bChess960);

      var sEnPassant = Empty;
      var sHalfMoveCount = "0";
      var sFullMoveNumber = "1";
      Init(bWhiteMovesFirst, sEnPassant, sHalfMoveCount, sFullMoveNumber);
      #endregion
    }

    private void setupPawns() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      var vPiece = vP6;
      var nBlack = (Int32)sq.a7;
      var nWhite = (Int32)sq.a2;
      for (var nFile = 0; nFile < nFiles; nFile++, nBlack++, nWhite++) {
        blackSide.PlacePiece(vPiece, nBlack);
        whiteSide.PlacePiece(vPiece, nWhite);
      }
    }

    private void setupPiece(Byte vPiece, Int32 nWhite) {
      if (nWhite < nFiles) {            //[Safe]
        var blackSide = Side[Black];
        var whiteSide = Side[White];

        var nBlack = nRankLast + nWhite;
        blackSide.PlacePiece(vPiece, nBlack);
        whiteSide.PlacePiece(vPiece, nWhite);
      }
      else {
        Trace.Assert(nWhite < nFiles, $"nWhite = {nWhite} >= nFiles {nFiles}");
      }
    }

    private void InitCastleRules(Int32 nRookFromOOO, Int32 nRookFromOO, Boolean bChess960) {
      State.IsChess960 = bChess960;
      foreach (var side in Side) {
        side.InitCanCastle();

        //
        // Validation normally provided by parseCastleRights()
        //
        side.GrantCastling(nRookFromOOO, bChess960);
        side.GrantCastling(nRookFromOO, bChess960);

        side.HashCastlingRights();
      }
    }

    #region Chess960 Setup Diagnostics
    private static void hasColor(Int32 n, Plane qpColor, String sName, String sPossessive = "its") {
      var qp = BIT0 << n;
      var sHas = (qp & qpColor) != 0 ? "has" : "does not have";
      LogLine($"The {sName} at {(sq)n} {sHas} {sPossessive} color.");
    }
    #endregion

    public void SetFEN(String? sFEN = null) {
      if (IsNullOrEmpty(sFEN)) sFEN = sOrthodoxStartFEN;

      using (var parser = new Parser(sFEN)) {
        parser.SetupToken.Expect();
        var sHalfMoveClock = parser.ParseCount("0");
        var sFullMoveNumber = parser.ParseCount("1");
        ParseFEN(parser.SetupToken.Value, sHalfMoveClock, sFullMoveNumber);
      }

      verifyFEN(sFEN);
    }

    [Conditional("TestFEN")]
    protected void verifyFEN(String sFEN) {
      // Output FEN:
      var sFEN2 = ToString(PositionType.FEN);
      var nFENLength = sFEN.Length;
      // Input FEN abbreviations should match Output FEN Prefix:
      var sFEN2Prefix = nFENLength < sFEN2.Length ?
        sFEN2.Substring(0, nFENLength) : sFEN2;

      if (sFEN != sFEN2Prefix)
        LogInfo(Level.warn, "Input FEN inconsistent with Output FEN");
    }
    #endregion
  }
}
