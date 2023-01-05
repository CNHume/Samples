//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-03-16 CNHume]Split Start into this file
//
// Conditionals:
//
#define HashCastlingRights
#define TestFEN
//#define TestBinomials
#define ReflectSetup960

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  using Command;

  using Exceptions;

  using Test;

  using static System.String;
  using static Command.Parser;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    #region Constants
    private const String sOrthodoxSetup = "RNBQKBNR";
    private const String sOrthodoxStartEPD = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - hmvc 0; fmvn 1;";
    private const String sOrthodoxStartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    #endregion

    #region Serialization Methods
    private Tabiya? findTabiya() {
      // Match on the Position Prefix, ignoring variability in the Half Move Clock and Full Move Number values:
      var sPrefix = ToString(PositionType.Prefix);
      return TestPositions
        .FirstOrDefault(tabiya => tabiya.FEN != null && tabiya.FEN.StartsWith(sPrefix));
    }

    private PerfCase[] getTestCases() {
      var tabiyaFound = findTabiya();
      if (tabiyaFound == null)
        throw new PerftException("Test Position Not Found");

      var testCases = tabiyaFound.PerfCases;
      if (testCases == null)
        throw new PerftException("No Perft Cases Found");

      return testCases;
    }

    private String? getName(String? sId = default) {
      var tabiyaFound = findTabiya();
      return tabiyaFound == null ?
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

    public void ParseEPD(String sPrefix, Dictionary<String, List<String>?>? operations) {
      using var scanner = new Scanner(sPrefix);
      var rookFromSquares = new List<Int32>(4);
      var bWTM = bWhiteMovesFirst;
      String? sEnPassant = default;
      var sHalfMoveCount = GetSingleValue(operations, "hmvc", "0");
      var sFullMoveNumber = GetSingleValue(operations, "fmvn", "0");
      try {
        bWTM = ParsePosition(scanner, out sEnPassant, rookFromSquares);
      }
      catch (PositionException ex) {
        LogInfo(Level.error, ex.Message);
      }
      finally {
        InitRoot(bWTM, sEnPassant, rookFromSquares, sHalfMoveCount, sFullMoveNumber, operations);
      }

      var sValue = GetSingleValue(operations, "id");
      var sId = IsNullOrEmpty(sValue) ?
        sValue : VerbatimLiteralToString(sValue);

      setNameIfLegal(sId);
    }

    public void ParseFEN(String sPrefix, String sHalfMoveClock, String sFullMoveNumber) {
      using var scanner = new Scanner(sPrefix);
      var rookFromSquares = new List<Int32>(4);
      var bWTM = bWhiteMovesFirst;
      String? sEnPassant = default;
      try {
        bWTM = ParsePosition(scanner, out sEnPassant, rookFromSquares);
      }
      catch (PositionException ex) {
        LogInfo(Level.error, ex.Message);
      }
      finally {
        InitRoot(bWTM, sEnPassant, rookFromSquares, sHalfMoveClock, sFullMoveNumber);
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
      // The Pawns are set up along the 2nd and 7th rank in the same manner as they are in traditional, or
      // Orthodox Chess.
      //
      // The Pieces are set up randomly, subject to the following pair of constraints:
      //
      // 1) The two Rooks must appear on opposite sides of the King.
      // 2) One Bishop must be light-squared and the other dark-squared.
      //
      //[Note]Both sides have the same type of piece standing on each file.  So, initial Piece placements
      // for the two sides should be mirror images of each other, just as they are in Orthodox Chess.
      //
      // To see that there are a total of 960 permutations for the Pieces, first note that there are four
      // squares for each of the two Bishops, for a total of 16 possible arrangements.  Then six possible
      // squares remain for the Queen.
      //
      // This leaves five possible squares for the two Knights.  Since Knights are not distinguished from
      // each other, there are 5 choose 2 = C(5, 2) = 10 possible combinations for their placement.
      //
      // Then three empty squares remain where the King and the two Rooks will be placed, without choice.
      // Thus, there are 4 * 4 * 6 * 10 = 960 permutations.
      //
      // The 960 permutations occur in pairs, whose elements are mirror images of each other.  In one case,
      // the color of the squares on which the Queens stand will match the color of the Queens; and in the
      // other case the color of the squares on which the Queens stand will be opposite that of the Queens.
      //
      // We refer to the case where "Queens get their color" as the normal representation, and to the case
      // where the Queens do not get their color as the reflected representation for each of the 480 pairs.
      //
      // Orthodox Startpos: 249
      // Mirrored Startpos: 249 + 480 = 729
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
      var bReflect = IsOdd(wChess960);

      setup960(bReflect, nQueen, nKnights, nBishopDark, nBishopLite);
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
    private Int32 findEmptyFile(Boolean bReflect = false, Int32 nEmpty = 0) {
      var qpPiece = Side[White].Piece;
      var qp = bReflect ? BIT7 : BIT0;

      for (var n = 0; n <= nEmpty; n++) {
        if ((qp & qpPiece) != 0)
          nEmpty++;                     // Extend the search

        if (bReflect)
          qp >>= 1;
        else
          qp <<= 1;
      }

      return bReflect ? nFiles - 1 - nEmpty : nEmpty;
    }

    //
    // The following variation on findEmptyFile() limits the choice to squares of the same color:
    //
    private Int32 findEmpty2(Boolean bReflect = false, Int32 nEmpty = 0, Boolean bOdd = true) {
      const Int32 nBy = 2;
      var qpPiece = Side[White].Piece;
      var qp = bReflect ? BIT7 : BIT0;
      var n = 0;

      if (bOdd) {
        nEmpty++;
        n++;

        if (bReflect)
          qp >>= 1;
        else
          qp <<= 1;
      }

      for (; n <= nEmpty; n += nBy) {
        if ((qp & qpPiece) != 0)
          nEmpty += nBy;                // Extend the search

        if (bReflect)
          qp >>= nBy;
        else
          qp <<= nBy;
      }

      return bReflect ? nFiles - 1 - nEmpty : nEmpty;
    }

    private void setup960(
      Boolean bReflect, Int32 nQueen, Int32 nKnights, Int32 nBishopDark, Int32 nBishopLite) {
#if DEBUG
      LogLine(
        $"setup960(Queen: {nQueen}, " +
        $"KnightPair: {nKnights}, " +
        $"BishopLite: {nBishopLite}, " +
        $"BishopDark: {nBishopDark})");
#endif
      TestBinomials();                  //[Conditional]

      // Position.Clear() is performed by Push() in NewGame()

      #region Setup the Dark and Lite-Squared Bishops
      if (bReflect) {
        nBishopDark = nPerNibble - 1 - nBishopDark;
        nBishopLite = nPerNibble - 1 - nBishopLite;
        Swap(ref nBishopDark, ref nBishopLite);
      }

      var nBishopDarkFile = 2 * nBishopDark;
      var nBishopLiteFile = 2 * nBishopLite + 1;
#if DEBUG
      hasColor(nBishopDarkFile, SquareDark, "Dark Bishop");
      hasColor(nBishopLiteFile, SquareLite, "Lite Bishop");
#endif
      setupPiece(vB6, nBishopDarkFile);
      setupPiece(vB6, nBishopLiteFile);
      #endregion

      #region Setup the Queen
      var nQueen2 = 2 * nQueen;
      var nQueenSquare = findEmpty2(bReflect, nQueen2);
#if DEBUG
      hasColor(nQueenSquare, SquareLite, "Queen", "her");
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
      if (bReflect) {
        nKnight1 = 3 - nKnight1;
        nKnight2 = 4 - nKnight2;
      }

      var nKnight2File = findEmptyFile(false, nKnight2);
#if DEBUG
      var sqKnight2 = (Sq)nKnight2File;
#endif
      //
      // Occupy square before making the second choice:
      //
      setupPiece(vN6, nKnight2File);

      var nKnight1File = findEmptyFile(false, nKnight1);
#if DEBUG
      var sqKnight1 = (Sq)nKnight1File;
#endif
      setupPiece(vN6, nKnight1File);
      #endregion

      #region Setup Queenside Rook, King, and Kingside Rook
      var nRookFileOOO = findEmptyFile(bReflect);
      setupPiece(vR6, nRookFileOOO);

      setupPiece(vK6, findEmptyFile(bReflect));

      var nRookFileOO = findEmptyFile(bReflect);
      setupPiece(vR6, nRookFileOO);
      #endregion

      #region Setup the Pawns
      setupPawns();
      #endregion
#if ReflectSetup960
      var setup = PositionSetup(bReflect);
#else
      var setup = PositionSetup();
#endif
#if DEBUG
      var sense = bReflect ? "Reflected" : "Normal";
      LogLine($"{sense} Setup = {setup}");
#endif
      #region Grant Castling Rights and Init the Position
      //
      // With the Pieces and Pawns in place, the position can now be initialized:
      //
      State!.IsChess960 = setup != sOrthodoxSetup || bReflect;

      var rookFromSquares = new List<Int32>(4);
      setupCastlingRights(nRookFileOOO, nRookFileOO, rookFromSquares);

      var sEnPassant = Empty;
      var sHalfMoveCount = "0";
      var sFullMoveNumber = "1";
      InitRoot(bWhiteMovesFirst, sEnPassant, rookFromSquares, sHalfMoveCount, sFullMoveNumber);
      #endregion
    }

    private void setupPawns() {
      const Byte vPiece = vP6;
      for (var nFile = 0; nFile < nFiles; nFile++) {
        foreach (var side in Side) {
          var nTo = sqr(nFile, side.Parameter.PawnRank);
          side.PlacePiece(vPiece, nTo);
        }
      }
    }

    private void setupPiece(Byte vPiece, Int32 nFile) {
      if (nFile < nFiles) {            //[Safe]
        foreach (var side in Side) {
          var nTo = sqr(nFile, side.Parameter.PieceRank);
          side.PlacePiece(vPiece, nTo);
        }
      }
      else {
        Trace.Assert(nFile < nFiles, $"nFile = {nFile} >= nFiles {nFiles}");
      }
    }

    private void setupCastlingRights(
      Int32 nRookFileOOO, Int32 nRookFileOO, List<int> rookFromSquares) {
      ClearCastleRules();

      foreach (var side in Side) {
        //
        // Validation normally provided by parseCastlingFlags()
        //
        var nRookFromOOO = sqr(nRookFileOOO, side.Parameter.PieceRank);
        var nRookFromOO = sqr(nRookFileOO, side.Parameter.PieceRank);

        rookFromSquares.Add(nRookFromOOO);
        rookFromSquares.Add(nRookFromOO);
      }
    }

    #region Chess960 Setup Diagnostics
    private static void hasColor(Int32 n, Plane qpColor, String sName, String sPossessive = "its") {
      var sHas = (bit(n) & qpColor) != 0 ? "has" : "does not have";
      LogLine($"The {sName} at {(Sq)n} {sHas} {sPossessive} color.");
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
    private void verifyFEN(String sFEN) {
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
