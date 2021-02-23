//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-03-16 CNHume]Split Start into this file
//
// Conditionals:
//
#define Flip960
#define TestFEN

namespace Engine {
  using Command;

  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;

  using Test;

  using static Board.BoardSide;
  using static Command.Parser;
  using static Logging.Logger;
  using static System.String;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class Position : Board {
    #region Constants
    protected const String sOrthodoxStartEPD = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - hmvc 0; fmvn 1;";
    protected const String sOrthodoxStartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    #endregion

    #region Serialization Methods
    private Tabiya findTabiya() {
      // Match on the Position Prefix, ignoring variability in the Half Move Clock and Full Move Number values:
      var sPrefix = ToString(PositionType.Prefix);
      return TestPositions.FirstOrDefault(tabiya => tabiya.FEN.StartsWith(sPrefix));
    }

    protected PerfCase[] getTestCases() {
      var tabiyaFound = findTabiya();
      if (tabiyaFound is null)
        throw new PerftException("Test Position Not Found");

      var testCases = tabiyaFound.PerftCases;
      if (testCases is null)
        throw new PerftException("No Perft Cases Found");

      return testCases;
    }

    protected String getName(String sId = default) {
      var tabiyaFound = findTabiya();
      return tabiyaFound is null ? sId : tabiyaFound.Name;
    }

    private void setName(String sId = default) {
      Name = getName(sId);
    }

    private void setNameIfLegal(String sId = default) {
      //[Note]Calling IsLegal() affects FlagsLo and FlagsDraw
      if (!IsLegal())
        LogInfo(Level.error, "Illegal Setup");
      setName(sId);
    }

    public void ParseEPD(String sPrefix, Dictionary<String, List<String>> operations) {
      var scanner = new Scanner(sPrefix);
      try {
        var sHalfMoveCount = GetSingleValue(operations, "hmvc", "0");
        var sFullMoveNumber = GetSingleValue(operations, "fmvn", "0");
        var bWTM = ParsePosition(scanner, out String sPassed);
        Init(bWTM, sPassed, sHalfMoveCount, sFullMoveNumber, operations);
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
        var bWTM = ParsePosition(scanner, out String sPassed);
        Init(bWTM, sPassed, sHalfMoveClock, sFullMoveNumber);
      }
      catch (PositionException ex) {
        LogInfo(Level.error, ex.Message);
      }
      finally {
        scanner.Close();
      }

      setNameIfLegal();
    }

    public void SetEPD(String sEPD = sOrthodoxStartEPD) {
      if (IsNullOrEmpty(sEPD)) sEPD = sOrthodoxStartEPD;

      using (var parser = new Parser(sEPD)) {
        parser.SetupLexeme.Expect();
        var operations = parser.ParseOperations();
        ParseEPD(parser.SetupLexeme.Value, operations);
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
      var bFlip = IsEven(wChess960);

      setup960(bFlip, nQueen, nKnights, nBishopDark, nBishopLite);
    }

    /// <summary>Swap two entities of type T.</summary>
    protected static void Swap<T>(ref T e1, ref T e2) {
      var e = e1;
      e1 = e2;
      e2 = e;
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

    //
    // Return the last of nEmpty squares:
    //
    private Int32 findEmpty(Boolean bFlip, Int32 nEmpty = 0, Boolean bSkip = false) {
      var nBy = bSkip ? 2 : 1;
      var qpPiece = Side[White].Piece;
      var qp = bFlip ? BIT7 : BIT0;

      for (var n = 0; n <= nEmpty; n += nBy) {
        if ((qp & qpPiece) != 0)
          nEmpty += nBy;                // Extend the search

        if (bFlip)
          qp >>= nBy;
        else
          qp <<= nBy;
      }

      return bFlip ? 7 - nEmpty : nEmpty;
    }

    private static string has(Boolean bHas) {
      return bHas ? "has" : "does not have";
    }

    private void setup960(Boolean bFlip, Int32 nQueen, Int32 nKnights, Int32 nBishopDark, Int32 nBishopLite) {
      const Boolean bSkip = true;
      const Int32 nKnightEmpty = 5;
      choose2(nKnightEmpty, nKnights, out Int32 nKnight1, out Int32 nKnight2);
#if DEBUG
      LogLine($"setup960(Queen: {nQueen}, KnightPair: {nKnights}, BishopLite: {nBishopLite}, BishopDark: {nBishopDark})");
      LogLine($"choose2(N1: {nKnight1}, N2: {nKnight2})");
#endif
      Clear();                          // Clear Board

      //
      // Set up the Pieces:
      //
      if (bFlip) {
        nBishopDark = 3 - nBishopDark;
        nBishopLite = 3 - nBishopLite;
        Swap(ref nBishopDark, ref nBishopLite);
      }

      var nBishopDarkSquare = 2 * nBishopDark;
      var nBishopLiteSquare = 2 * nBishopLite + 1;
#if DEBUG
      var qpBishopDark = BIT0 << nBishopDarkSquare;
      var qpBishopLite = BIT0 << nBishopLiteSquare;
      var bBishopDarkHasColor = (DarkSquare & qpBishopDark) != 0;
      var bBishopLiteHasColor = (LiteSquare & qpBishopLite) != 0;
      var sBishopDarkHasColor = has(bBishopDarkHasColor);
      var sBishopLiteHasColor = has(bBishopLiteHasColor);
      var sqBishopDark = (sq)nBishopDarkSquare;
      var sqBishopLite = (sq)nBishopLiteSquare;
      LogLine($"The Dark Bishop at {sqBishopDark} {sBishopDarkHasColor} its color.");
      LogLine($"The Lite Bishop at {sqBishopLite} {sBishopLiteHasColor} its color.");
#endif
      setupPiece(vB6, nBishopDarkSquare);
      setupPiece(vB6, nBishopLiteSquare);

      var nQueenSquare = findEmpty(bFlip, 2 * nQueen, bSkip);
      var qpQueen = BIT0 << nQueenSquare;
      var bHasHerColor = (LiteSquare & qpQueen) != 0;
#if DEBUG
      var sqQueen = (sq)nQueenSquare;
      var sHasHerColor = has(bHasHerColor);
      LogLine($"The Queen at {sqQueen} {sHasHerColor} her color.");
#endif
      setupPiece(vQ6, nQueenSquare);

      //
      // Find empty square for the larger value first:
      //
      setupPiece(vN6, findEmpty(bFlip, nKnight2));
      setupPiece(vN6, findEmpty(bFlip, nKnight1));

      var nRookOOO = findEmpty(bFlip);
      setupPiece(vR6, nRookOOO);

      setupPiece(vK6, findEmpty(bFlip));

      var nRookOO = findEmpty(bFlip);
      setupPiece(vR6, nRookOO);

      //
      // Set up the Pawns:
      //
      setupPawns();
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
      //
      // With the Pieces and Pawns in place, the position can now be initialized:
      //
      var bWTM = true;
      grantCastling(nRookOOO, nRookOO);
      var sPassed = Empty;
      var sHalfMoveCount = "0";
      var sFullMoveNumber = "1";
      Init(bWTM, sPassed, sHalfMoveCount, sFullMoveNumber);
    }

    private void setupPawns() {
      var vPiece = vP6;
      for (sq sqWhite = sq.a2, sqBlack = sq.a7; sqWhite <= sq.h2; sqWhite++, sqBlack++) {
        var nWhite = (Int32)sqWhite;
        var nBlack = (Int32)sqBlack;
        placePiece(Side[White], vPiece, nWhite);
        placePiece(Side[Black], vPiece, nBlack);
      }
    }

    private void setupPiece(Byte vPiece, Int32 nWhite) {
      if (nWhite < nFiles) {            //[Safe]
        var nBlack = nRankLast + nWhite;
        placePiece(Side[White], vPiece, nWhite);
        placePiece(Side[Black], vPiece, nBlack);
      }
      else {
        Trace.Assert(nWhite < nFiles, $"nWhite = {nWhite} >= nFiles {nFiles}");
      }
    }

    private void grantCastling(Int32 nRookFromOOO, Int32 nRookFromOO, Boolean bChess960 = true) {
      var castle = State.Rule;
      if (castle is null)
        throw new BoardException("Null Castle Instance");

      castle.Clear();
      castle.IsChess960 = bChess960;

      foreach (var side in Side) {
        var qpRook = Rook & side.Piece;
        var nRank = side.SideName == SideName.White ? 0 : nRankLast;
        var nSide = (Int32)side.SideName;
        var rule = castle.RuleSide[nSide];

        //
        // GrantCastling() is normally called from parseCastleRights();
        // so extra validation is performed here.
        //
        side.FlagsHi &= ~HiFlags.CanCastleMask;
        side.FlagsHi |= rule.GrantCastling(side.KingPos, nRookFromOOO + nRank, qpRook, bChess960);
        side.FlagsHi |= rule.GrantCastling(side.KingPos, nRookFromOO + nRank, qpRook, bChess960);
      }
    }

    public void SetFEN(String sFEN = sOrthodoxStartFEN) {
      if (IsNullOrEmpty(sFEN)) sFEN = sOrthodoxStartFEN;

      using (var parser = new Parser(sFEN)) {
        parser.SetupLexeme.Expect();
        var sHalfMoveClock = parser.ParseCount("0");
        var sFullMoveNumber = parser.ParseCount("1");
        ParseFEN(parser.SetupLexeme.Value, sHalfMoveClock, sFullMoveNumber);
      }

      verifyFEN(sFEN);
    }

    [Conditional("TestFEN")]
    protected void verifyFEN(String sFEN) {
      var sFEN2 = ToString(PositionType.FEN);   // Canonical Output FEN
      var nFENLength = sFEN.Length;     // Input FEN abbreviations should match Output FEN prefix:
      var sFEN2Prefix = nFENLength < sFEN2.Length ? sFEN2.Substring(0, nFENLength) : sFEN2;

      if (sFEN != sFEN2Prefix)
        LogInfo(Level.warn, "Input FEN inconsistent with Output FEN");
    }
    #endregion
  }
}
