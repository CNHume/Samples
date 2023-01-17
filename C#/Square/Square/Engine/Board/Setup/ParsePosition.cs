﻿//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2011-07-05 CNHume]Created File
//
// Conditionals:
//
//#define DebugCastlingRights
//#define EnsureFromSquares
#define HashCastlingRights
//#define Magic

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Text;

  using Command;                        // For Scanner

  using Exceptions;

  using static System.Char;
  using static System.String;

  using static Command.Parser;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  partial class Board {
    #region Position Setup
    public void Validate() {
      foreach (var side in Side) {
        var nKings = (Int32)side.PieceCount(vK6);
        if (nKings != 1) {
          var message = $"Invalid {side.Parameter.SideName} King Placement";
          throw new InvalidPositionException(message);
        }

        if (((qpRank1 | qpRank8) & Pawn) != 0) {
          var message = "Invalid Pawn Placement";
          throw new InvalidPositionException(message);
        }

        var nPawns = (Int32)side.PieceCount(vP6);
        var nLimit = nFiles - nPawns;
        if (nLimit < 0) {
          var message = $"Too many {side.Parameter.SideName} Pawns";
          throw new InvalidPositionException(message);
        }

        //
        // Validate Piece Counts:
        //
        foreach (var p in Promotions) {
          var nSetup = p == Piece.Q ? 1 : 2;

          if (p == Piece.B)
            nSetup = HasBishopPair(side.FlagsSide) ? 2 : 1;

          var vPiece = PieceIndex((UInt32)p);
          var nCount = (Int32)side.PieceCount(vPiece);

          var nExtra = nCount - nSetup;
          if (nExtra > 0) nLimit -= nExtra;

          if (nLimit < 0) {
            var message = $"Too many {side.Parameter.SideName} pieces";
            throw new InvalidPositionException(message);
          }
        }
      }
    }

    private void parsePlacement(Char cPlacement, ref Boolean wasDigit, ref Int32 x, Int32 y) {
      const Boolean ignoreCase = true;

      if (!IsDigit(cPlacement)) {
        wasDigit = false;
        var bWhiteSide = IsUpper(cPlacement);
        var piece = cPlacement.ToString().TryParseEnum<Piece>(ignoreCase);
        if (!piece.HasValue)
          throw new ParsePositionException($"Unexpected Piece Name = {cPlacement}");

        var vPiece = PieceIndex((UInt32)piece);
        var side = GetSide(bWhiteSide);
        side.PlacePiece(vPiece, sqr(x, y));
        x++;                            // Placed Piece
      }
      else if (wasDigit)
        throw new ParsePositionException("Unexpected empty squares digit");
      else {                            // Skip Empty Squares
        wasDigit = true;
        var nEmpty = cPlacement - '0';
        x += nEmpty;

        if (x > nFiles) {
          var nRankPos = y + 1;
          throw new ParsePositionException($"Too many squares on rank {nRankPos}");
        }
      }
    }

    private void parseFENPlacements(String sPlacements) {
      var rank = sPlacements.Split(cSlash);
      var nRows = rank.Length;

      // Require a Row for each Rank
      if (nRows != nRanks) {
        var sb = new StringBuilder("FEN specifies ")
          .Append(nRows)
          .Append("rank");
        if (nRows != 1) sb.Append('s');
        throw new ParsePositionException(sb.ToString());
      }

      for (var nRow = 0; nRow < nRows; nRow++) {
        var y = InvertRank(nRow);
        var sRank = rank[nRow];
        var nLength = sRank.Length;
        var nPos = 0;
        var wasDigit = false;           // Disallow double digits 

        for (var x = 0; x < nFiles; nPos++) {
          if (nPos < nLength)
            parsePlacement(sRank[nPos], ref wasDigit, ref x, y);
          else
            throw new ParsePositionException($"Too few characters for rank {y + 1}");
        }                               //[Next]Char

        if (nPos < nLength)
          throw new ParsePositionException($"Too many characters for rank {y + 1}");
      }                                 //[Next]Rank
    }

    private static Boolean parseWTM(String sToMove) {
      return sToMove.ToLower() switch {
        "b" => false,
        "w" => true,
        _ => throw new ParsePositionException($"Unknown Side To Move = {sToMove}")
      };
    }

    //
    //[Chess960]Castle Rules are inferred from sCastleFlags.  These
    // present the only difference between Orthodox Chess and Chess960.
    //
    private void parseCastlingFlags(string sCastleFlags, List<int> rookFromSquares) {
      if (sCastleFlags.Length > 4)
        throw new ParsePositionException($"Invalid Castling Flags = {sCastleFlags}");

      var bChess960Flags = false;
      var bOrthodoxFlags = false;
      for (var nPos = 0; nPos < sCastleFlags.Length; nPos++) {
        var cFlag = sCastleFlags[nPos];
        switch (cFlag) {
        case 'K':
          cFlag = 'H';
          bOrthodoxFlags = true;
          break;
        case 'Q':
          cFlag = 'A';
          bOrthodoxFlags = true;
          break;
        case 'k':
          cFlag = 'h';
          bOrthodoxFlags = true;
          break;
        case 'q':
          cFlag = 'a';
          bOrthodoxFlags = true;
          break;
        default:
          bChess960Flags = true;
          break;
        }

        var cPosLower = ToLower(cFlag);
        if (cPosLower < cFileMin || cFileMax < cPosLower)
          throw new ParsePositionException($"Unknown Castle Flag = {cFlag}");

        var nRookFile = cPosLower - cFileMin;
        var bWhiteSide = IsUpper(cFlag);
        var side = GetSide(bWhiteSide);
        var pieceRank = side.Parameter.PieceRank;
        var nRookFrom = sqr(nRookFile, pieceRank);
#if DebugCastlingRights
        var sqRookFrom = (Sq)nRookFrom;
        var sideName = side.Parameter.SideName;
#endif
        rookFromSquares.Add(nRookFrom);
      }                                 //[Next]cFlag

      if (bChess960Flags && bOrthodoxFlags)
        throw new ParsePositionException("Mixed use of Chess 960 and Orthodox Castling Flags");
    }

    private void parsePassed(String? sEnPassant) {
      const Boolean ignoreCase = true;

      //
      //[Init]buildPawnAtx() is called here because Pawn[A1H8|A8H1]Atx
      // will be needed when tryEP() calls Friend.Passed().
      //
      buildPawnAtx();

      if (IsNullOrEmpty(sEnPassant) || sEnPassant == "-")
        return;

      var sqEnPassant = sEnPassant.TryParseEnum<Sq>(ignoreCase);
      if (!sqEnPassant.HasValue)
        throw new ParsePositionException($"Invalid En Passant String = {sEnPassant}");

      // The destination square to which an e.p. capturing Pawn will move:
      var nEnPassant = (Int32)sqEnPassant;
      if (y(nEnPassant) != Foe.Parameter.PassRank)
        throw new ParsePositionException($"Invalid En Passant Rank = {sqEnPassant}");

      var qpFoe = Foe.Piece;
      // The square actually holding the e.p. Pawn to be captured:
      var nMovedTo = nEnPassant + Foe.Parameter.PawnStep;

      //
      // The square on nTo must have a Pawn; and both squares "behind" nTo must be vacant:
      //
      var nStart = nEnPassant + Friend.Parameter.PawnStep;
      var qpStart = bit(nStart);
      var qpEnPassant = bit(nEnPassant);
      var qpVacant = qpStart | qpEnPassant;
      var bInvalid = (qpVacant & RankPiece) != 0 ||
                     (qpFoe & Pawn & bit(nMovedTo)) == 0;

      if (bInvalid)
        throw new ParsePositionException($"Invalid En Passant Square = {sqEnPassant}");

      tryEP(nEnPassant);

      if (!IsPassed())
        LogInfo(Level.warn, $"Illegal En Passant Square = {sqEnPassant}");
    }

    //
    // Specification of Forsyth-Edwards Notation (FEN) and of Extended Position Description (EPD),
    // can be found in the Portable Game Notation Specification and Implementation Guide:
    // http://www.drpribut.com/sports/standard.txt
    //
    // EPD Syntax is also covered at
    // https://www.chessprogramming.org/Extended_Position_Description#EPD_Syntax
    //
    // An EPD opcode legend can also be found at
    // https://www.chessprogramming.org/Extended_Position_Description#Opcode_mnemonics
    //
    protected Boolean ParsePosition(
      Scanner scanner, List<int> rookFromSquares, out String sPassed) {
      // Clear() should have been performed by the Push() in NewGame()
      //[Debug]Clear();

      //
      // 1. Scanner Pieces by Rank
      //
      if (!scanner.HasTextSpan())
        throw new ParsePositionException("No piece placements provided");

      parseFENPlacements(scanner.Next());

      //
      // 2. Side to Move
      //
      var sToMove = scanner.HasTextSpan() ? scanner.Next() : "w";
      var bWTM = parseWTM(sToMove);

      //
      // 3. Castling Flags
      //
      ClearCastleRules();
      var sCastleFlags = scanner.HasTextSpan() ? scanner.Next() : "-";
      if (sCastleFlags != "-")
        parseCastlingFlags(sCastleFlags, rookFromSquares);

      //
      // 4. Square Passed for En Passant
      //
      sPassed = scanner.HasTextSpan() ? scanner.Next() : "-";
      return bWTM;
    }

    protected void InitRoot(
      Boolean bWTM, List<int> rookFromSquares, String? sEnPassant,
      String? sHMVCValue, String? sFMVNValue, Dictionary<String, List<String>?>? operations = default) {
      const string sFMVNName = "Full Move Number";
      const string sHMVCName = "Half Move Clock";

      #region EPD Operations
      Operations = operations;
      #endregion

      //
      // Outside of their Equal Masks, FlagsTurn and FlagsSide
      // were reset by pushRoot()
      //
      setWTM(bWTM);

      #region EnPassant
      parsePassed(sEnPassant);
      Hash ^= hashFlags(bWTM);          // Includes epHash()
      #endregion                        // EnPassant

      #region Init Castling
      initCastling(rookFromSquares);
      #endregion                        // Init Castling

      #region Half Move Clock and Full Move Number
      HalfMoveClock = ParseByte(sHMVCName, sHMVCValue);

      if (IsPassed() && HalfMoveClock > 0) {
        var sqEP = (Sq)FlagsTurn.sqrEP();
        LogInfo(Level.warn, $"ep({sqEP}) implies {sHMVCName} = {HalfMoveClock} Must Be Zero");
      }

      // Parent == null for a Root Position; so SetDraw0 can be elided here.
      //updateRepetitionCycle();

      var wMoveNumber = ParseUInt16(sFMVNName, sFMVNValue);
      // Zero is sometimes used when the initial MoveNumber is unknown
      if (wMoveNumber == 0) wMoveNumber = 1;
      State!.MovePly = plyCount(wMoveNumber);
      if (!bWTM) State!.MovePly++;
      #endregion                        // Half Move Clock and Full Move Number

      try {
        Validate();
        initCastleRules();
      }
      catch (InvalidPositionException ex) {
        Display(ex.Message);
        throw;
      }
    }

    private void ensureCastlingSymmetry() {
      var (blackSide, whiteSide) = Side.GetBothSides();
      var blackRule = blackSide.Parameter.Rule;
      var whiteRule = whiteSide.Parameter.Rule;

      if (blackRule.CastlesFrom.HasValue && whiteRule.CastlesFrom.HasValue) {
        if (x(blackRule.CastlesFrom.Value) != x(whiteRule.CastlesFrom.Value))
          throw new ParsePositionException("Kings must castle from the same file");
      }
#if EnsureFromSquares
      else if (blackRule.CastlesFrom.HasValue || whiteRule.CastlesFrom.HasValue) {
        if (blackRule.CastlesFrom.HasValue)
          whiteRule.CastlesFrom = sqr(x(blackRule.CastlesFrom.Value), whiteSide.Parameter.PieceRank);
        else if (whiteRule.CastlesFrom.HasValue)
          blackRule.CastlesFrom = sqr(x(whiteRule.CastlesFrom.Value), blackSide.Parameter.PieceRank);
      }
#endif                                  // EnsureFromSquares
      if (blackRule.RookOOFrom.HasValue && whiteRule.RookOOFrom.HasValue) {
        if (x(blackRule.RookOOFrom.Value) != x(whiteRule.RookOOFrom.Value))
          throw new ParsePositionException("Rooks must OO from the same file");
      }
#if EnsureFromSquares
      else if (blackRule.RookOOFrom.HasValue || whiteRule.RookOOFrom.HasValue) {
        if (blackRule.RookOOFrom.HasValue)
          whiteRule.RookOOFrom = sqr(x(blackRule.RookOOFrom.Value), whiteSide.Parameter.PieceRank);
        else if (whiteRule.RookOOFrom.HasValue)
          blackRule.RookOOFrom = sqr(x(whiteRule.RookOOFrom.Value), blackSide.Parameter.PieceRank);
      }
#endif                                  // EnsureFromSquares
      if (blackRule.RookOOOFrom.HasValue && whiteRule.RookOOOFrom.HasValue) {
        if (x(blackRule.RookOOOFrom.Value) != x(whiteRule.RookOOOFrom.Value))
          throw new ParsePositionException("Rooks must OOO from the same file");
      }
#if EnsureFromSquares
      else if (blackRule.RookOOOFrom.HasValue || whiteRule.RookOOOFrom.HasValue) {
        if (blackRule.RookOOOFrom.HasValue)
          whiteRule.RookOOOFrom = sqr(x(blackRule.RookOOOFrom.Value), whiteSide.Parameter.PieceRank);
        else if (whiteRule.RookOOOFrom.HasValue)
          blackRule.RookOOOFrom = sqr(x(whiteRule.RookOOOFrom.Value), blackSide.Parameter.PieceRank);
      }
#endif                                  // EnsureFromSquares
    }

    private Boolean IsChess960() {
      foreach (var side in Side)
        if (!side.Parameter.Rule.IsOrthodoxCastling())
          return true;

      return false;
    }

    private Position.PositionSide findSide(int nFrom) {
      foreach (var side in Side)
        if (side.Parameter.PieceRank == y(nFrom))
          return side;

      var vPiece = GetPieceIndex(nFrom);
      var piece = IndexPiece(vPiece);
      var sqFrom = (Sq)nFrom;
      throw new ParsePositionException($"Side not found for {piece} at {sqFrom}");
    }

    /*
     * To build Castling Rules for Root Position:
     *
     * side.FlagsSide = default                 // Clear Castling Rights from Position.Clear()
     * side.ClearCastleRule()
     * rookFromSquares.Add(nRookFrom)           // Add rookFromSquares per parseCastlingFlags(sCastleFlags)
     * side.GrantCastling(nRookFrom)            // Set FromSquares and CastlingRights
     * side.HashCastlingRights()
     * ensureCastlingSymmetry()                 // Verify FromSquares
     * IsChess960()
     * side.InitCastleRule()                    // Set OOO and OO rules
     */
    private void initCastling(List<int> rookFromSquares) {
      //[Test]rookFromSquares.Sort();
      foreach (var nRookFrom in rookFromSquares) {
        var side = findSide(nRookFrom);
        side.GrantCastling(nRookFrom);
      }

      foreach (var side in Side)
        side.HashCastlingRights();

      ensureCastlingSymmetry();

      State!.IsChess960 = IsChess960();
    }

    protected void ClearCastleRules() {
      foreach (var side in Side)
        side.ClearCastleRule();
    }

    private void initCastleRules() {
      foreach (var side in Side)
        side.InitCastleRule();
    }
    #endregion
  }
}
