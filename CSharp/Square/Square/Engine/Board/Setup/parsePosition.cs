﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2011-07-05 CNHume]Created File
//
// Conditionals:
//
//#define Magic

namespace Engine {
  using Command;                        // For Scanner
  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Text;

  using static CastleRule;
  using static Command.Parser;
  using static Logging.Logger;

  using static System.Char;
  using static System.String;

  //
  // Type Aliases:
  //
  partial class Board {
    #region Position Setup
    public Boolean IsValid(out String sInvalid) {       // Validate a new setup
      var bValid = true;
      var sReason = Empty;

      foreach (var side in Side) {
        var nKings = (Int32)nibble(side.Counts >> vK6 * nPerNibble);
        if (nKings != 1) {
          bValid = false;
          sReason = $"Invalid {side.Parameter.SideName} King Placement";
          break;
        }

        if (((qpRank1 | qpRank8) & Pawn) != 0) {
          bValid = false;
          sReason = "Invalid Pawn Placement";
          break;
        }

        var nPawns = (Int32)nibble(side.Counts >> vP6 * nPerNibble);
        var nLimit = nFiles - nPawns;
        if (nLimit < 0) {
          bValid = false;
          sReason = $"Too many {side.Parameter.SideName} Pawns";
          break;
        }

        //
        // Validate Piece Counts:
        //
        foreach (var p in Promotions) {
          var nSetup = p == Piece.Q ? 1 : 2;

          if (p == Piece.B)
            nSetup = bishopPair(side.FlagsHi) ? 2 : 1;

          var vPiece = pieceIndex((UInt32)p);
          var nCount = (Int32)nibble(side.Counts >> vPiece * nPerNibble);

          var nExtra = nCount - nSetup;
          if (nExtra > 0) nLimit -= nExtra;

          if (nLimit < 0) {
            bValid = false;
            sReason = $"Too many {side.Parameter.SideName} pieces";
            goto exit;
          }
        }
      }
    exit:

      sInvalid = bValid ? null : sReason;
      return bValid;
    }

    private void parsePlacement(Char cPlacement, ref Boolean wasDigit, ref Int32 x, Int32 y) {
      if (!IsDigit(cPlacement)) {
        wasDigit = false;
        var bColor = IsUpper(cPlacement);
        var piece = TryParsePiece(cPlacement.ToString());
        if (!piece.HasValue)
          throw new ParsePositionException($"Unexpected Piece Name = {cPlacement}");

        var vPiece = pieceIndex((UInt32)piece);
        var side = getSide(bColor);
        placePiece(side, vPiece, sqr(x, y));
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
      const Char cSlash = '/';
      var rank = sPlacements.Split(cSlash);
      var nRows = rank.Length;

      // Require a Row for each Rank
      if (nRows != nRanks) {
        var sb = new StringBuilder("FEN specifies ")
          .Append(nRows)
          .Append("rank");
        if (nRows != 1) sb.Append("s");
        throw new ParsePositionException(sb.ToString());
      }

      for (var nRow = 0; nRow < nRows; nRow++) {
        var y = invertRank(nRow);
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
      switch (sToMove.ToLower()) {
      case "w":
        return true;
      case "b":
        return false;
      default:
        throw new ParsePositionException($"Unknown Side To Move = {sToMove}");
      }
    }

    //
    //[Chess 960]parseCastleRights() and InitCastleRules() allow for Chess 960 castling
    //
    private Boolean parseCastleRights(String sCastleFlags) {
      var bOrthodox = false;

      if (sCastleFlags != "-") {
        if (sCastleFlags.Length > 4)
          throw new ParsePositionException($"Invalid Castling Flags = {sCastleFlags}");

        var castle = State.Rule;
        if (castle is null)
          throw new BoardException("Null Castle Instance");
        castle.Clear();
        foreach (var side in Side)
          side.FlagsHi &= ~HiFlags.CanCastleMask;

        for (var nPos = 0; nPos < sCastleFlags.Length; nPos++) {
          var cFlag = sCastleFlags[nPos];
          switch (cFlag) {
          case 'K':
            cFlag = 'H';
            bOrthodox = true;
            break;
          case 'Q':
            cFlag = 'A';
            bOrthodox = true;
            break;
          case 'k':
            cFlag = 'h';
            bOrthodox = true;
            break;
          case 'q':
            cFlag = 'a';
            bOrthodox = true;
            break;
          default:
            castle.IsChess960 = true;
            break;
          }

          var cPosLower = ToLower(cFlag);
          if (cPosLower < cFileMin || cFileMax < cPosLower)
            throw new ParsePositionException($"Unknown Castle Flag = {cFlag}");

          var nRookFile = cPosLower - cFileMin;
          var bWhite = IsUpper(cFlag);
          var side = getSide(bWhite);
          var rule = getRule(bWhite);
          var nRank = bWhite ? 0 : nRankLast;
          side.FlagsHi |= rule.GrantCastling(side.KingPos, nRookFile + nRank, Rook & side.Piece, castle.IsChess960);
        }                               //[Next]Right

        if (castle.IsChess960 && bOrthodox)
          throw new ParsePositionException("Mixed use of Chess 960 and Orthodox Castling Flags");

        bool bFromSameFile = Side[White].KingPos + nRankLast == Side[Black].KingPos;
        castle.ValidateCastlingSymmetry(bFromSameFile);
      }

      return bOrthodox;
    }

    protected void parsePassed(Boolean bWTM, String sPassed) {
      //
      //[Init]buildPawnAtx() is called here because Pawn[A1H8|A8H1]Atx
      // are needed for passed() and hence for tryEP() below.
      //
      buildPawnAtx();

      FlagsLo &= ~LoFlags.Copy;         // Includes WTM
      if (bWTM) FlagsLo |= LoFlags.WTM;

      if (IsNullOrEmpty(sPassed) || sPassed == "-")
        return;

      var sqPassed = TryParseSquare(sPassed);
      if (!sqPassed.HasValue)
        throw new ParsePositionException($"Invalid En Passant String = {sPassed}");

      (BoardSide friend, BoardSide foe) = getSides(bWTM);

      // The destination square to which an e.p. capturing Pawn will move:
      var nEnPassantTo = (Int32)sqPassed;
      if (y(nEnPassantTo) != friend.Parameter.EnPassantRank)
        throw new ParsePositionException($"Invalid En Passant Rank = {sqPassed}");

      var qpFoe = foe.Piece;
      // The square actually holding the e.p. Pawn to be captured:
      var nCaptureFrom = nEnPassantTo + foe.Parameter.ShiftRank;

      //
      // The square on nTo must have a Pawn; and both squares "behind" nTo must be vacant:
      //
      var nStart = nEnPassantTo + friend.Parameter.ShiftRank;
      var qpStart = BIT0 << nStart;
      var qpPassed = BIT0 << nEnPassantTo;
      var qpVacant = qpStart | qpPassed;
      var bInvalid = (qpVacant & RankPiece) != 0 ||
                     (qpFoe & Pawn & BIT0 << nCaptureFrom) == 0;

      if (bInvalid)
        throw new ParsePositionException($"Invalid En Passant Square = {sqPassed}");

      (CastleRuleParameter friendRule, CastleRuleParameter foeRule) = getRules(bWTM);
      tryEP(friend, friendRule, foe, foeRule, nCaptureFrom, nEnPassantTo);

      if (!IsPassed())
        LogInfo(Level.warn, $"Illegal En Passant Square = {sqPassed}");
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
    protected Boolean ParsePosition(Scanner scanner, out String sPassed) {
      Boolean bWTM;
      Clear();                          // Clear Board

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
      bWTM = parseWTM(sToMove);

      //
      // 3. Castling Flags
      //
      var sCastleFlags = scanner.HasTextSpan() ? scanner.Next() : "-";
      parseCastleRights(sCastleFlags);

      //
      // 4. Square Passed for En Passant
      //
      sPassed = scanner.HasTextSpan() ? scanner.Next() : "-";
      return bWTM;
    }

    protected void Init(
      Boolean bWTM, String sPassed, String sHalfMoveClock, String sFullMoveNumber,
      Dictionary<String, List<String>> operations = default) {
      // Preserve EPD Operations passed via ParseEPD()
      Operations = operations;

      //
      // FlagsHi/Lo bits outside of those in their respective Equal Masks were reset by pushRoot()
      //
      parsePassed(bWTM, sPassed);

      Hash ^= hashFlags(bWTM);

      HalfMoveClock = ParseByte("Half Move Clock", sHalfMoveClock);

      var wMoveNumber = ParseUInt16("Full Move Number", sFullMoveNumber);
      // Zero is sometimes used when the initial MoveNumber is unknown
      if (wMoveNumber == 0) wMoveNumber = 1;
      State.MovePly = plyCount(wMoveNumber);
      if (!bWTM) State.MovePly++;

      if (IsValid(out string sInvalid)) {
        if (State.Rule is null)
          throw new BoardException("Null CastleRule Instance");

        State.Rule.Init();
      }
      else {
        Display(sInvalid);
        throw new InvalidPositionException(sInvalid);
      }
    }
    #endregion
  }
}
