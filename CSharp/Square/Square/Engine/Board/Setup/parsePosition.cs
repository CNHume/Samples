//
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

  using static Board.BoardSide;
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
        var nKings = (Byte)(side.Counts >> vK6 * nPerNibble) & vNibble;
        if (nKings != 1) {
          bValid = false;
          sReason = $"Invalid {side.SideName} King Placement";
          break;
        }

        if (((qpRank1 | qpRank8) & Pawn) != 0) {
          bValid = false;
          sReason = "Invalid Pawn Placement";
          break;
        }

        var nPawns = (Byte)(side.Counts >> vP6 * nPerNibble) & vNibble;
        var nLimit = nFiles - nPawns;
        if (nLimit < 0) {
          bValid = false;
          sReason = $"Too many {side.SideName} Pawns";
          break;
        }

        //
        // Validate Piece Counts:
        //
        foreach (var p in Promotions) {
          var nSetup = p == Piece.Q ? 1 : 2;

          if (p == Piece.B)
            nSetup = bishopPair(side.FlagsHi) ? 2 : 1;

          var nPiece = (Int32)p - vFirst;
          var nCount = (Byte)(side.Counts >> nPiece * nPerNibble) & vNibble;

          var nExtra = nCount - nSetup;
          if (nExtra > 0) nLimit -= nExtra;

          if (nLimit < 0) {
            bValid = false;
            sReason = $"Too many {side.SideName} pieces";
            goto exit;
          }
        }
      }
    exit:

      sInvalid = bValid ? null : sReason;
      return bValid;
    }

    private void parsePieces(String sPieces) {
      const Char cSlash = '/';
      var rank = sPieces.Split(cSlash);
      var nRows = rank.Length;

      if (nRows != nRanks) {
        var sb = new StringBuilder("FEN specifies ")
          .Append(nRows)
          .Append("rank");
        if (nRows != 1) sb.Append("s");
        throw new ParsePositionException(sb.ToString());
      }

      for (var nRow = 0; nRow < nRows; nRow++) {
        var sRank = rank[nRow];
        var nRankLength = sRank.Length;
        var nChar = 0;
        var y = nRanks - (nRow + 1);
        var wasDigit = false;

        for (var x = 0; x < nFiles; nChar++) {
          if (nChar < nRankLength) {
            var cParse = sRank[nChar];

            if (IsDigit(cParse)) {
              if (wasDigit)
                throw new ParsePositionException("Unexpected empty squares digit");

              wasDigit = true;
              var nEmpty = cParse - '0';
              x += nEmpty;

              if (x > nFiles) {
                var nRankPos = y + 1;
                throw new ParsePositionException($"Too many squares on rank {nRankPos}");
              }
            }                           // Skipped Empty Squares
            else {
              wasDigit = false;
              var nTo = sqr(x, y);
              var qp = BIT0 << nTo;
              var bColor = IsUpper(cParse);

              var vPiece = vPieceNull;
              switch (ToUpper(cParse)) {
              case 'P':
                vPiece = vP6;
                break;
              case 'R':
                vPiece = vR6;
                break;
              case 'N':
                vPiece = vN6;
                break;
              case 'B':
                vPiece = vB6;
                break;
              case 'Q':
                vPiece = vQ6;
                break;
              case 'K':
                vPiece = vK6;
                break;
              default:
                throw new ParsePositionException($"Unexpected Piece Name = {cParse}");
              }

              var side = getSide(bColor);
              placePiece(side, vPiece, nTo);
              x++;                      // Placed Piece
            }
          }                             // Parsed Char
          else {
            var nRankPos = y + 1;
            throw new ParsePositionException($"Too few characters for rank {nRankPos}");
          }
        }                               //[Next]Char

        if (nChar < nRankLength) {
          var nRankPos = y + 1;
          throw new ParsePositionException($"Too many characters for rank {nRankPos}");
        }
      }                                 //[Next]Rank
    }

    //
    //[Chess 960]parseCastleRights() and InitCastleRules() allow for Chess 960 castling
    //
    private Boolean parseCastleRights(String sRights) {
      var bOrthodox = false;

      if (sRights != "-") {
        if (sRights.Length > 4)
          throw new ParsePositionException($"Invalid Castling Abilities = {sRights}");

        var castle = State.Rule;
        if (castle is null)
          throw new BoardException("Null Castle Instance");
        castle.Clear();
        foreach (var side in Side)
          side.FlagsHi &= ~HiFlags.CanCastleMask;

        for (var pos = 0; pos < sRights.Length; pos++) {
          var cPos = sRights[pos];
          switch (cPos) {
          case 'K':
            cPos = 'H';
            bOrthodox = true;
            break;
          case 'Q':
            cPos = 'A';
            bOrthodox = true;
            break;
          case 'k':
            cPos = 'h';
            bOrthodox = true;
            break;
          case 'q':
            cPos = 'a';
            bOrthodox = true;
            break;
          default:
            castle.IsChess960 = true;
            break;
          }

          var cPosLower = ToLower(cPos);
          if (cPosLower < cFileMin || cFileMax < cPosLower)
            throw new ParsePositionException($"Unknown Castle Flag = {cPos}");

          var nRookFile = cPosLower - cFileMin;
          var bUpper = IsUpper(cPos);
          var side = getSide(bUpper);
          var rule = getRule(bUpper);
          var nRank = bUpper ? 0 : nRankLast;
          side.FlagsHi |= rule.GrantCastling(side.KingPos, nRookFile + nRank, Rook & side.Piece, castle.IsChess960);
        }                               //[Next]Right

        if (castle.IsChess960 && bOrthodox)
          throw new ParsePositionException("Mixed use of Chess 960 and Orthodox Castling Rights");

        castle.ValidateCastlingSymmetry(Side[White].KingPos + nRankLast == Side[Black].KingPos);
      }

      return bOrthodox;
    }

    public static sq? TryParseSquare(String s) {
      var sq = (sq?)null;

      if (s?.Length == 2) {
        var cFile = Char.ToLower(s[0]);
        var cRank = s[1];
        if (cFileMin <= cFile && cFile <= cFileMax &&
            cRankMin <= cRank && cRank <= cRankMax) {
          var nX = cFile - cFileMin;
          var nY = cRank - cRankMin;
          sq = (sq)sqr(nX, nY);
        }
      }

      return sq;
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

      var nExpectedRank = bWTM ? 5 : 2;
      var nPassedTo = (Int32)sqPassed;
      if (nPassedTo / nFiles != nExpectedRank)
        throw new ParsePositionException($"Invalid En Passant Rank = {sqPassed}");

      var bValid = false;
      (BoardSide friend, BoardSide foe) = getSides(bWTM);

      var qpFoe = foe.Piece;
      var nTo = nPassedTo - friend.Rank;

      //
      // The square on nTo must have a Pawn; and both squares "behind" nTo must be vacant:
      //
      var nStart = nPassedTo + friend.Rank;
      var qpStart = BIT0 << nStart;
      var qpPassed = BIT0 << nPassedTo;
      var qpVacant = qpStart | qpPassed;
      bValid = (qpFoe & Pawn & BIT0 << nTo) != 0 && (qpVacant & RankPiece) == 0;

      if (!bValid)
        throw new ParsePositionException($"Invalid En Passant Square = {sqPassed}");

      (CastleRuleSide friendRule, CastleRuleSide foeRule) = getRules(bWTM);
      tryEP(friend, friendRule, foe, foeRule, nTo, nPassedTo);

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

      parsePieces(scanner.Next());

      //
      // 2. Side to Move
      //
      var sToMove = scanner.HasTextSpan() ? scanner.Next() : "w";

      switch (sToMove.ToLower()) {
      case "w":
        bWTM = true;
        break;
      case "b":
        bWTM = false;
        break;
      default:
        throw new ParsePositionException($"Unknown Side To Move = {sToMove}");
      }

      //
      // 3. Castling Abilities
      //
      var sRights = scanner.HasTextSpan() ? scanner.Next() : "-";
      parseCastleRights(sRights);

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
