//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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
            nSetup = hasBishopPair(side.FlagsSide) ? 2 : 1;

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
      sInvalid = sReason;
      return bValid;
    }

    private void parsePlacement(Char cPlacement, ref Boolean wasDigit, ref Int32 x, Int32 y) {
      if (!IsDigit(cPlacement)) {
        wasDigit = false;
        var bWhiteSide = IsUpper(cPlacement);
        var piece = TryParsePiece(cPlacement.ToString());
        if (!piece.HasValue)
          throw new ParsePositionException($"Unexpected Piece Name = {cPlacement}");

        var vPiece = pieceIndex((UInt32)piece);
        var side = getSide(bWhiteSide);
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
      const Char cSlash = '/';
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
      return sToMove.ToLower() switch {
        "b" => false,
        "w" => true,
        _ => throw new ParsePositionException($"Unknown Side To Move = {sToMove}")
      };
    }

    //
    //[Chess 960]InitCastleRules() and parseCastleRights() allow for Chess 960 castling
    //
    private Boolean parseCastleRights(String sCastleFlags) {
      var bOrthodox = false;

      if (sCastleFlags != "-") {
        if (sCastleFlags.Length > 4)
          throw new ParsePositionException($"Invalid Castling Flags = {sCastleFlags}");

        //State.IsChess960 = false;
        State.ClearCastleRule(Side);

        foreach (var side in Side)
          side.FlagsSide &= ~SideFlags.CanCastleMask;

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
            State.IsChess960 = true;
            break;
          }

          var cPosLower = ToLower(cFlag);
          if (cPosLower < cFileMin || cFileMax < cPosLower)
            throw new ParsePositionException($"Unknown Castle Flag = {cFlag}");

          var nRookFile = cPosLower - cFileMin;
          var bWhiteSide = IsUpper(cFlag);
          var side = getSide(bWhiteSide);
          var rule = side.Rule;
          var nRank = side.Parameter.StartRank;

          side.FlagsSide |= rule.GrantCastling(side.KingPos, nRookFile + nRank, Rook & side.Piece, State.IsChess960);
        }                               //[Next]Right

        if (State.IsChess960 && bOrthodox)
          throw new ParsePositionException("Mixed use of Chess 960 and Orthodox Castling Flags");

        ValidateCastling();
      }

      return bOrthodox;
    }

    public void ValidateCastling() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      var blackRule = blackSide.Rule;
      var whiteRule = whiteSide.Rule;

      if (blackRule.CastlesFrom.HasValue && whiteRule.CastlesFrom.HasValue) {
        if (blackRule.CastlesFrom.Value != whiteRule.CastlesFrom.Value + nRankLast)
          throw new ParsePositionException("Both Kings must castle from the same file");
      }
      else if (blackRule.CastlesFrom.HasValue || whiteRule.CastlesFrom.HasValue) {
        if (blackRule.CastlesFrom.HasValue)
          whiteRule.CastlesFrom = blackRule.CastlesFrom.Value - nRankLast;
        else if (whiteRule.CastlesFrom.HasValue)
          blackRule.CastlesFrom = whiteRule.CastlesFrom.Value + nRankLast;
      }

      if (blackRule.RookOOFrom.HasValue && whiteRule.RookOOFrom.HasValue) {
        if (blackRule.RookOOFrom.Value != whiteRule.RookOOFrom.Value + nRankLast)
          throw new ParsePositionException("Both sides must OO with Rooks from the same file");
      }
      else if (blackRule.RookOOFrom.HasValue || whiteRule.RookOOFrom.HasValue) {
        if (blackRule.RookOOFrom.HasValue)
          whiteRule.RookOOFrom = blackRule.RookOOFrom.Value - nRankLast;
        else if (whiteRule.RookOOFrom.HasValue)
          blackRule.RookOOFrom = whiteRule.RookOOFrom.Value + nRankLast;
      }

      if (blackRule.RookOOOFrom.HasValue && whiteRule.RookOOOFrom.HasValue) {
        if (blackRule.RookOOOFrom.Value != whiteRule.RookOOOFrom.Value + nRankLast)
          throw new ParsePositionException("Both sides must OOO with Rooks from the same file");
      }
      else if (blackRule.RookOOOFrom.HasValue || whiteRule.RookOOOFrom.HasValue) {
        if (blackRule.RookOOOFrom.HasValue)
          whiteRule.RookOOOFrom = blackRule.RookOOOFrom.Value - nRankLast;
        else if (whiteRule.RookOOOFrom.HasValue)
          blackRule.RookOOOFrom = whiteRule.RookOOOFrom.Value + nRankLast;
      }
    }

    protected void parsePassed(String sEnPassant) {
      //
      //[Init]buildPawnAtx() is called here because Pawn[A1H8|A8H1]Atx
      // are needed for passed() and hence for tryEP() below.
      //
      buildPawnAtx();

      if (IsNullOrEmpty(sEnPassant) || sEnPassant == "-")
        return;

      var sqEnPassant = TryParseSquare(sEnPassant);
      if (!sqEnPassant.HasValue)
        throw new ParsePositionException($"Invalid En Passant String = {sEnPassant}");

      // The destination square to which an e.p. capturing Pawn will move:
      var nEnPassant = (Int32)sqEnPassant;
      if (y(nEnPassant) != Friend.Parameter.EnPassantRank)
        throw new ParsePositionException($"Invalid En Passant Rank = {sqEnPassant}");

      var qpFoe = Foe.Piece;
      // The square actually holding the e.p. Pawn to be captured:
      var nMovedTo = nEnPassant + Foe.Parameter.ShiftRank;

      //
      // The square on nTo must have a Pawn; and both squares "behind" nTo must be vacant:
      //
      var nStart = nEnPassant + Friend.Parameter.ShiftRank;
      var qpStart = BIT0 << nStart;
      var qpEnPassant = BIT0 << nEnPassant;
      var qpVacant = qpStart | qpEnPassant;
      var bInvalid = (qpVacant & RankPiece) != 0 ||
                     (qpFoe & Pawn & BIT0 << nMovedTo) == 0;

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
      Boolean bWTM, String sEnPassant, String sHalfMoveClock, String sFullMoveNumber,
      Dictionary<String, List<String>?>? operations = default) {
      // Preserve EPD Operations passed via ParseEPD()
      Operations = operations;

      //
      // FlagsLo/FlagsSide bits outside of their respective Equal Masks were reset by pushRoot()
      //
      setWTM(bWTM);
      parsePassed(sEnPassant);
      Hash ^= hashFlags(bWTM);

      HalfMoveClock = ParseByte("Half Move Clock", sHalfMoveClock);

      var wMoveNumber = ParseUInt16("Full Move Number", sFullMoveNumber);
      // Zero is sometimes used when the initial MoveNumber is unknown
      if (wMoveNumber == 0) wMoveNumber = 1;
      State.MovePly = plyCount(wMoveNumber);
      if (!bWTM) State.MovePly++;

      if (IsValid(out string sInvalid)) {
        foreach (var side in Side)
          side.Rule.Init();
      }
      else {
        Display(sInvalid);
        throw new InvalidPositionException(sInvalid);
      }
    }
    #endregion
  }
}
