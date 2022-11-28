//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-07-05 CNHume]Created File
//
// Conditionals:
//
#define HashCastlingRights
//#define Magic
//#define UpdateRepetitionCycle

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
    public String? IsValid() {           // Validate a new setup
      foreach (var side in Side) {
        var nKings = (Int32)side.PieceCount(vK6);
        if (nKings != 1) {
          return $"Invalid {side.Parameter.SideName} King Placement";
        }

        if (((qpRank1 | qpRank8) & Pawn) != 0) {
          return "Invalid Pawn Placement";
        }

        var nPawns = (Int32)side.PieceCount(vP6);
        var nLimit = nFiles - nPawns;
        if (nLimit < 0) {
          return $"Too many {side.Parameter.SideName} Pawns";
        }

        //
        // Validate Piece Counts:
        //
        foreach (var p in Promotions) {
          var nSetup = p == Piece.Q ? 1 : 2;

          if (p == Piece.B)
            nSetup = hasBishopPair(side.FlagsSide) ? 2 : 1;

          var vPiece = pieceIndex((UInt32)p);
          var nCount = (Int32)side.PieceCount(vPiece);

          var nExtra = nCount - nSetup;
          if (nExtra > 0) nLimit -= nExtra;

          if (nLimit < 0)
            return $"Too many {side.Parameter.SideName} pieces";
        }
      }

      return default;
    }

    private void parsePlacement(Char cPlacement, ref Boolean wasDigit, ref Int32 x, Int32 y) {
      const Boolean ignoreCase = true;

      if (!IsDigit(cPlacement)) {
        wasDigit = false;
        var bWhiteSide = IsUpper(cPlacement);
        var piece = cPlacement.ToString().TryParseEnum<Piece>(ignoreCase);
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

    private void parseCastlingRights(String sCastleFlags) {
      foreach (var side in Side)
        side.ClearCanCastle();

      if (sCastleFlags != "-")
        grantCastlingRights(sCastleFlags);

      validateCastlingRights();

      foreach (var side in Side)
        side.HashCastlingRights();
    }

    //
    //[Chess960]Castle Rules are inferred from sCastleFlags.  These
    // present the only difference between Orthodox Chess and Chess960.
    //
    private void grantCastlingRights(string sCastleFlags) {
      if (sCastleFlags.Length > 4)
        throw new ParsePositionException($"Invalid Castling Flags = {sCastleFlags}");

      State!.IsChess960 = false;
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
          State!.IsChess960 = true;
          break;
        }

        var cPosLower = ToLower(cFlag);
        if (cPosLower < cFileMin || cFileMax < cPosLower)
          throw new ParsePositionException($"Unknown Castle Flag = {cFlag}");

        var nRookFile = cPosLower - cFileMin;
        var bWhiteSide = IsUpper(cFlag);
        var side = getSide(bWhiteSide);

        side.GrantCastling(nRookFile, State!.IsChess960);
      }                               //[Next]cFlag

      if (State!.IsChess960 && bOrthodoxFlags)
        throw new ParsePositionException("Mixed use of Chess 960 and Orthodox Castling Flags");
    }

    private void validateCastlingRights() {
      var blackSide = Side[Black];
      var whiteSide = Side[White];

      var blackRule = blackSide.Parameter.Rule;
      var whiteRule = whiteSide.Parameter.Rule;

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
      const Boolean ignoreCase = true;

      //
      //[Init]buildPawnAtx() is called here because Pawn[A1H8|A8H1]Atx
      // will be needed when tryEP() calls Friend.Passed().
      //
      buildPawnAtx();

      if (IsNullOrEmpty(sEnPassant) || sEnPassant == "-")
        return;

      var sqEnPassant = sEnPassant.TryParseEnum<sq>(ignoreCase);
      if (!sqEnPassant.HasValue)
        throw new ParsePositionException($"Invalid En Passant String = {sEnPassant}");

      // The destination square to which an e.p. capturing Pawn will move:
      var nEnPassant = (Int32)sqEnPassant;
      if (y(nEnPassant) != Friend.Parameter.EnPassantRank)
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
    protected Boolean ParsePosition(Scanner scanner, out String sPassed) {
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
      var sCastleFlags = scanner.HasTextSpan() ? scanner.Next() : "-";
      parseCastlingRights(sCastleFlags);

      //
      // 4. Square Passed for En Passant
      //
      sPassed = scanner.HasTextSpan() ? scanner.Next() : "-";
      return bWTM;
    }

    protected void Init(
      Boolean bWTM, String sEnPassant, String? sHalfMoveClock, String? sFullMoveNumber,
      Dictionary<String, List<String>?>? operations = default) {
      const string sFMVN = "Full Move Number";
      const string sHMVC = "Half Move Clock";
      // Preserve EPD Operations passed via ParseEPD()
      Operations = operations;

      //
      // FlagsTurn/FlagsSide bits outside of their respective Equal Masks were reset by pushRoot()
      //
      setWTM(bWTM);
      parsePassed(sEnPassant);
      Hash ^= hashFlags(bWTM);

      HalfMoveClock = ParseByte(sHMVC, sHalfMoveClock);

      if (IsPassed() && HalfMoveClock > 0) {
        var sqEP = (sq)ep(FlagsTurn);
        LogInfo(Level.warn, $"ep({sqEP}) implies {sHMVC} = {HalfMoveClock} Must Be Zero");
      }

      updateRepetitionCycle();

      var wMoveNumber = ParseUInt16(sFMVN, sFullMoveNumber);
      // Zero is sometimes used when the initial MoveNumber is unknown
      if (wMoveNumber == 0) wMoveNumber = 1;
      State!.MovePly = plyCount(wMoveNumber);
      if (!bWTM) State!.MovePly++;

      var sInvalid = IsValid();
      if (IsNullOrEmpty(sInvalid))
        initCastleRules();
      else {
        Display(sInvalid);
        throw new InvalidPositionException(sInvalid);
      }
    }

    private void initCastleRules() {
      foreach (var side in Side)
        side.Parameter.Rule.Init();
    }
    #endregion
  }
}
