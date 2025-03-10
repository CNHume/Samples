﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2011-07-05 CNHume]Created File
//
// Conditionals:
//
//#define DebugCastlingRights
//#define EnsureFromSquares
#define HashCastlingRights
//#define Magic
#define ShowEPIllegal

using System.Text;

using static System.Char;
using static System.String;

namespace Engine;

using Commands;                          // For Scanner

using Exceptions;

using static Commands.Parser;
using static Logging.Logger;

//
// Type Aliases:
//
partial class Board {
  #region Board Setup
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

  private void parsePiecePlacements(String? sPlacements) {
    if (sPlacements == null)
      throw new ParsePositionException("No piece placements found");

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
      var wasDigit = false;             // Disallow double digits

      for (var x = 0; x < nFiles; nPos++) {
        if (nPos < nLength)
          parsePlacement(sRank[nPos], ref wasDigit, ref x, y);
        else
          throw new ParsePositionException($"Too few characters for rank {y + 1}");
      }                                 //[Next]Char

      if (nPos < nLength)
        throw new ParsePositionException($"Too many characters for rank {y + 1}");
    }                                   //[Next]Rank
  }

  private static Boolean parseWTM(String? sToMove) {
    return sToMove?.ToLower() switch {
      "b" => false,
      "w" => true,
      _ => throw new ParsePositionException($"Unknown Side To Move = {sToMove}")
    };
  }

  //
  //[Chess960]Castle Rules are inferred from sCastleFlags.  These
  // present the only difference between Orthodox Chess and Chess960.
  //
  private void parseCastlingFlags(String? sCastleFlags, List<Byte> rookFromSquares) {
    if (sCastleFlags == null)
      throw new ParsePositionException($"No Castling Flags Found");

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
      var vRookFrom = (Byte)sqr(nRookFile, pieceRank);
#if DebugCastlingRights
        var sqRookFrom = (Sq)vRookFrom;
        var sideName = side.Parameter.SideName;
#endif
      rookFromSquares.Add(vRookFrom);
    }                                   //[Next]cFlag

    if (bChess960Flags && bOrthodoxFlags)
      throw new ParsePositionException("Mixed use of Chess 960 and Orthodox Castling Flags");
  }

  private void buildPawnAtx() {
    foreach (var side in Side)
      side.ResetPawnAtx();
  }

  private void parsePassed(String? sEnPassant) {
    const Boolean ignoreCase = true;

    //
    //[Init]Define Pawn[A1H8|A8H1]Atx for Friend.EPGuard():
    //
    buildPawnAtx();

    if (IsNullOrEmpty(sEnPassant) || sEnPassant == "-")
      return;

    var sqEP = sEnPassant.TryParseEnum<Sq>(ignoreCase);
    if (!sqEP.HasValue)
      throw new ParsePositionException($"Invalid En Passant String = {sEnPassant}");

    // The destination square to which an e.p. capturing Pawn will move:
    var vEPTarget = (Byte)sqEP;
    if (y(vEPTarget) != Foe.Parameter.PassRank)
      throw new ParsePositionException($"Invalid Rank for En Passant Games: {sqEP}");

    // The square actually holding the e.p. Pawn to be captured:
    var nMovedTo = vEPTarget + Foe.Parameter.PawnStep;

    //
    // Both squares behind nTo must be vacant, nTo must hold a Pawn,
    // and an EPGuard must be present.
    //
    var nStart = vEPTarget + Friend.Parameter.PawnStep;
    var qpStart = bit(nStart);
    var qpEnPassant = bit(vEPTarget);
    var qpVacant = qpStart | qpEnPassant;
    var bInvalid =
      (qpVacant & RankPiece) != 0 ||
      (Foe.Piece & Pawn & bit(nMovedTo)) == 0 ||
      Friend.EPGuard(vEPTarget) == 0;

    if (bInvalid)
      throw new ParsePositionException($"Invalid En Passant Games: {sqEP}");

    tryEP(vEPTarget);
#if ShowEPIllegal
    if (!IsEPLegal())
      LogInfo(LogLevel.note, $"En Passant Illegal: {sqEP}");
#endif                                  // ShowEPIllegal
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
    Scanner scanner, List<Byte> rookFromSquares, out String? sPassed) {
    // Clear() should have been performed by the Push() in NewGame()
    //[Debug]Clear();

    //
    // 1. Scanner Pieces by Rank
    //
    var sPlacements = scanner.HasTextSpan() ? scanner.Next() : Empty;
    parsePiecePlacements(sPlacements);

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
    // 4. Games Passed for En Passant
    //
    sPassed = scanner.HasTextSpan() ? scanner.Next() : "-";
    return bWTM;
  }

  protected void Setup(
    Boolean bWTM, List<Byte> rookFromSquares,
    String? sEnPassant, String? sHMVCValue, String? sFMVNValue,
    Dictionary<String, List<String>?>? operations = default) {
    const String sFMVNName = "Full Move Number";
    const String sHMVCName = "Half Move Clock";

    #region EPD Operations
    Operations = operations;
    #endregion

    //
    // Outside of their Equal Masks, FlagsTurn and FlagsSide
    // were reset by pushRoot()
    //
    setSides(bWTM);

    #region EnPassant
    parsePassed(sEnPassant);
    hashFlags(bWTM, ref Hash);          // Including epHash()
    #endregion                          // EnPassant

    #region Init Castling
    initCastling(rookFromSquares);
    #endregion                          // Init Castling

    #region Half Move Clock and Full Move Number
    HalfMoveClock = ParseByte(sHMVCName, sHMVCValue);

    if (HalfMoveClock > 0 && EPTarget.HasValue) {
      var sq = (Sq)EPTarget;
      LogInfo(LogLevel.warn, $"ep({sq}) implies {sHMVCName} = {HalfMoveClock} Must Be Zero");
    }

    //
    // SetDraw0 unnecessary here because Parent is null for the Root Position.
    //

    // SearchPly is defined wrt MovePly
    var wMoveNumber = ParseUInt16(sFMVNName, sFMVNValue);
    State.MovePly = ply(wMoveNumber, bWTM);
    #endregion                          // Half Move Clock and Full Move Number

    Validate();
    initCastleRules();
  }

  private void validateCastlingSymmetry() {
    var (blackSide, whiteSide) = Side.GetBothSides();
    var blackRule = blackSide.Parameter.Rule;
    var whiteRule = whiteSide.Parameter.Rule;

    if (blackRule.CastlesFrom.HasValue && whiteRule.CastlesFrom.HasValue) {
      if (x(blackRule.CastlesFrom.Value) != x(whiteRule.CastlesFrom.Value))
        throw new InvalidPositionException("Kings must castle from the same file");
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
        throw new InvalidPositionException("Rooks must OO from the same file");
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
        throw new InvalidPositionException("Rooks must OOO from the same file");
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

  private Position.PositionSide findCastleFromSide(Int32 nFrom) {
    foreach (var side in Side)
      if (side.Parameter.PieceRank == y(nFrom))
        return side;

    var vPiece = GetPieceIndex(nFrom);
    var piece = IndexPiece(vPiece);
    var sqFrom = (Sq)nFrom;
    throw new ParsePositionException($"Side not found for {piece} castling from {sqFrom}");
  }

  /*
   * To build Castling Rules for Root Position:
   *
   * side.FlagsSide = default           // Clear Castling Rights from Position.Clear()
   * side.ClearCastleRule()
   * rookFromSquares.Add(vRookFrom)     // Add rookFromSquares per parseCastlingFlags(sCastleFlags)
   * side.GrantCastling(vRookFrom)      // Set FromSquares and CastlingRights
   * side.HashCastlingRights()
   * validateCastlingSymmetry()         // Verify FromSquares
   * isChess960()
   * side.InitCastleRule()              // Set OOO and OO rules
   */
  private void initCastling(List<Byte> rookFromSquares) {
    //[Test]rookFromSquares.Sort();
    foreach (var vRookFrom in rookFromSquares) {
      var side = findCastleFromSide(vRookFrom);
      side.GrantCastling(vRookFrom);
    }

    foreach (var side in Side)
      side.HashCastlingRights();

    validateCastlingSymmetry();

    State.IsChess960 = isChess960();
  }

  private Boolean isChess960() {
    foreach (var side in Side)
      if (!side.Parameter.Rule.IsOrthodoxCastling())
        return true;

    return false;
  }

  protected void ClearCastleRules() {
    foreach (var side in Side)
      side.ClearCastleRule();
  }

  private void initCastleRules() {
    foreach (var side in Side)
      side.InitCastleRule();
  }
  #endregion                            // Board Setup
}
