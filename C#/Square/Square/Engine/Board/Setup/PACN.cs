﻿namespace Engine;

using Command.Exceptions;

partial class Board {
  #region Constants
  // Castling:
  internal const String sHyphenOO = "O-O";
  internal const String sHyphenOOO = "O-O-O";
  internal const String sPureOO = "OO";
  internal const String sPure00 = "00";
  internal const String sPureOOO = "OOO";
  internal const String sPure000 = "000";

  // Null Move:
  internal const String sNullMove = "0000";
  #endregion

  #region Pure Algebraic Coordinate Notation (PACN) Methods
  private static Sq parseSquare(String sMove, ref Int32 nPos, Int32 nLen) {
    const Boolean ignoreCase = true;
    Sq? result = default;
    if (nPos + 2 <= nLen) {
      result = sMove.Substring(nPos, 2).TryParseEnum<Sq>(ignoreCase);
      nPos += 2;
    }

    if (result.HasValue)
      return result.Value;
    else
      throw new MoveException($"Invalid Square: {sMove}");
  }

  private static Piece? parsePiece(String sMove, ref Int32 nPos, Int32 nLen) {
    const Boolean ignoreCase = true;
    return nPos < nLen ?
      sMove[nPos++].ToString().TryParseEnum<Piece>(ignoreCase) : default;
  }

  private void parsePACN(String sMove, out Sq sqFrom, out Sq sqTo, out Piece promotion) {
    promotion = default;
    var nLen = sMove.Length;
    var nPos = 0;
    sqFrom = parseSquare(sMove, ref nPos, nLen);
    sqTo = parseSquare(sMove, ref nPos, nLen);
    var piece = parsePiece(sMove, ref nPos, nLen);
    if (piece.HasValue) {
      if (Promotions.Any(p => p == piece.Value))
        promotion = piece.Value;
      else
        //[Safe]pacnMoveTokenRules should prevent an Invalid Promotion Piece:
        throw new MoveException($"Invalid Promotion Piece: {piece.Value}");
    }
  }

  private Int32 parseFromTo(UInt16 wGamePly, String sMove, ref Boolean bCastles, ref Move move) {
    parsePACN(sMove, out Sq sqFrom, out Sq sqTo, out Piece promotion);
    var nFrom = (Int32)sqFrom;
    var nTo = (Int32)sqTo;
    var qpFrom = bit(nFrom);
    var qpTo = bit(nTo);
    var vPieceFrom = GetPieceIndex(nFrom);
    var vPieceTo = GetPieceIndex(nTo);

    var qpFriend = Friend.Piece;
    var qpFoe = Foe.Piece;

    var wMove = MoveNumber(wGamePly);
    var friendSideName = Friend.Parameter.SideName;
    var foeSideName = Foe.Parameter.SideName;

    // Validate Piece Color
    if ((qpFrom & RankPiece) == 0)
      throw new MoveException($"Move {wMove}: There is no {friendSideName} piece to move from {sqFrom}.");
    else if ((qpFrom & qpFriend) == 0) {
      var pieceFrom = IndexPiece(vPieceFrom);
      throw new MoveException(
        $"Move {wMove}: {friendSideName} cannot move {foeSideName} {pieceFrom} from {sqFrom} to {sqTo}.");
    }

    var vCapture = vPieceNull;
    var bCapture = (qpTo & qpFoe) != 0;
    if (bCapture)
      vCapture = vPieceTo;
    else if (vPieceFrom == vP6 && IsPassed() && nTo == FlagsTurn.sqrEP()) {
      bCapture = true;
      vCapture = vEP6;
    }
    else if (vPieceFrom == vK6) {
      //
      //[Chess960]OO/OOO notation is required in those cases where a King castles by
      // moving only one square.  However, cases where a King might otherwise be seen
      // as capturing its own Rook are assumed to be attempts to castle.  CanCastle()
      // will be called if needed, when this method returns.
      //
      var bUnambiguousRook = State.IsChess960 && vPieceTo == vR6;
      var bUnambiguousKing = (AtxKing[nFrom] & qpTo) == 0;
      if (bUnambiguousRook || bUnambiguousKing) {
        var rule = Friend.Parameter.Rule;
        move = rule.Castles(nTo);
        if (move == Move.Undefined)
          throw new MoveException($"Move {wMove}: {sMove} is an Illegal King Move.");
        bCastles = true;
      }
    }

    if (!bCastles)
      move = Friend.BuildMove(
        wGamePly, sMove, sqFrom, sqTo, promotion, nFrom, nTo,
        qpTo, vPieceFrom, vCapture, bCapture);
    else if (promotion != Piece.None)
      throw new MoveException($"Move {wMove}: {friendSideName} cannot promote when castling.");

    return nTo;
  }

  //
  // UCI moves are expressed in Pure Algebraic Coordinate Notation (PACN):
  // See https://www.chessprogramming.org/Algebraic_Chess_Notation
  //
  // This is simpler and more compact than the reversible computer notation
  // devised by [Warren] Smith.  UCI documentation incorrectly implies that
  //"The move format is in long algebraic notation."
  //
  protected Move ParsePACNMove(UInt16 wGamePly, String sMove) {
    var sUpperMove = sMove.ToUpper();
    var rule = Friend.Parameter.Rule;

    var bCastles = false;
    Int32? nTo = default;
    var move = Move.Undefined;
    if (sUpperMove == sPureOO || sUpperMove == sPure00) {
      bCastles = true;
      nTo = rule.KingOOTo;
      move = rule.OO;
    }
    else if (sUpperMove == sPureOOO || sUpperMove == sPure000) {
      bCastles = true;
      nTo = rule.KingOOOTo;
      move = rule.OOO;
    }
    else if (sUpperMove == sNullMove) //[UCI]
      move = Move.NullMove;
    else                              //[PACN]
      nTo = parseFromTo(wGamePly, sMove, ref bCastles, ref move);

    //
    //[Chess960]Validate Castling in common with the use of OO/OOO notation:
    //
    if (bCastles && !(nTo.HasValue && CanCastle(nTo.Value)))
      throw new MoveException($"Illegal Castle: {sMove}");
#if DebugParse
      var sb = new StringBuilder();
      sb.AppendPACN(move, Side, State.IsChess960);
      sb.FlushLine();
#endif
    return move;
  }
  #endregion                            // Pure Algebraic Coordinate Notation (PACN) Methods
}
