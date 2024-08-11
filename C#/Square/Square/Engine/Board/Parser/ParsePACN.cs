using static System.StringComparison;

namespace Engine;

using Exceptions;

using static Extension;

partial class Board {
  #region Pure Algebraic Coordinate Notation (PACN) Methods
  private Sq parseSquare(String sMove, ref Int32 nPos, Int32 nLen) {
    const Boolean ignoreCase = true;
    Sq? result = default;
    var nPos2 = nPos + 2;
    if (nPos2 <= nLen) {
      result = sMove[nPos..nPos2].TryParseEnum<Sq>(ignoreCase);
      nPos = nPos2;
    }

    if (result.HasValue)
      return result.Value;

    //[Safe]pacnMoveTokenRules should have prevented any Invalid Square
    throw new MoveException(
      Friend.MoveError($"Invalid Square in {sMove}"));
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
      if (!Promotions.Any(p => p == piece.Value))
        //[Safe]pacnMoveTokenRules should have prevented any Invalid Promotion Piece
        throw new MoveException(
          Friend.MoveError($"Invalid Promotion Piece {piece.Value} in {sMove}"));

      promotion = piece.Value;
    }
  }

  private Int32 parseFromTo(String sMove, ref Boolean bCastles, ref Move move) {
    parsePACN(sMove, out Sq sqFrom, out Sq sqTo, out Piece promotion);
    var nFrom = (Int32)sqFrom;
    var qpFrom = bit(nFrom);
    var vPieceFrom = GetPieceIndex(nFrom);

    // Validate Piece Color
    if ((qpFrom & RankPiece) == 0)
      throw new MoveException(
        Friend.MoveError($"There is no piece to move", sqFrom, sqTo));

    if ((qpFrom & Friend.Piece) == 0) {
      var foeSideName = Foe.Parameter.SideName;
      var pieceFrom = IndexPiece(vPieceFrom);
      throw new MoveException(
        Friend.MoveError($"Cannot move {foeSideName} {pieceFrom}", sqFrom, sqTo));
    }

    var nTo = (Int32)sqTo;
    var qpTo = bit(nTo);
    var vPieceTo = GetPieceIndex(nTo);

    var vCapture = vPieceNull;
    var bCapture = (qpTo & Foe.Piece) != 0;
    if (bCapture)
      vCapture = vPieceTo;
    else if (vPieceFrom == vP6 &&
             IsEPLegal() &&
             nTo == EPTarget) {
      bCapture = true;
      vCapture = vEP6;
    }
    else if (vPieceFrom == vK6) {
      //
      //[Chess960]If a King is able to castle by moving one square, OO/OOO
      // notation must be used to avoid ambiguity with a non-castling move.
      //
      // However, moves where a King lands on a square occupied by its own
      // Rook are assumed to be attempts to castle.
      //
      // CanCastle() is called if needed, when this method returns.
      //
      var bKingJump = (qpTo & AtxKing[nFrom]) == 0;
      var bRookSwap = State.IsChess960 && vPieceTo == vR6;
      if (bKingJump || bRookSwap) {
        var rule = Friend.Parameter.Rule;
        move = rule.Castles(nTo);
        if (IsUndefined(move))
          throw new MoveException(
            Friend.MoveError($"Illegal King Move", sqFrom, sqTo));
        bCastles = true;
      }
    }

    if (!bCastles)
      move = Friend.BuildMove(
        sMove, promotion, nFrom, nTo,
        qpTo, vPieceFrom, vCapture, bCapture);
    else if (promotion != Piece.None)
      throw new MoveException(
        Friend.MoveError($"Promotion Illegal in King Move", sqFrom, sqTo));

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
  protected Move ParsePACNMove(String sMove) {
    var move = Move.Undefined;
    Int32? nTo = default;
    var bCastles = false;
    var rule = Friend.Parameter.Rule;

    if (sMove.Equals(sPureOO, InvariantCultureIgnoreCase) ||
        sMove == sPure00) {
      bCastles = true;
      nTo = rule.KingOOTo;
      move = rule.OOMove;
    }
    else if (sMove.Equals(sPureOOO, InvariantCultureIgnoreCase) ||
             sMove == sPure000) {
      bCastles = true;
      nTo = rule.KingOOOTo;
      move = rule.OOOMove;
    }
    else if (sMove == sNullMove)        //[UCI]
      move = Move.NullMove;
    else                                //[PACN]
      nTo = parseFromTo(sMove, ref bCastles, ref move);

    //
    //[Chess960]Validate Castling:
    //
    if (bCastles && !(nTo.HasValue && CanCastle(nTo.Value)))
      throw new MoveException(
        Friend.MoveError($"Illegal Castling in {sMove}"));

    return move;
  }
  #endregion                            // Pure Algebraic Coordinate Notation (PACN) Methods
}
