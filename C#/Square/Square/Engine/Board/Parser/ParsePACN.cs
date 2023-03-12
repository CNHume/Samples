namespace Engine;

using Command.Exceptions;

using static Engine.Board;
using static Extension;

partial class Board {
  #region Pure Algebraic Coordinate Notation (PACN) Methods
  private Sq parseSquare(UInt16 wGamePly, String sMove, ref Int32 nPos, Int32 nLen) {
    const Boolean ignoreCase = true;
    Sq? result = default;
    if (nPos + 2 <= nLen) {
      result = sMove.Substring(nPos, 2).TryParseEnum<Sq>(ignoreCase);
      nPos += 2;
    }

    if (result.HasValue)
      return result.Value;

    //[Safe]pacnMoveTokenRules should have prevented any Invalid Square
    throw new MoveException(
      Friend.MoveError(wGamePly, $"Invalid Square in {sMove}"));
  }

  private static Piece? parsePiece(String sMove, ref Int32 nPos, Int32 nLen) {
    const Boolean ignoreCase = true;
    return nPos < nLen ?
      sMove[nPos++].ToString().TryParseEnum<Piece>(ignoreCase) : default;
  }

  private void parsePACN(
    UInt16 wGamePly, String sMove, out Sq sqFrom, out Sq sqTo, out Piece promotion) {
    promotion = default;
    var nLen = sMove.Length;
    var nPos = 0;
    sqFrom = parseSquare(wGamePly, sMove, ref nPos, nLen);
    sqTo = parseSquare(wGamePly, sMove, ref nPos, nLen);
    var piece = parsePiece(sMove, ref nPos, nLen);
    if (piece.HasValue) {
      if (!Promotions.Any(p => p == piece.Value))
        //[Safe]pacnMoveTokenRules should have prevented any Invalid Promotion Piece
        throw new MoveException(
          Friend.MoveError(wGamePly, $"Invalid Promotion Piece {piece.Value} in {sMove}"));

      promotion = piece.Value;
    }
  }

  private Int32 parseFromTo(
    UInt16 wGamePly, String sMove, ref Boolean bCastles, ref Move move) {
    parsePACN(wGamePly, sMove, out Sq sqFrom, out Sq sqTo, out Piece promotion);
    var nFrom = (Int32)sqFrom;
    var nTo = (Int32)sqTo;
    var qpFrom = bit(nFrom);
    var qpTo = bit(nTo);
    var vPieceFrom = GetPieceIndex(nFrom);
    var vPieceTo = GetPieceIndex(nTo);

    var qpFriend = Friend.Piece;
    var qpFoe = Foe.Piece;

    // Validate Piece Color
    if ((qpFrom & RankPiece) == 0)
      throw new MoveException(
        Friend.MoveError(wGamePly, $"There is no piece to move from {sqFrom} to {sqTo}"));
    else if ((qpFrom & qpFriend) == 0) {
      var foeSideName = Foe.Parameter.SideName;
      var pieceFrom = IndexPiece(vPieceFrom);
      throw new MoveException(
        Friend.MoveError(wGamePly, $"Cannot move {foeSideName} {pieceFrom} from {sqFrom} to {sqTo}"));
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
          throw new MoveException(
            Friend.MoveError(wGamePly, $"Illegal King Move from {sqFrom} to {sqTo}"));
        bCastles = true;
      }
    }

    if (!bCastles)
      move = Friend.BuildMove(
        wGamePly, sMove, sqFrom, sqTo, promotion, nFrom, nTo,
        qpTo, vPieceFrom, vCapture, bCapture);
    else if (promotion != Piece.None)
      throw new MoveException(
        Friend.MoveError(wGamePly, $"Illegal Promotion in King Move from {sqFrom} to {sqTo}"));

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
    if (sUpperMove == sPureOO ||
        sUpperMove == sPure00) {
      bCastles = true;
      nTo = rule.KingOOTo;
      move = rule.OO;
    }
    else if (sUpperMove == sPureOOO ||
             sUpperMove == sPure000) {
      bCastles = true;
      nTo = rule.KingOOOTo;
      move = rule.OOO;
    }
    else if (sUpperMove == sNullMove)   //[UCI]
      move = Move.NullMove;
    else                                //[PACN]
      nTo = parseFromTo(wGamePly, sMove, ref bCastles, ref move);

    //
    //[Chess960]Validate Castling:
    //
    if (bCastles && !(nTo.HasValue && CanCastle(nTo.Value)))
      throw new MoveException(
        Friend.MoveError(wGamePly, $"Illegal Castling in {sMove}"));

    return move;
  }
  #endregion                            // Pure Algebraic Coordinate Notation (PACN) Methods
}
