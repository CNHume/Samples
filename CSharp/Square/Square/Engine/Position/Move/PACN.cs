//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
//#define DebugParse

namespace Engine {
  using Command;                        // For Scanner, Token
  using Command.Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class Position : Board {
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

    // User Friendly Algebraic Notation:
    internal const String sTakes = "x";
    internal const String sMoves = "-";

    // Evaluations:
    internal const String sEvalMinus = "-";
    internal const String sEvalMovesToMate = "#";
    internal const String sEvalUCIMovesToMate = "mate ";
    internal const String sEvalUCICentiPawns = "cp ";

    internal const String sEvalUndefined = "Undefined";
    internal const String sEvalInfinity = "Infinity";
    internal const String sEvalOverflow = "Overflow";

    // Move Annotatons:
    internal const String sNotePromotion = "=";
    internal const String sTextStalemate = "stalemate";
    internal const String sTextCheckmate = "checkmate";
    internal const String sNoteCheckmate = "#";
    internal const String sNoteCheck = "+";
    internal const String sNoteDraw2 = "@";
    internal const String sNoteDraw = "=";

    // Move Numbering:
    internal const String sElipsis = "...";
    internal const String sMoveNumber = ".";

    // Quiescent Move Sequence:
    internal const String sQxntOpen = "[";
    internal const String sQxntClose = "]";
    #endregion

    #region Pure Algebraic Coordinate Notation (PACN) Methods
    private static sq? parseSquare(String sMove, ref Int32 nPos, Int32 nLen) {
      sq? sq = default;
      if (nPos + 2 <= nLen) {
        sq = TryParseSquare(sMove.Substring(nPos, 2));
        nPos += 2;
      }
      return sq;
    }

    private static Piece? parsePiece(String sMove, ref Int32 nPos, Int32 nLen) {
      return nPos < nLen ?
        TryParsePiece(sMove[nPos++].ToString()) : default;
    }

    private Boolean parsePACN(String sMove, out sq? sqFrom, out sq? sqTo, out Piece promotion) {
      var nLen = sMove.Length;
      var nPos = 0;
      sqFrom = parseSquare(sMove, ref nPos, nLen);
      sqTo = parseSquare(sMove, ref nPos, nLen);
      if (sqFrom.HasValue && sqTo.HasValue) {
        var piece = parsePiece(sMove, ref nPos, nLen);
        if (!piece.HasValue) {
          promotion = Piece.None;
          return true;
        }
        else if (Promotions.Any(p => p == piece.Value)) {
          promotion = piece.Value;
          return true;
        }

        //[Safe]pacnMoveTokenRules should prevent an Invalid Promotion Piece:
        throw new MoveException($"Invalid Promotion Piece: {piece.Value}");
      }

      promotion = Piece.None;
      return false;
    }

    private Move buildMove(String sPACN, sq? sqFrom, sq? sqTo, Piece promotion,
                           Int32 nFrom, Int32 nTo, Plane qpTo, Byte vPiece,
                           Plane qpFriend, Byte vCapture, Boolean bCapture) {
      //
      // Validate Non-Castling Move
      //
      var qpPieceAtx = pieceAtx(vPiece, nFrom, bCapture);
      var piece = indexPiece(vPiece);

      if (!qpPieceAtx.HasValue)         //[Safe]
        throw new ParseException($"Unexpected Piece in Move: {sPACN}");
      else {
        qpPieceAtx &= ~qpFriend;
        if ((qpPieceAtx & qpTo) == 0)
          throw new MoveException($"{piece} cannot move from {sqFrom} to {sqTo}");
      }

      //
      // Validate Promotion
      //
      var bLastRank = Side.Any(side => (side.Parameter.RankLast & qpTo) != 0);
      var bRequired = vPiece == vP6 && bLastRank;
      var bSupplied = promotion != Piece.None;
      if (bRequired != bSupplied) {
        var sDiagnosis = bRequired ? "Required" : "Illegal";
        throw new MoveException($"Promotion Piece {sDiagnosis} for {sPACN}");
      }

      var move = promotionMove(promotion) | pieceMove(piece) | fromToMove(nFrom, nTo);
      if (bCapture) move |= captureMove(indexPiece(vCapture));
      return move;
    }

    private Int32 parseFromTo(String sPACN, ref Boolean bCastles, ref Move move) {
      var bWTM = WTM();
      if (!parsePACN(sPACN, out sq? sqFrom, out sq? sqTo, out Piece promotion))
        throw new MoveException($"Invalid Move: {sPACN}");

      var nFrom = (Int32)sqFrom;
      var nTo = (Int32)sqTo;
      var qpFrom = BIT0 << nFrom;
      var qpTo = BIT0 << nTo;
      var vPieceFrom = getPieceIndex(nFrom);
      var vPieceTo = getPieceIndex(nTo);

      (BoardSide friend, BoardSide foe) = getSides(bWTM);
      var qpFriend = friend.Piece;
      var qpFoe = foe.Piece;

      // Validate Piece Color
      if ((qpFrom & RankPiece) == 0)
        throw new MoveException($"There is no piece to move from {sqFrom}");
      else if ((qpFrom & qpFriend) == 0) {
        var friendSideName = friend.Parameter.SideName;
        var foeSideName = foe.Parameter.SideName;
        var pieceFrom = indexPiece(vPieceFrom);
        throw new MoveException(
          $"{friendSideName} cannot move {foeSideName} {pieceFrom} from {sqFrom} to {sqTo}");
      }

      var castle = State.Rule;
      var vCapture = vPieceNull;
      var bCapture = (qpTo & qpFoe) != 0;
      if (bCapture)
        vCapture = vPieceTo;
      else if (vPieceFrom == vP6 && IsPassed() && nTo == ep(FlagsLo)) {
        bCapture = true;
        vCapture = vEP6;
      }
      else if (vPieceFrom == vK6) {
        //
        //[Chess 960]OO/OOO notation is required in those cases where a King castles by
        // moving only one square.  However, cases where a King might otherwise be seen
        // as capturing its own Rook are assumed to be attempts to castle.  canCastle()
        // will be called if needed, when this method returns.
        //
        var bUnambiguousRook = castle.IsChess960 && vPieceTo == vR6;
        var bUnambiguousKing = (KingAtx[nFrom] & qpTo) == 0;
        if (bUnambiguousRook || bUnambiguousKing) {
          var rule = getRule(bWTM);
          move = rule.Castles(nTo);
          if (move == Move.Undefined)
            throw new MoveException($"Illegal King Move: {sPACN}");
          bCastles = true;
        }
      }

      if (!bCastles)
        move = buildMove(sPACN, sqFrom, sqTo, promotion,
                         nFrom, nTo, qpTo, vPieceFrom,
                         qpFriend, vCapture, bCapture);
      else if (promotion != Piece.None)
        throw new MoveException($"Cannot promote when castling: {sPACN}");

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
    protected Move parsePACNMove(String sPACN) {
      var sMove = sPACN.ToUpper();
      var bWTM = WTM();
      var rule = getRule(bWTM);

      var bCastles = false;
      Int32? nTo = default;
      var move = Move.Undefined;
      if (sMove == sPureOO || sMove == sPure00) {
        bCastles = true;
        nTo = rule.KingOOTo;
        move = rule.OO;
      }
      else if (sMove == sPureOOO || sMove == sPure000) {
        bCastles = true;
        nTo = rule.KingOOOTo;
        move = rule.OOO;
      }
      else if (sMove == sNullMove)      //[UCI]
        move = Move.NullMove;
      else                              //[PACN]
        nTo = parseFromTo(sPACN, ref bCastles, ref move);

      //
      //[Chess 960]Validate Castling in common with the OO/OOO notation cases:
      //
      if (bCastles && !canCastle(bWTM, nTo.Value))
        throw new MoveException($"Illegal Castle: {sPACN}");
#if DebugParse
      var sb = new StringBuilder();
      sb.AppendPACN(move, castle.IsChess960);
      sb.FlushLine();
#endif
      return move;
    }

    //
    // The following advances through a line of PACN moves, pushing each position; and returns the final position
    //
    public Position ParsePACNMakeMoves(Parser parser) {
      var position = this;

      if (parser.SpaceToken.Accept()) {
        //
        // Make each move sequentially, returning the final position
        //
        while (parser.PACNMoveToken.Accept()) {
          var sPACN = parser.PACNMoveToken.Value;
          var child = position.Push();  // See UCI.unmove()
          try {
            var move = position.parsePACNMove(sPACN);
            if (child.tryOrSkip(ref move)) {
              position = child;
              position.setName();
            }
            else
              throw new MoveException($"Illegal Move: {sPACN}");
          }
          catch {
            // Reclaim *last* child if parsePACNMove() should fail to complete normally
            Pop(ref child);
            throw;
          }

          if (!parser.SpaceToken.Accept()) break;
        }
      }

      return position;
    }

    //
    // The following parses a line of PACN move alternatives at the current position
    //
    public void ParsePACNSearchMoves(List<Move> searchMoves, Token spaceToken, Token pacnMoveToken) {
      searchMoves.Clear();
      var parseMoves = new List<Move>();

      if (spaceToken.Accept()) {
        var child = Push();
        try {
          while (pacnMoveToken.Accept()) {
            //
            // Parse alternative moves, wrt the current position,
            // without actually making the moves.
            //
            var sPACN = pacnMoveToken.Value;
            var move = parsePACNMove(sPACN);
            if (child.tryOrSkip(ref move))
              parseMoves.Add(move);
            else
              throw new MoveException($"Illegal Move: {sPACN}");

            if (!spaceToken.Accept()) break;
          }
        }
        finally {
          // Reclaim child if parsePACNMove() should fail to complete normally
          Pop(ref child);
        }
      }

      searchMoves.AddRange(parseMoves.Distinct());
      if (searchMoves.Count == 0)
        throw new MoveException("No Search Move specified");
    }
    #endregion
  }
}