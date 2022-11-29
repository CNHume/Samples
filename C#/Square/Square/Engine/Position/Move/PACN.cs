//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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
    internal const String sTextInsufficient = "draw by insufficient material to mate";
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
    private static sq parseSquare(String sMove, ref Int32 nPos, Int32 nLen) {
      const Boolean ignoreCase = true;
      sq? sq = default;
      if (nPos + 2 <= nLen) {
        sq = sMove.Substring(nPos, 2).TryParseEnum<sq>(ignoreCase);
        nPos += 2;
      }

      if (sq.HasValue)
        return sq.Value;
      else
        throw new MoveException($"Invalid Square: {sMove}");
    }

    private static Piece? parsePiece(String sMove, ref Int32 nPos, Int32 nLen) {
      const Boolean ignoreCase = true;
      return nPos < nLen ?
        sMove[nPos++].ToString().TryParseEnum<Piece>(ignoreCase) : default;
    }

    private void parsePACN(String sMove, out sq sqFrom, out sq sqTo, out Piece promotion) {
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

    private Int32 parseFromTo(String sMove, ref Boolean bCastles, ref Move move) {
      parsePACN(sMove, out sq sqFrom, out sq sqTo, out Piece promotion);
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
        throw new MoveException($"There is no piece to move from {sqFrom}");
      else if ((qpFrom & qpFriend) == 0) {
        var friendSideName = Friend.Parameter.SideName;
        var foeSideName = Foe.Parameter.SideName;
        var pieceFrom = indexPiece(vPieceFrom);
        throw new MoveException(
          $"{friendSideName} cannot move {foeSideName} {pieceFrom} from {sqFrom} to {sqTo}");
      }

      var vCapture = vPieceNull;
      var bCapture = (qpTo & qpFoe) != 0;
      if (bCapture)
        vCapture = vPieceTo;
      else if (vPieceFrom == vP6 && IsPassed() && nTo == FlagsTurn.ep()) {
        bCapture = true;
        vCapture = vEP6;
      }
      else if (vPieceFrom == vK6) {
        //
        //[Chess 960]OO/OOO notation is required in those cases where a King castles by
        // moving only one square.  However, cases where a King might otherwise be seen
        // as capturing its own Rook are assumed to be attempts to castle.  CanCastle()
        // will be called if needed, when this method returns.
        //
        var bUnambiguousRook = State!.IsChess960 && vPieceTo == vR6;
        var bUnambiguousKing = (KingAtx[nFrom] & qpTo) == 0;
        if (bUnambiguousRook || bUnambiguousKing) {
          var rule = Friend.Parameter.Rule;
          move = rule.Castles(nTo);
          if (move == Move.Undefined)
            throw new MoveException($"Illegal King Move: {sMove}");
          bCastles = true;
        }
      }

      if (!bCastles)
        move = Friend.BuildMove(
          sMove, sqFrom, sqTo, promotion, nFrom, nTo,
          qpTo, vPieceFrom, vCapture, bCapture);
      else if (promotion != Piece.None)
        throw new MoveException($"Cannot promote when castling: {sMove}");

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
    private Move parsePACNMove(String sMove) {
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
        nTo = parseFromTo(sMove, ref bCastles, ref move);

      //
      //[Chess 960]Validate Castling in common with the OO/OOO notation cases:
      //
      if (bCastles && !(nTo.HasValue && CanCastle(nTo.Value)))
        throw new MoveException($"Illegal Castle: {sMove}");
#if DebugParse
      var sb = new StringBuilder();
      sb.AppendPACN(move, Side, State!.IsChess960);
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
          var sMove = parser.PACNMoveToken.Value;
          var child = position.Push();  // See UCI.unmove()
          try {
            var move = position.parsePACNMove(sMove);
            if (child.tryOrSkip(ref move)) {
              position = child;
              position.setName();
            }
            else
              throw new MoveException($"Illegal Move: {sMove}");
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
            var sMove = pacnMoveToken.Value;
            var move = parsePACNMove(sMove);
            if (child.tryOrSkip(ref move))
              parseMoves.Add(move);
            else
              throw new MoveException($"Illegal Move: {sMove}");

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