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
    protected Boolean parsePACN(String sMove, out sq? sqFrom, out sq? sqTo, out Piece promotion) {
      promotion = Piece._;

      var sFrom = (String)null;
      var sTo = (String)null;

      var nLen = sMove.Length;
      var nPos = 0;

      if (nPos + 2 <= nLen) {
        sFrom = sMove.Substring(nPos, 2);
        nPos += 2;
      }

      if (nPos + 2 <= nLen) {
        sTo = sMove.Substring(nPos, 2);
        nPos += 2;
      }

      sqFrom = TryParseSquare(sFrom);
      sqTo = TryParseSquare(sTo);
      var cPromotion = nPos < nLen ? sMove[nPos++] : cSpace;
      var bValid = nPos == nLen && sqFrom.HasValue && sqTo.HasValue;

      switch (Char.ToUpper(cPromotion)) {
      case 'R':
        promotion = Piece.R;
        break;
      case 'N':
        promotion = Piece.N;
        break;
      case 'B':
        promotion = Piece.B;
        break;
      case 'Q':
        promotion = Piece.Q;
        break;
      case cSpace:
        break;
      default:
        bValid = false;
        break;
      }

      return bValid;
    }

    private Move buildMove(String sPACN, sq? sqFrom, sq? sqTo, Piece promotion, Int32 nFrom, Int32 nTo, Plane qpTo,
                           Byte vPiece, Plane qpFriend, Byte vCapture, Boolean bCapture) {
      //
      // Validate Non-Castling Move
      //
      var qpPieceAtx = pieceAtx(vPiece, nFrom, bCapture);
      var piece = (Piece)(vPiece + vFirst);

      if (!qpPieceAtx.HasValue)         //[Safe]
        throw new ParseException($"Unexpected Piece in move: {sPACN}");
      else {
        qpPieceAtx &= ~qpFriend;
        if ((qpPieceAtx & qpTo) == 0)
          throw new MoveException($"{piece} cannot move from {sqFrom} to {sqTo}");
      }

      //
      // Validate Promotion
      //
      var bPromote = Side.Any(side => (side.RankLast & qpTo) != 0);
      var bRequired = vPiece == vP6 && bPromote;
      var bSupplied = promotion != Piece._;
      if (bRequired != bSupplied) {
        var sDiagnosis = bRequired ? "Promotion Required: " : "Invalid Promotion: ";
        throw new MoveException(sDiagnosis + sPACN);
      }

      var move = (Move)(((UInt32)piece << nPieceBit) |
                        ((UInt32)promotion << nPromoteBit) |
                        ((UInt32)nTo << nToBit) |
                        ((UInt32)nFrom << nFromBit));

      if (bCapture)
        move |= (Move)((UInt32)(vCapture + vFirst) << nCaptiveBit);

      return move;
    }

    private Int32 parseFromTo(String sPACN, ref Boolean bCastles, ref Move move) {
      var bWTM = WTM();
      if (!parsePACN(sPACN, out sq? sqFrom, out sq? sqTo, out Piece promotion))
        throw new MoveException($"Invalid move: {sPACN}");

      var nFrom = (Int32)sqFrom;
      var nTo = (Int32)sqTo;
      var qpFrom = BIT0 << nFrom;
      var qpTo = BIT0 << nTo;
      var vPieceFrom = getPiece(nFrom);
      var vPieceTo = getPiece(nTo);

      (BoardSide friend, BoardSide foe) = getSides(bWTM);
      var qpFriend = friend.Piece;
      var qpFoe = foe.Piece;

      // Validate Piece Color
      if ((qpFrom & RankPiece) == 0)
        throw new MoveException($"There is no piece to move from {sqFrom}");
      else if ((qpFrom & qpFriend) == 0) {
        var piece = (Piece)(vPieceFrom + vFirst);
        throw new MoveException($"{friend.SideName} cannot move {foe.SideName} {piece} from {sqFrom}");
      }

      var castle = State.Rule;
      var vCapture = vPieceNull;
      var bCapture = (qpTo & qpFoe) != 0;
      if (bCapture)
        vCapture = vPieceTo;
      else if (vPieceFrom == vP6) {
        if (IsPassed() && nTo == ep(FlagsLo)) {
          bCapture = true;
          vCapture = vEP6;
        }
      }
      else if (vPieceFrom == vK6 && ((KingAtx[nFrom] & qpTo) == 0 ||
                                 castle.IsChess960 && vPieceTo == vR6)) {
        //
        //[Chess 960]OO/OOO notation is required in those cases where a King castles by
        // moving only one square.  However, cases where a King would otherwise need to
        // be seen as capturing its own Rook will instead be assumed attempts to castle.
        // canCastle() will be called as needed, when this method returns.
        //
        var rule = getRule(bWTM);
        move = rule.Castles(nTo);
        if (move == Move.Undefined)
          throw new MoveException($"Illegal King Move: {sPACN}");
        bCastles = true;
      }

      if (!bCastles)
        move = buildMove(sPACN, sqFrom, sqTo, promotion, nFrom, nTo, qpTo, vPieceFrom, qpFriend, vCapture, bCapture);
      else if (promotion != Piece._)
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
      var nTo = (Int32?)null;
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
              throw new MoveException($"Illegal move: {sPACN}");
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
              throw new MoveException($"Illegal move: {sPACN}");

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