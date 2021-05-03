//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-06-26 CNHume]Created File
//
// Conditionals:
//
//#define TestRotation
#define RefreshPV
#define BuildCapture
#define SaveCapture
#define TestDraw3

namespace Engine {
  using Exceptions;

  using MoveOrder;                      // For Variation

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;
  using System.Reflection;
  using System.Text;

  using static Board;
  using static Logging.Logger;
  using static Position;
  using static System.Math;
  using static System.String;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;
  using PieceCounter = System.UInt32;

  //
  // Instead of storing the Pawn Count (mod 4) the least significant bit pair
  // represents EventFlag bits marking which of two Bishop colors are held by
  // the current side. Then, the higher order bit pairs are used to represent
  // successive Piece Counts (mod 4).  So, 5 bit pairs, or a total of 10 bits
  // are used per side.
  //
  using PieceHashcode = System.UInt16;  // 10 bits
  using Plane = System.UInt64;
  using Ply = System.UInt16;

  static class Extension {
    #region Delegates
    public delegate StringBuilder MoveWriter(StringBuilder sb, Move move, CastleRule castle);
    #endregion

    #region Castle Rights
    public static StringBuilder AppendCastleRights(this StringBuilder sb, HiFlags fBlackHi, HiFlags fWhiteHi, CastleRule castle) {
      if (((fBlackHi | fWhiteHi) & HiFlags.CanCastleMask) == 0)
        sb.Append("-");
      else if (castle is not null && castle.IsChess960) {
        if ((fWhiteHi & HiFlags.CanOO) != 0)
          sb.Append((Char)('A' + castle.RuleParameter[White].RookOOFrom));
        if ((fWhiteHi & HiFlags.CanOOO) != 0)
          sb.Append((Char)('A' + castle.RuleParameter[White].RookOOOFrom));
        if ((fBlackHi & HiFlags.CanOO) != 0)
          sb.Append((Char)('a' + castle.RuleParameter[Black].RookOOFrom - nRankLast));
        if ((fBlackHi & HiFlags.CanOOO) != 0)
          sb.Append((Char)('a' + castle.RuleParameter[Black].RookOOOFrom - nRankLast));
      }
      else {
        if ((fWhiteHi & HiFlags.CanOO) != 0)
          sb.Append('K');
        if ((fWhiteHi & HiFlags.CanOOO) != 0)
          sb.Append('Q');
        if ((fBlackHi & HiFlags.CanOO) != 0)
          sb.Append('k');
        if ((fBlackHi & HiFlags.CanOOO) != 0)
          sb.Append('q');
      }

      return sb;
    }
    #endregion

    #region Square Methods
    //
    // Identify the Squares in a Plane
    //
    public static StringBuilder AppendSquares(this StringBuilder sb, Plane qp,
                                              String sDelimiter = sSpace) {
      while (qp != 0) {
        var n = RemoveLo(ref qp);
        sb.Append(sDelimiter).Append((sq)n);
      }

      return sb;
    }

    public static StringBuilder AppendRanks(this StringBuilder sb, String s, Boolean bFlip = false) {
      var bRightRuler = bFlip;
      for (int x = 0; x < nRanks; x++) {
        var rank = bFlip ? invertRank(x) : x;
        if (!bRightRuler)               // Left Pad
          sb.Append(s);
        sb.Append((Char)(cRankMin + rank));
        if (bRightRuler)                // Right Pad
          sb.Append(s);
      }
      return sb;
    }

    public static StringBuilder AppendFiles(this StringBuilder sb, String s, Boolean bFlip = false) {
      var bRightRuler = bFlip;
      for (var x = 0; x < nFiles; x++) {
        var file = bFlip ? invertFile(x) : x;
        if (!bRightRuler)	              // Left Pad
          sb.Append(s);
        sb.Append((Char)(cFileMin + file));
        if (bRightRuler)                // Right Pad
          sb.Append(s);
      }
      return sb;
    }

    public static StringBuilder AppendRuler(this StringBuilder sb, bool bRotateBoard = false, bool bFlip = false) {
      var bRightRuler = bFlip;
      if (!bRightRuler)
        sb.Append(cSpace);
      if (bRotateBoard)                 // Show Rank Numbers
        sb.AppendRanks(sSpace, bFlip);
      else                              // Show File Letters
        sb.AppendFiles(sSpace, bFlip);
      sb.Append(cNewline);
      return sb;
    }

    public static StringBuilder AppendRect(this StringBuilder sb, UInt32 uRect, Boolean bFlip = false) {
      var bRightRuler = bFlip;
      for (var x = 0; x < nFiles; x++) {
        var file = bFlip ? invertFile(x) : x;
        var c = (uRect & BIT0 << file) == 0 ? cVacant : cOccupied;
        if (!bRightRuler)               // Left Pad
          sb.Append(sSpace);
        sb.Append(c);
        if (bRightRuler)                // Right Pad
          sb.Append(sSpace);
      }
      return sb;
    }

    public static StringBuilder AppendIndent(this StringBuilder sb, int nDent) {
      sb.Append(Empty.PadRight(2 * nDent));
      return sb;
    }

    public static StringBuilder AppendDiag(this StringBuilder sb, Int32 nDiagLen, UInt32 uDiag) {
      for (var z = 0U; z < nDiagLen; z++, uDiag >>= 1) {
        var c = (uDiag & BIT0) == 0 ? cVacant : cOccupied;
        sb.Append(sSpace3).Append(c);
      }
      return sb;
    }
#if TestRotation
    public static StringBuilder AppendRectRotations(this StringBuilder sb, Plane[] qpRect, Plane qp) {
      for (var x = 0; x < nFiles; x++, qp <<= 1)
        sb.Append(sSpace).Append(sqUsingBit(qpRect, qp));
      return sb;
    }

    public static StringBuilder AppendDiagRotations(this StringBuilder sb, Int32 nDiagLen, Plane[] qpDiag, Plane qp) {
      for (var z = 0U; z < nDiagLen; z++, qp <<= 1)
        sb.Append(sSpace2).Append(sqUsingBit(qpDiag, qp));
      return sb;
    }
#endif
    #endregion

    #region Simple String Builders
    public static StringBuilder FlushLine(this StringBuilder sb) {
      if (sb.Length > 0) {
        LogLine(sb.ToString());
        sb.Clear();
      }
      return sb;
    }

    public static StringBuilder Delimit(this StringBuilder sb, String sDelimiter = sSpace) {
      if (sb.Length > 0)
        sb.Append(sDelimiter);
      return sb;
    }

    public static StringBuilder Wrap(
      this StringBuilder sbLine,
      StringBuilder sb,
      String sDelimiter = sSpace,
      Int16 mWrap = mWrapLength) {
      if (sbLine.Length < mWrap)    // PGN style line wrap
        return sbLine.Delimit(sDelimiter);

      sb.Append(sbLine).AppendLine();
      return sbLine.Clear();
    }

    private static StringBuilder appendMoveNumber(
      this StringBuilder sbLine,
      StringBuilder sb,
      Ply wGamePly,
      String sSuffix) {
      return sbLine.Append(moveNumber(wGamePly)).Append(sSuffix);
    }

    public static StringBuilder Append(this StringBuilder sb, Command.SearchBound bound) {
      return bound.AppendBounds(sb);
    }

    public static List<String> AddNotEmpty(this List<String> list, String s) {
      if (!IsNullOrEmpty(s))
        list.Add(s);
      return list;
    }
    #endregion

    #region Eval Methods
    //
    // Concise, User friendly Eval
    //
    public static StringBuilder AppendEvalTerm(this StringBuilder sb, Eval mEval) {
      sb.Delimit();

      if (mEval == EvalUndefined) {
        sb.Append(sEvalUndefined);
        return sb;
      }

      var mAbs = Abs(mEval);
      if (mAbs < MateMin)
        sb.AppendFormat("{0:0.00}", (Decimal)mEval / mUnitWeight);
      else {
        if (mEval < 0)
          sb.Append(sEvalMinus);

        if (mAbs <= MateMax)
          sb.Append(sEvalMovesToMate).Append(MateMax - mAbs);
        else if (mAbs == PlusInfinity)
          sb.Append(sEvalInfinity);
        else
          sb.Append(sEvalOverflow);
      }

      return sb;
    }

    //
    // UCI Eval
    //
    public static StringBuilder AppendEvalInfo(this StringBuilder sb, Eval mEval) {
      sb.Delimit();

      if (mEval == EvalUndefined) {
        sb.Append(sEvalUndefined);
        return sb;
      }

      var mAbs = Abs(mEval);
      if (mAbs < MateMin) {
        var nCentiPawn = Round(100 * mEval, mUnitWeight);
        sb.Append(sEvalUCICentiPawns).Append(nCentiPawn);
      }
      else {
        sb.Append(sEvalUCIMovesToMate);

        if (mEval < 0)
          sb.Append(sEvalMinus);

        if (mAbs <= MateMax)
          sb.Append(MateMax - mAbs);
        else if (mAbs == PlusInfinity)
          sb.Append(sEvalInfinity);
        else
          sb.Append(sEvalOverflow);
      }

      return sb;
    }
    #endregion

    #region Move Builders
    private static StringBuilder annotation(this StringBuilder sb, Move move) {
      if ((move & Move.NoteFinal) != 0) {
        if ((move & Move.NoteCheck) != 0)
          sb.Append(sNoteCheckmate);
        else
          sb.Append(sSpace)
            .Append(sTextStalemate);
      }
      else {
        if ((move & Move.NoteCheck) != 0)
          sb.Append(sNoteCheck);
#if TestDraw3
        if ((move & Move.NoteDraw) != 0)
          sb.Append(sNoteDraw);
        else if ((move & Move.NoteDraw2) != 0)
          sb.Append(sNoteDraw2);
#endif
      }

      return sb;
    }

    //
    // Format a Move in the more User friendly Algebraic Notation (AN)
    //
    public static StringBuilder AppendAN(this StringBuilder sb, Move move, CastleRule castle) {
      const Boolean bExpandFrom =
#if RefreshPV
        false;                          //[Assume]abbreviate() has been called
#else
        true;
#endif
      if (isNullMove(move) || !isDefinite(move)) {
        sb.Append(move);
        return sb;
      }

      var nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
      var nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
      var uPiece = (UInt32)move >> nPieceBit & vPieceMask;
      var vPiece = (Byte)(uPiece - vFirst);
      var bCastles = isCastles(move);
      if (bCastles) {
        #region Castles
        var sCastle = Empty;
        if (nTo == castle.RuleParameter[White].KingOOTo || nTo == castle.RuleParameter[Black].KingOOTo)
          sCastle = sHyphenOO;
        else if (nTo == castle.RuleParameter[White].KingOOOTo || nTo == castle.RuleParameter[Black].KingOOOTo)
          sCastle = sHyphenOOO;

        Debug.Assert(vPiece == vK6 && !IsNullOrEmpty(sCastle), "Invalid Castle");

        sb.Append(sCastle);
        #endregion
      }
      else {
        var sqTo = (sq)nTo;
        var sqFrom = (sq)nFrom;
        var bFile = (move & Move.OnlyFile) == 0;
        var bRank = (move & Move.OnlyRank) == 0;
        var uCapture = (UInt32)move >> nCaptiveBit & vPieceMask;
        var bCapture = uCapture > 0;
        var bPawnCapture = false;
        var bEnPassant = false;

        if (vPiece != vP6)     // Pawn piece aso elided when bExpandFrom
          sb.Append(PieceSymbol(vPiece));
        else if (bCapture)
          bPawnCapture = true;

        #region From Square
        //
        // The From Square may be appended in its entirety,
        // or abbreviated to include only its Rank or File:
        //
        if ((bPawnCapture || bFile) && bRank || bExpandFrom)
          sb.Append(sqFrom);
        else if (bPawnCapture || bFile || bRank) {
          var s = sqFrom.ToString();
          sb.Append(s[bRank ? 1 : 0]);
        }
        #endregion

        if (bCapture) {
          var vCapture = (Byte)(uCapture - vFirst);
          bEnPassant = vCapture == vEP6;
          sb.Append(sTakes);
#if SaveCapture && BuildCapture         //[Note]This static method cannot invoke getPiece()
          var bPawnCaptive = vCapture == vP6 || bEnPassant;
          if (bExpandFrom && !bPawnCaptive) {
            var sCapture = PieceSymbol(vCapture);
            sb.Append(sCapture);
          }
#endif
        }
        else if (bExpandFrom)
          sb.Append(sMoves);

        sb.Append(sqTo);

        #region Pawn Move Annotations
        if (vPiece == vP6) {
          var uPromotion = (UInt32)move >> nPromoteBit & vPieceMask;
          if (uPromotion > 0) {
            var vPromotion = (Byte)(uPromotion - vFirst);
            var sPromotion = PieceSymbol(vPromotion);
            sb.Append(sNotePromotion).Append(sPromotion);
          }
          else if (bEnPassant)
            sb.Append("(ep)");
        }
        #endregion
      }

      return sb.annotation(move);
    }

    //
    // Format a Move in Pure Algebraic Coordinate Notation (PACN).
    //
    public static StringBuilder AppendPACN(this StringBuilder sb, Move move, CastleRule castle) {
      if (isNullMove(move)) {
        sb.Append(sNullMove);
        return sb;
      }
      else if (!isDefined(move)) {
        sb.Append(move);
        return sb;
      }

      var nFrom = (Int32)move >> nFromBit & (Int32)uSquareMask;
      var nTo = (Int32)move >> nToBit & (Int32)uSquareMask;
      var piece = (Piece)((UInt32)move >> nPieceBit & vPieceMask);
      var bCastles = isCastles(move);
      //[Chess960]Avoid potential ambiguity of ordinary King moves with castling
      if (castle.IsChess960 && bCastles) {
        var sCastle = Empty;
        if (nTo == castle.RuleParameter[White].KingOOTo || nTo == castle.RuleParameter[Black].KingOOTo)
          sCastle = sPureOO;
        else if (nTo == castle.RuleParameter[White].KingOOOTo || nTo == castle.RuleParameter[Black].KingOOOTo)
          sCastle = sPureOOO;

        Debug.Assert(piece == Piece.K && !IsNullOrEmpty(sCastle), "Invalid Castle");
        sb.Append(sCastle);
      }
      else {
        var sqTo = (sq)nTo;
        var sqFrom = (sq)nFrom;
        sb.Append(sqFrom).Append(sqTo);

        if (piece == Piece.P) {
          var promotion = (Piece)((UInt32)move >> nPromoteBit & vPieceMask);
          if (promotion != Piece._)
            sb.Append(promotion.ToString().ToLower());
        }
      }

      return sb;
    }

    public static StringBuilder AppendMoves(
      this StringBuilder sb, IEnumerable<Move> moves,
      Ply wGamePly, Boolean bPure, CastleRule castle) {
      if (moves is null || !moves.Any())
        return sb;

      var sbLine = new StringBuilder().Append(sb);
      sb.Clear();

      if (IsOdd(wGamePly) && !bPure)    // Odd Ply => Number Black Move
        sbLine.Wrap(sb).appendMoveNumber(sb, wGamePly, sElipsis);

      const Int32 nCapacity = 1;
      var brackets = new Stack<String>(nCapacity);

      var bWasQxnt = false;
      foreach (var move in moves) {
        var bDelimited = false;
        var bQxnt = (move & Move.Qxnt) != 0;

        if (!bPure) {
          if (bQxnt && !bWasQxnt) {
            bWasQxnt = bQxnt;
            brackets.Push(sQxntClose);
            sbLine.Wrap(sb).Append(sQxntOpen);
            bDelimited = true;
          }
          else if (bWasQxnt && !bQxnt) {
            //[Safe]This should not occur because quiescent moves are made at the end of a search
            bWasQxnt = bQxnt;
            sbLine.Append(brackets.Pop());
          }
        }

        if (!bDelimited) {
          if (bPure)
            sbLine.Delimit();
          else
            sbLine.Wrap(sb);
        }

        if (bPure)
          sbLine.AppendPACN(move, castle);
        else {
          if (IsEven(wGamePly))         // Even Ply => Number White Move
            sbLine.appendMoveNumber(sb, wGamePly, sMoveNumber).Wrap(sb);

          sbLine.AppendAN(move, castle);
        }

        wGamePly++;
      }

      Debug.Assert(bWasQxnt == brackets.Count > 0, "Unbalanced Brackets");
      if (bWasQxnt)
        sbLine.Append(brackets.Pop());

      if (sbLine.Length > 0)
        sb.Append(sbLine);

      return sb;
    }

    //
    // Useful for listing candidate moves:
    //
    public static StringBuilder MapMoves(
      this StringBuilder sb, MoveWriter mw, IEnumerable<Move> moves, CastleRule castles) {
      if (moves is not null) {
        foreach (var move in moves) {
          mw(sb.Delimit(), move, castles);
        }
      }

      return sb;
    }

    public static StringBuilder WriteMoves(
      this StringBuilder sb, List<Move> moves, Ply wGamePly, Boolean bPure, CastleRule castle) {
      return bPure ?
        sb.MapMoves(AppendPACN, moves, castle) :
        sb.AppendMoves(moves, wGamePly, bPure, castle);
    }

    public static StringBuilder WriteVariation(
      this StringBuilder sb, Variation vn,
      Int32 nLine, Boolean bMultiPV,
      Boolean bWTM, Ply wGamePly,
      Boolean bPure, CastleRule castle) {
      var mEval = reflectValue(bWTM, vn.Value);
      if (bPure) {
        sb.Append("info score")
          .AppendEvalInfo(mEval);
      }
      else {
        sb.Append("info eval")
          .AppendEvalTerm(mEval);
      }

      sb.Append(sSpace);
      var sPurePV = bPure ? "pv" : "pvan";
      if (bMultiPV) {
        sb.Append("multi")
          .Append(sPurePV)
          .Append(sSpace)
          .Append(nLine + 1);           //[UCI]MultiPV are one-based
      }
      else
        sb.Append(sPurePV);

      //[Note]Variations begin with BestMove from MovePosition
      return sb.WriteMoves(vn.Moves, wGamePly, bPure, castle);
    }

    public static StringBuilder BestMove(
      this StringBuilder sb, List<Move> bestMoves, CastleRule castle) {
      if (bestMoves.Count > 0) {
        if (sb.Length == 0) sb.Append("info ");

        sb.Append("bestmove ")
          .AppendPACN(bestMoves[0], castle);

        if (bestMoves.Count > 1) {
          sb.Append(" ponder ")
            .AppendPACN(bestMoves[1], castle);
        }
      }

      return sb;
    }

    public static StringBuilder UpdateBestInfo(
      this StringBuilder sb,
      List<Move> bestMoves,
      List<Move> lineMoves,
      Eval mEval,
      Boolean bPonder,
      CastleRule castle) {
      //
      //[Note]addPV() calls UpdateBestInfo(), which compares this
      // variation to the previous BestMoves to inform the GUI of
      // changes to either of the Best or the Ponder moves.
      //
      if (bestMoves is null)
        throw new BoardException("Null bestMoves Instance");

      if (bestMoves.Count < 1 || !equalMoves(bestMoves[0], lineMoves[0]) ||
          bestMoves.Count > 1 && bPonder && !equalMoves(bestMoves[1], lineMoves[1])) {
        //[Note]refreshPV() has not been called
        bestMoves.Clear();
        bestMoves.AddRange(lineMoves);
        sb.BestMove(bestMoves, castle);
        sb.Append(" ");
      }

      sb.Append(sb.Length > 0 ? "score" : "info score");
      sb.AppendEvalInfo(mEval);

      return sb;
    }

    public static StringBuilder AppendOperations(
      this StringBuilder sb, Dictionary<String, List<String>> operations) {
      if (operations is not null) {
        foreach (var op in operations) {        // .OrderBy(op => op.Key)
          sb.Append(sSpace).Append(op.Key);

          foreach (var operand in op.Value)
            sb.Append(sSpace).Append(operand);

          sb.Append(";");
        }
      }

      return sb;
    }
    #endregion

    #region Composition Diagnostics
    public static StringBuilder AppendPieceCounts(
      this StringBuilder sb, Boolean bColor, PieceCounter uPieceCounts) {
      sb.AppendLine("Piece Counts:");
      var sFormat = bColor ? " {0}{2} {3,2}" : " {1}{2} {3,2}";

      for (var v6 = (Byte)0; v6 < nPieces; v6++,
           uPieceCounts >>= nPerNibble) {
        var n = (Byte)uPieceCounts & vNibble;
        var s = PieceSymbol(v6);

        sb.AppendFormat(sFormat, Parameter[White].Symbol, Parameter[Black].Symbol, s, n)
          .AppendLine();
      }

      return sb;
    }

    public static StringBuilder AppendPieceCounts(
      this StringBuilder sb, PieceCounter uWhiteCounts, PieceCounter uBlackCounts) {
      sb.AppendLine("Piece Counts:");

      for (var v6 = (Byte)0; v6 < nPieces; v6++,
           uWhiteCounts >>= nPerNibble,
           uBlackCounts >>= nPerNibble) {
        var nWhite = (Byte)uWhiteCounts & vNibble;
        var nBlack = (Byte)uBlackCounts & vNibble;
        var s = PieceSymbol(v6);

        sb.AppendFormat(" {0}{2} {3,2}  {1}{2} {4,2}",
            Parameter[White].Symbol, Parameter[Black].Symbol, s, nWhite, nBlack)
          .AppendLine();
      }

      return sb;
    }

    public static StringBuilder AppendPieceHash(
      this StringBuilder sb, PieceHashcode wHashWhite, PieceHashcode wHashBlack) {
      sb.AppendLine("Piece Hashcode:");

      for (var v6 = vHF; v6 < vK6; v6++,
           wHashWhite >>= nPerTwoBits,
           wHashBlack >>= nPerTwoBits) {
        var nWhite = (Byte)wHashWhite & vTwoBits;
        var nBlack = (Byte)wHashBlack & vTwoBits;
        var s = v6 == vHF ? "F" : PieceSymbol(v6);

        sb.AppendFormat(" {0}{2} {3,2}  {1}{2} {4,2}",
            Parameter[White].Symbol, Parameter[Black].Symbol, s, nWhite, nBlack)
          .AppendLine();
      }

      return sb;
    }
    #endregion

    #region Partial Sort Methods
    public static Int32 Insert<T>(this T[] entries, Int32 next) where T : IComparable {
      const Int32 first = 0;
      var entry = entries[next];        //[Assume]descending order
      while (next > first && entries[next - 1].CompareTo(entry) < 0)
        entries[next] = entries[--next];
      entries[next] = entry;
      return next;
    }

    public static void Place<T>(this T[] entries, Int32 first, Int32 next) {
      var entry = entries[next];
      while (next > first) entries[next] = entries[--next];
      entries[next] = entry;
    }
    #endregion
  }
}
