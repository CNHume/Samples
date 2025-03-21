﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2013-06-26 CNHume]Created File
//
// Conditionals:
//
#define BitOperations
//#define DeBruijn                        // DeBruijn vs Mask
//#define FullData                        // Full vs Half
#define TestRotation
#define RefreshPV
#define BuildCapture
#define SaveCapture
#define TestDraw3

using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Diagnostics;
using System.Reflection;                // For FieldInfo
using System.Runtime.CompilerServices;
using System.Text;

using static System.Math;
using static System.String;
using static System.StringComparison;

namespace Engine;

using Cache;

using Exceptions;

using MoveOrder;                        // For Variation

using static Board;
using static CacheValue.PawnPosition;
using static Logging.Logger;
using static Position;

using Eval = Int16;
//
// Type Aliases:
//
using Parameter = Position.PositionSide.PositionParameter;
using Plane = UInt64;
using Ply = UInt16;

static class Extension {
  #region Constants
  // Null Move:
  internal const String sNullMove = "0000";

  // Castling:
  internal const String sPureOO = "OO";
  internal const String sPure00 = "00";
  private const String sHyphenOO = "O-O";

  internal const String sPureOOO = "OOO";
  internal const String sPure000 = "000";
  private const String sHyphenOOO = "O-O-O";

  // User Friendly Algebraic Notation:
  private const String sTakes = "x";

  // Move Numbering:
  private const String sElipsis = "...";
  private const String sMoveNumber = ".";

  // Evaluations:
  private const String sEvalMinus = "-";
  private const String sEvalMovesToMate = "#";
  private const String sEvalUCIMovesToMate = "mate ";
  private const String sEvalUCICentiPawns = "cp ";

  private const String sEvalUndefined = "Undefined";
  private const String sEvalInfinity = "Infinity";
  private const String sEvalOverflow = "Overflow";

  // Move Annotations:
  private const String sNotePromotion = "=";
  private const String sNoteCheckmate = "#";
  private const String sNoteCheck = "+";
  private const String sNoteDraw2 = "@";
  private const String sNoteDraw = "=";

  // Quiescent Move Sequence:
  private const String sQxntOpen = "[";
  private const String sQxntClose = "]";
  #endregion                            // Constants

  #region Delegates
  public delegate StringBuilder MoveWriter(
    StringBuilder sb, Move move, BoardSide[] sides, Boolean bChess960);
  #endregion

  #region Castle Rights
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static (Parameter blackParameter, Parameter whiteParameter) GetBothParameters(
    this Parameter[] parameters) {
    return (parameters[Black], parameters[White]);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static (PositionSide blackSide, PositionSide whiteSide) GetBothSides(
    this PositionSide[] sides) {
    return (sides[Black], sides[White]);
  }

  public static StringBuilder AppendCastlingRights(
    this StringBuilder sb, PositionSide[] sides, Boolean bChess960) {
    var (blackSide, whiteSide) = sides.GetBothSides();

    var fBlackSide = blackSide.FlagsSide;
    var fWhiteSide = whiteSide.FlagsSide;
    var fColorSide = fBlackSide | fWhiteSide;

    if (!fColorSide.Has(SideFlags.CanCastle))
      sb.Append(cMinus);
    else if (bChess960) {
      sb.AppendChess960CastleRights(whiteSide, 'A');
      sb.AppendChess960CastleRights(blackSide, 'a');
    }
    else {
      if (fWhiteSide.Has(SideFlags.CanOO))
        sb.Append('K');
      if (fWhiteSide.Has(SideFlags.CanOOO))
        sb.Append('Q');

      if (fBlackSide.Has(SideFlags.CanOO))
        sb.Append('k');
      if (fBlackSide.Has(SideFlags.CanOOO))
        sb.Append('q');
    }

    return sb;
  }

  public static StringBuilder AppendChess960CastleRights(
    this StringBuilder sb, BoardSide side, Char cRookFile) {
    var fside = side.FlagsSide;
    var rule = side.Parameter.Rule;

    if (fside.Has(SideFlags.CanOO) && rule.RookOOFrom.HasValue)
      sb.Append((Char)(cRookFile + x(rule.RookOOFrom.Value)));
    if (fside.Has(SideFlags.CanOOO) && rule.RookOOOFrom.HasValue)
      sb.Append((Char)(cRookFile + x(rule.RookOOOFrom.Value)));

    return sb;
  }
  #endregion                            // Castle Rights

  #region Square Methods
  //
  // Identify the Squares in a Plane
  //
  public static StringBuilder AppendSquares(
    this StringBuilder sb, Plane qp, String sDelimiter = sSpace) {
    while (qp != 0) {
      var n = RemoveLo(ref qp);
      sb.Append(sDelimiter)
        .Append((Sq)n);
    }

    return sb;
  }

  public static StringBuilder AppendRanks(
    this StringBuilder sb, String s, Boolean bFlip = false) {
    var bRightRuler = bFlip;
    for (var x = 0; x < nRanks; x++) {
      var rank = bFlip ? InvertRank(x) : x;
      if (!bRightRuler)               // Left Pad
        sb.Append(s);
      sb.Append((Char)(cRankMin + rank));
      if (bRightRuler)                // Right Pad
        sb.Append(s);
    }
    return sb;
  }

  public static StringBuilder AppendFiles(
    this StringBuilder sb, String s, Boolean bFlip = false) {
    var bRightRuler = bFlip;
    for (var x = 0; x < nFiles; x++) {
      var file = bFlip ? InvertFile(x) : x;
      if (!bRightRuler)               // Left Pad
        sb.Append(s);
      sb.Append((Char)(cFileMin + file));
      if (bRightRuler)                // Right Pad
        sb.Append(s);
    }
    return sb;
  }

  public static StringBuilder AppendRuler(
    this StringBuilder sb, Boolean bRotateBoard = false, Boolean bFlip = false) {
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

  public static StringBuilder AppendOrth(
    this StringBuilder sb, UInt32 uOrth, Boolean bFlip = false) {
    var bRightRuler = bFlip;
    for (var x = 0; x < nFiles; x++) {
      var file = bFlip ? InvertFile(x) : x;
      var c = (uOrth & bit(file)) == 0 ? cVacant : cOccupied;
      if (!bRightRuler)               // Left Pad
        sb.Append(cSpace);
      sb.Append(c);
      if (bRightRuler)                // Right Pad
        sb.Append(cSpace);
    }
    return sb;
  }

  public static StringBuilder AppendIndent(this StringBuilder sb, Int32 nDent) {
    sb.Append(Empty.PadRight(2 * nDent));
    return sb;
  }

  public static StringBuilder AppendDiag(
    this StringBuilder sb, Int32 nDiagLen, UInt32 uDiag) {
    for (var z = 0U; z < nDiagLen; z++, uDiag >>= 1) {
      var c = IsOdd(uDiag) ? cOccupied : cVacant;
      sb.Append(sSpace3).Append(c);
    }
    return sb;
  }
#if TestRotation
  // Find square holding bit based on Rotation Map:
  private static Sq sqUsingBit(Plane[] qpMask, Plane qp) {
    for (var n = 0; n < nSquares; n++)
      if (qpMask[n] == qp) return (Sq)n;

    throw new BoardException("Games Not Found");
  }

  public static StringBuilder AppendOrthRotations(
    this StringBuilder sb, Plane[] qpOrth, Plane qp) {
    for (var x = 0; x < nFiles; x++, qp <<= 1)
      sb.Append(cSpace).Append(sqUsingBit(qpOrth, qp));
    return sb;
  }

  public static StringBuilder AppendDiagRotations(
    this StringBuilder sb, Int32 nDiagLen, Plane[] qpDiag, Plane qp) {
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

  public static StringBuilder Delimit(
    this StringBuilder sb, String sDelimiter = sSpace) {
    if (sb.Length > 0)
      sb.Append(sDelimiter);
    return sb;
  }

  public static StringBuilder Wrap(
    this StringBuilder sbLine,
    StringBuilder sb,
    String sDelimiter = sSpace,
    Int16 mWrap = mWrapLength) {
    if (sbLine.Length < mWrap)          // PGN style line wrap
      return sbLine.Delimit(sDelimiter);

    sb.Append(sbLine)
      .AppendLine();
    return sbLine.Clear();
  }

  private static StringBuilder appendMoveNumber(
    this StringBuilder sbLine,
    Ply wGamePly,
    String sSuffix) {
    return
      sbLine
        .Append(MoveNumber(wGamePly))
        .Append(sSuffix);
  }

  public static StringBuilder Append(this StringBuilder sb, Commands.SearchBound bound) {
    return bound.AppendBounds(sb);
  }

  public static List<String> AddNotEmpty(this List<String> list, String s) {
    if (!IsNullOrEmpty(s))
      list.Add(s);
    return list;
  }

  public static StringBuilder AppendNodeNumber(
    this StringBuilder sb, Int64 lNodes) {
    return sb.AppendFormat($"Node #{lNodes}");
  }
  #endregion

  #region Eval Methods
  //
  // Concise, User-friendly Eval
  //
  public static StringBuilder AppendEvalTerm(this StringBuilder sb, Eval mEval) {
    sb.Delimit();

    if (mEval == EvalUndefined) {
      sb.Append(sEvalUndefined);
      return sb;
    }

    var mAbs = Abs(mEval);
    if (mAbs < MateMin)
      sb.AppendFormat($"{(Decimal)mEval / mUnitWeight:0.00}");
    else {
      if (mEval < 0)
        sb.Append(sEvalMinus);

      if (mAbs <= MateMax)
        sb.Append(sEvalMovesToMate)
          .Append(MateMax - mAbs);
      else if (mAbs == PlusInfinity)
        sb.Append(sEvalInfinity);
      else
        sb.Append(sEvalOverflow);
    }

    return sb;
  }

  //
  // Centi-Pawn Value
  //
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private static Int32 cpValue(Eval mEval) {
    return Round(100 * mEval, mUnitWeight);
  }

  public static StringBuilder AppendEvalInfo(this StringBuilder sb, Eval mEval) {
    sb.Delimit();

    if (mEval == EvalUndefined) {
      sb.Append(sEvalUndefined);
      return sb;
    }

    var mAbs = Abs(mEval);
    if (mAbs < MateMin)
      sb.Append(sEvalUCICentiPawns)
        .Append(cpValue(mEval));
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
  #endregion                            // Eval Methods

  #region Enum Methods
  //[Speed]Enum.HasFlag() incurs significant performance overhead, due to reflection.
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  // Based on the Stack Overflow question:
  //"Most common C# bitwise operations on enums"
  // https://stackoverflow.com/questions/93744/most-common-c-sharp-bitwise-operations-on-enums
  public static Boolean HasFlag2<T>(this T enumeration, T flags) where T : Enum {
    // The following throws
    // InvalidCastException: Unable to cast object of type 'TurnFlags' to type 'System.Int32'
    var e = (Int32)(Object)enumeration;
    var f = (Int32)(Object)flags;
    return (e & f) != 0;
  }
#if TestHasFlag2
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this TurnFlags enumeration, TurnFlags flags) {
    return enumeration.HasFlag2(flags);
  }
#else
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this TurnFlags enumeration, TurnFlags flags) {
    return (enumeration & flags) != 0;
  }
#endif                                  // TestHasFlag2
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this SideFlags enumeration, SideFlags flags) {
    return (enumeration & flags) != 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this PRPFlags enumeration, PRPFlags flags) {
    return (enumeration & flags) != 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this EvalFlags enumeration, EvalFlags flags) {
    return (enumeration & flags) != 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this DrawFlags enumeration, DrawFlags flags) {
    return (enumeration & flags) != 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this ModeFlags enumeration, ModeFlags flags) {
    return (enumeration & flags) != 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this Move enumeration, Move flags) {
    return (enumeration & flags) != 0;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  public static Boolean Has(this ProbeResult enumeration, ProbeResult flags) {
    return (enumeration & flags) != 0;
  }
  #endregion                            // Enum Methods

  #region Move Builders
  private static StringBuilder annotation(this StringBuilder sb, Move move) {
    if (move.Has(Move.NoteFinal)) {
      //[Safe]Quiescent Move.NoteFinal should not occur.
      Trace.Assert((move & Move.Qxnt) == 0, "Quiescent Move.NoteFinal");

      if (move.Has(Move.NoteCheck))
        sb.Append(sNoteCheckmate);
      else
        sb.Append(cSpace)
          .Append(sTextStalemate);
    }
    else {
      if (move.Has(Move.NoteCheck))
        sb.Append(sNoteCheck);
#if TestDraw3
      if (move.Has(Move.NoteDraw))
        sb.Append(sNoteDraw);
      else if (move.Has(Move.NoteDraw2))
        sb.Append(sNoteDraw2);
#endif
    }

    return sb;
  }

  //
  // Format a Move in the more User friendly Algebraic Notation (AN)
  //
  public static StringBuilder AppendAN(
    this StringBuilder sb, Move move, BoardSide[] sides, Boolean _) {
    const Boolean bExpandFrom =
#if RefreshPV
      false;                            //[Assume]abbreviate() has been called
#else
      true;
#endif
    if (IsNullMove(move) || IsIndefinite(move))
      return sb.Append(move);

    unpack2(move, out Int32 nFrom, out Int32 nTo,
            out UInt32 uPiece, out UInt32 uPromotion,
            out Boolean bCastles, out Boolean bCapture);
    var vPiece = PieceIndex(uPiece);
    var bEnPassant = false;

    if (bCastles) {
      #region Castles
      var sCastle = Empty;
      foreach (var side in sides) {
        var rule = side.Parameter.Rule;
        if (nTo == rule.KingOOTo) {
          sCastle = sHyphenOO;
          break;
        }

        if (nTo == rule.KingOOOTo) {
          sCastle = sHyphenOOO;
          break;
        }
      }

      Debug.Assert(vPiece == vK6 && !IsNullOrEmpty(sCastle), "Invalid Castle");
      sb.Append(sCastle);
      #endregion
    }
    else {
      var sqTo = (Sq)nTo;
      var sqFrom = (Sq)nFrom;

      if (vPiece != vP6)                // Pawn symbols are also elided when bExpandFrom is set
        sb.Append(PieceSymbol(vPiece));

      #region Abbreviate From Square
      //
      // The From Games may be shown in its entirety,
      // or abbreviated to show only its File or Rank:
      //
      if (IsShowFrom(move) || bExpandFrom)
        sb.Append(sqFrom);
      else {
        var sFrom = sqFrom.ToString();
        if (IsShowFile(move))
          sb.Append(sFrom[FilePos]);
        else if (IsShowRank(move))
          sb.Append(sFrom[RankPos]);
      }
      #endregion

      if (bCapture) {
        var uCapture = Captured(move);
        var vCapture = PieceIndex(uCapture);
        bEnPassant = vCapture == vEP6;

        sb.Append(sTakes);
#if SaveCapture && BuildCapture         //[Note]This static method cannot invoke GetPieceIndex()
        var bPawnCaptive = vCapture == vP6 || bEnPassant;
        if (bExpandFrom && !bPawnCaptive) {
          var sCapture = PieceSymbol(vCapture);
          sb.Append(sCapture);
        }
#endif
      }

      sb.Append(sqTo);

      #region Promotion
      if (vPiece == vP6) {
        if (uPromotion > 0) {
          var vPromotion = PieceIndex(uPromotion);
          var sPromotion = PieceSymbol(vPromotion);
          sb.Append(sNotePromotion).Append(sPromotion);
        }
      }
      #endregion                        // Promotion
    }

    sb.annotation(move);

    if (bEnPassant)
      sb.Append(" {ep}");

    return sb;
  }

  //
  // Format a Move in Pure Algebraic Coordinate Notation (PACN).
  //
  public static StringBuilder AppendPACN(
    this StringBuilder sb, Move move, BoardSide[] sides, Boolean bChess960) {
    if (IsNullMove(move))
      return sb.Append(sNullMove);
    else if (IsIndefinite(move))
      return sb.Append(move);

    unpack2(move, out Int32 nFrom, out Int32 nTo,
            out UInt32 uPiece, out UInt32 uPromotion,
            out Boolean bCastles, out Boolean _);
    var piece = (Piece)uPiece;

    //[Chess960]Avoid any ambiguity between castling and an ordinary King move
    if (bChess960 && bCastles) {
      #region Chess960 Castles
      var sCastle = Empty;
      foreach (var side in sides) {
        var rule = side.Parameter.Rule;
        if (nTo == rule.KingOOTo) {
          sCastle = sPureOO;
          break;
        }

        if (nTo == rule.KingOOOTo) {
          sCastle = sPureOOO;
          break;
        }
      }

      Debug.Assert(piece == Piece.K && !IsNullOrEmpty(sCastle), "Invalid Castle");
      sb.Append(sCastle);
      #endregion
    }
    else {
      sb.Append((Sq)nFrom)
        .Append((Sq)nTo);

      if (uPromotion > 0) {
        Debug.Assert(piece == Piece.P, "Only Pawns are allowed to promote");

        var promotion = (Piece)uPromotion;
        sb.Append(promotion.ToString().ToLower());
      }
    }

    return sb;
  }

  public static StringBuilder AppendNumberedMoves(
    this StringBuilder sb, IEnumerable<Move> moves,
    Ply wGamePly, BoardSide[] sides, Boolean bPure, Boolean bChess960) {
    if (moves == null || !moves.Any())
      return sb;

    var sbLine = new StringBuilder().Append(sb);
    sb.Clear();

    if (IsOdd(wGamePly) && !bPure)      // Odd Ply => Number Black Move
      sbLine.Wrap(sb)
            .appendMoveNumber(wGamePly, sElipsis);

    const Int32 nNestedBracketsCapacity = 2;
    Stack<String> brackets = new(nNestedBracketsCapacity);

    var bWasQxnt = false;
    foreach (var move in moves) {
      var bDelimited = false;
      var bQxnt = move.Has(Move.Qxnt);

      if (!bPure) {
        if (bQxnt && !bWasQxnt) {
          bWasQxnt = bQxnt;
          brackets.Push(sQxntClose);
          sbLine.Wrap(sb)
                .Append(sQxntOpen);
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
        sbLine.AppendPACN(move, sides, bChess960);
      else {
        if (IsEven(wGamePly))           // Even Ply => Number White Move
          sbLine.appendMoveNumber(wGamePly, sMoveNumber)
                .Wrap(sb);

        sbLine.AppendAN(move, sides, bChess960);
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
    this StringBuilder sb, MoveWriter mw, IEnumerable<Move> moves, BoardSide[] sides, Boolean bChess960) {
    if (moves != null) {
      foreach (var move in moves) {
        mw(sb.Delimit(), move, sides, bChess960);
      }
    }

    return sb;
  }

  public static StringBuilder WriteMoves(
    this StringBuilder sb, List<Move> moves, Ply wGamePly, BoardSide[] sides, Boolean bPure, Boolean bChess960) {
    return bPure ?
      sb.MapMoves(AppendPACN, moves, sides, bChess960) :
      sb.AppendNumberedMoves(moves, wGamePly, sides, bPure, bChess960);
  }

  public static StringBuilder WriteVariation(
    this StringBuilder sb, Variation vn,
    Int32 nLine, Boolean bMultiPV,
    Boolean bWTM, Ply wGamePly,
    Boolean bPure, BoardSide[] sides, Boolean bChess960) {
    var mEval = ReflectValue(bWTM, vn.Value);
    if (bPure) {
      sb.Append("info score")
        .AppendEvalInfo(mEval);
    }
    else {
      sb.Append("info eval")
        .AppendEvalTerm(mEval);
    }

    sb.Append(cSpace);
    var sPurePV = bPure ? "pv" : "pvan";
    if (bMultiPV) {
      sb.Append("multi")
        .Append(sPurePV)
        .Append(cSpace)
        .Append(nLine + 1);             //[UCI]MultiPV are one-based
    }
    else
      sb.Append(sPurePV);

    //[Note]Variations begin with BestMove from MovePosition
    if (vn.Moves != null)
      sb.WriteMoves(vn.Moves, wGamePly, sides, bPure, bChess960);

    return sb;
  }

  public static StringBuilder BestInfo(
    this StringBuilder sb, List<Move> bestMoves, BoardSide[] sides, Boolean bChess960) {
    if (bestMoves.Count > 0) {
      if (sb.Length == 0) sb.Append("info ");

      sb.Append("bestmove ")
        .AppendPACN(bestMoves[0], sides, bChess960);

      if (bestMoves.Count > 1) {
        sb.Append(" ponder ")
          .AppendPACN(bestMoves[1], sides, bChess960);
      }
    }

    return sb;
  }

  //
  // Called by addPV() to compare new best variations to the existing BestLine
  // and report changes in either Best move or Ponder move to the GUI.
  //
  public static StringBuilder UpdateBestInfo(
    this StringBuilder sb,
    List<Move> bestLine,
    List<Move> vnMoves,
    Eval mEval,
    Boolean bPonder,
    BoardSide[] sides,
    Boolean bChess960) {
    if (bestLine.Count < 1 || !EqualMoves(vnMoves[0], bestLine[0]) ||
        bestLine.Count > 1 && bPonder && !EqualMoves(vnMoves[1], bestLine[1])) {
      //[Note]refreshPV() has not been called
      bestLine.Clear();
      bestLine.AddRange(vnMoves);
      sb.BestInfo(bestLine, sides, bChess960);
      sb.Append(cSpace);
    }

    //
    // Report score for every new variation, even if
    // the Best move and Ponder move remain the same.
    //
    sb.Append(sb.Length > 0 ? "score" : "info score");
    sb.AppendEvalInfo(mEval);

    return sb;
  }

  public static StringBuilder AppendOperations(
    this StringBuilder sb, Dictionary<String, List<String>?>? operations) {
    if (operations != null) {
      foreach (var op in operations) {  //.OrderBy(op => op.Key)
        sb.Append(cSpace)
          .Append(op.Key);

        if (op.Value != null) {
          foreach (var operand in op.Value)
            sb.Append(cSpace)
              .Append(operand);
        }

        sb.Append(cSemi);
      }
    }

    return sb;
  }
  #endregion                            // Move Builders

  #region Composition Diagnostics
  public static StringBuilder AppendPieceCounts(
    this StringBuilder sb, BoardSide side, UInt32 uPieceCounts) {
    sb.AppendLine("Piece Counts:");
    for (Byte v6 = 0; v6 < nPieces; v6++,
         uPieceCounts >>= nPerNibble) {
      var u = Nibble(uPieceCounts);
      var s = PieceSymbol(v6);

      sb.AppendFormat($" {side.Parameter.Symbol}{s} {u,2}")
        .AppendLine();
    }

    return sb;
  }

  public static StringBuilder AppendPieceCounts(
    this StringBuilder sb,
    BoardSide blackSide,
    BoardSide whiteSide,
    UInt32 uBlackCounts,
    UInt32 uWhiteCounts) {
    sb.AppendLine("Piece Counts:");

    for (Byte v6 = 0; v6 < nPieces; v6++,
         uBlackCounts >>= nPerNibble,
         uWhiteCounts >>= nPerNibble) {
      var uBlack = Nibble(uBlackCounts);
      var uWhite = Nibble(uWhiteCounts);
      var s = PieceSymbol(v6);

      sb.AppendFormat($" {blackSide.Parameter.Symbol}{s} {uBlack,2}")
        .AppendFormat($" {whiteSide.Parameter.Symbol}{s} {uWhite,2},")
        .AppendLine();
    }

    return sb;
  }

  //
  // Piece Hashcode
  // --------------
  // Instead of storing the Pawn Count (mod 4) the least significant bit pair
  // represents EventFlag bits marking which of two Bishop colors are held by
  // the current side. Then, the higher order bit pairs are used to represent
  // successive Piece Counts (mod 4).  So, 5 bit pairs, or a total of 10 bits
  // are used per side.
  //
  public static StringBuilder AppendPieceHash(
    this StringBuilder sb, BoardSide blackSide, BoardSide whiteSide) {
    sb.AppendLine("Piece Hashcode:");
    var wHashBlack = blackSide.PieceHash;
    var wHashWhite = whiteSide.PieceHash;

    for (var v6 = vHF; v6 < vK6; v6++,
         wHashBlack >>= nPerTwoBits,
         wHashWhite >>= nPerTwoBits) {
      var nBlack = TwoBits(wHashBlack);
      var nWhite = TwoBits(wHashWhite);
      var s = v6 == vHF ? "F" : PieceSymbol(v6);

      sb.AppendFormat($" {blackSide.Parameter.Symbol}{s} {nBlack,2},")
        .AppendFormat($" {whiteSide.Parameter.Symbol}{s} {nWhite,2}")
        .AppendLine();
    }

    return sb;
  }
  #endregion                            // Composition Diagnostics

  #region Partial Sort Methods
  public static Int32 Insert<T>(this T[] entries, Int32 next) where T : IComparable {
    const Int32 first = 0;
    var entry = entries[next];          //[Assume]descending order
    while (next > first && entries[next - 1].CompareTo(entry) < 0)
      entries[next] = entries[--next];
    entries[next] = entry;
    return next;
  }

  //
  // Rotate the entries[first:next] subrange such that entries[next] becomes
  // the first element and the remaining entries are shifted upwards.
  //
  public static void Rotate<T>(this T[] entries, Int32 first, Int32 next) {
    if (next > first) {
      var entry = entries[next];
      while (next > first) entries[next] = entries[--next];
      entries[next] = entry;
    }
  }
  #endregion                            // Partial Sort Methods

  #region Parse Methods
  #region ParseEnumFromName Helpers
  //
  // Based on the answer to the Stackoverflow Question: "Enum value from display name"
  // See https://stackoverflow.com/questions/33225729/enum-value-from-display-name
  //
  //[Note]Certain Enums, e.g., IdentifierType may need to become Codeable Concepts.
  //
  public static TEnum? TryParseEnumFromName<TEnum>(
    this string name, bool ignoreCase = default)
    where TEnum : Enum {
    TEnum? value = default;
    if (!IsNullOrEmpty(name)) {
      var comparisonType = ignoreCase ? CurrentCultureIgnoreCase : CurrentCulture;
      var type = typeof(TEnum);
      foreach (var field in type.GetFields()) {
        var descriptionAttribute =
          Attribute.GetCustomAttribute(field, typeof(DescriptionAttribute)) as DescriptionAttribute;
        var displayAttribute =
          Attribute.GetCustomAttribute(field, typeof(DisplayAttribute)) as DisplayAttribute;

        var found =
          name.Equals(field.Name, comparisonType) ||
          descriptionAttribute != null &&
          name.Equals(descriptionAttribute.Description, comparisonType) ||
          displayAttribute != null &&
          name.Equals(displayAttribute.Name, comparisonType);

        if (found) {
          value = (TEnum?)field.GetValue(default);
          return value;
        }
      }
    }
    return value;
  }

  public static TEnum ParseEnumFromName<TEnum>(
    this string name, bool ignoreCase = default)
    where TEnum : Enum {
    var result = name.TryParseEnumFromName<TEnum>(ignoreCase);
    if (result == null) {
      var type = typeof(TEnum);
      throw new ArgumentOutOfRangeException($"{type.Name} does not contain {name}");
    }
    return result;
  }

  // Originally based on Enum and [Display(Name = "")] by Pawan Pal 2016-02-17
  // See https://forums.asp.net/t/2085611.aspx?Enum+and+Display+Name+
  public static string GetDisplayName(this Enum enumeration) {
    var attr = enumeration.GetAttribute<DisplayAttribute>();
    return attr?.Name ?? enumeration.ToString();
  }

  public static string GetDescription(this Enum enumeration) {
    var attr = enumeration.GetAttribute<DescriptionAttribute>();
    return attr?.Description ?? enumeration.ToString();
  }

  public static T? GetAttribute<T>(this Enum enumeration) where T : Attribute {
    var type = enumeration.GetType();
    var name = enumeration.ToString();
    // Get Enum field:
    var field = type.GetField(name);
    return field?.GetCustomAttribute<T>();
  }

  public static String GetTypeName(this Type type) {
    var displayName = type
      .GetCustomAttributes(typeof(DisplayAttribute), true)
      .FirstOrDefault() as DisplayAttribute;
    return displayName?.Name ?? type.Name;
  }
  #endregion                            // ParseEnumFromName Helpers

  public static TStruct? TryParseEnum<TStruct>(
    this String s, Boolean ignoreCase = default)
    where TStruct : struct {
    return Enum.TryParse(s, ignoreCase, out TStruct result) ?
      (TStruct?)result : default;
  }

  public static TEnum ParseEnum<TEnum>(
    this String value, Boolean ignoreCase = default)
    where TEnum : Enum {
    return (TEnum)Enum.Parse(typeof(TEnum), value, ignoreCase);
  }
  #endregion                            // Parse Methods

  #region Trailing Zero Count (TZC) Mode
  public static StringBuilder AppendTZCMode(this StringBuilder sb) {
#if DEBUG
#if BitOperations
    sb.Append(" Numerics");
#elif FullData                          // FullData
    sb.Append(" Full");
#if DeBruijn
    sb.Append("DeBruijn");
#else                                   //!DeBruijn
    sb.Append("Mask");
#endif                                  // DeBruijn
#else                                   //!FullData
    sb.Append(" Half");
#if DeBruijn
    sb.Append("DeBruijn");
#else                                   //!DeBruijn
    sb.Append("Mask");
#endif                                  // DeBruijn
#endif                                  // FullData
    sb.Append(" Debug");
#else                                   //!DEBUG
#if BitOperations
    sb.Append(" Numerics");
#elif FullData                          // FullData
    sb.Append(" Full");
#if DeBruijn
    sb.Append("DeBruijn");
#else                                   //!DeBruijn
    sb.Append("Mask");
#endif                                  // DeBruijn
#else                                   //!FullData
    sb.Append(" Half");
#if DeBruijn
    sb.Append("DeBruijn");
#else                                   //!DeBruijn
    sb.Append("Mask");
#endif                                  // DeBruijn
#endif                                  // FullData
    sb.Append(" Release");
#endif                                  // DEBUG
    return sb;
  }
  #endregion                            // Trailing Zero Count (TZC) Mode
}
