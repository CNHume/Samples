﻿//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2012-03-01 CNHume]Created File
//
//#define AbbreviateLookup
#define DebugMove
//#define DebugMoveColor
//#define DebugPlace
#define FilterCandidates
//#define Magic
#define TestDraw3
//#define TraceVal

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;

namespace Engine;

using Command;                          // For UCI.IsDebug

using static Logging.Logger;

//
// Type Aliases:
//
using Depth = UInt16;
using Eval = Int16;
using Plane = UInt64;

partial class Position : Board {
  #region Annotation Methods
  //
  // InCheck and most Draw Flags are available once IsLegal() has been called;
  // but Final (and therefore also Draw50) depend on whether a Legal Move can
  // also be found for the subsequent ply.
  //
  // These Note Flags are serialized by the annotation() Extension Method.
  //
  protected Move annotateEarly(Move move) {
    if (InCheck()) move |= Move.NoteCheck;
#if TestDraw3
    if (IsDraw2()) move |= Move.NoteDraw2;
    if (IsDraw()) move |= Move.NoteDraw;
#endif
    return move;
  }

  private Move annotateFinal(Move move) {
    if (IsFinal()) move |= Move.NoteFinal;
#if TestDraw3
    if (IsDraw()) move |= Move.NoteDraw;
#endif
    return move;
  }

  private Move abbreviate(Move move) {
    if (IsNullMove(move) || !IsDefined(move))   //[Safe]
      return move;

    unpack1(move, out Int32 nFrom, out Int32 nTo,
            out UInt32 uPiece, out Boolean bCapture);

    var vPiece = PieceIndex(uPiece);
    var qpAtxTo = PieceAtxTo(nFrom, nTo, vPiece, bCapture);

    filterCandidates(move, ref qpAtxTo);//[Conditional]

    if (qpAtxTo == 0) {
      var sAction = bCapture ? "capture" : "move";
      Debug.Assert(
        qpAtxTo != 0, $"There is no piece that can {sAction} from {(Sq)nFrom} to {(Sq)nTo}");
      Display();
    }

    //
    // qpAtxTo holds Pieces of the appropriate type which "attack" nTo.
    //
    if (vPiece == vP6 && bCapture)
      move |= Move.HideRank;            // Pawns capture from neighboring Files along one Rank.
    else if (IsOneOrLess(qpAtxTo))
      move |= Move.HideFrom;
    else {
      //
      // More than one piece of the type being moved attack nTo:
      //
      // Determine whether both Rank and File are required; or whether Rank or File suffices
      // to distinguish the piece.  File is preferred where either the Rank or File suffices.
      //
      //[Note]Ray Attacks are used to identify pieces of the type being moved along the Rank
      // or File, whether or not any pieces intervene.
      //
      const Byte vEmptyState = 0;
#if Magic
      var qpFileAtx = AtxFile[MagicFile[vEmptyState]];
#else
      var qpFileAtx = AtxFile[vEmptyState];
#endif
      //
      // Is there another piece of the type being moved
      // which can attack nTo from the same File?
      //
      if ((qpAtxTo & qpFileAtx[nFrom]) == 0)
        // File distinguishes the piece being moved: Rank can be hidden.
        move |= Move.HideRank;
      else {
        //[Note]AtxRank does not require Magic support.
        var qpRankAtx = AtxRank[vEmptyState];
        //
        // Is there another piece of the type being moved
        // which can attack nTo from the same Rank?
        //
        if ((qpAtxTo & qpRankAtx[nFrom]) == 0)
          // Rank distinguishes the piece being moved: File can be hidden.
          move |= Move.HideFile;
        //else neither File nor Rank can be hidden.
      }
    }

    return move;
  }

  [Conditional("FilterCandidates")]
  private void filterCandidates(Move move, ref Plane qpAtxTo) {
    Plane qpPinned = default;

    var child = Push();                 // Push Position to make the moves
    try {
      //
      // Try candidate moves from each square n
      // in qpAtxTo to filter the pinned pieces:
      //
      var moveTo = move & ~Move.FromMask;
      var qp = qpAtxTo;
      while (qp != 0) {
        var n = RemoveLo(ref qp, out Plane qpCandidate);
        if (!child.tryCandidate(moveTo | FromMove(n)))
          qpPinned |= qpCandidate;
      }
    }
    finally {
      Pop(ref child);                   // Pop Position used for this Ply
    }

    qpAtxTo &= ~qpPinned;
  }
  #endregion                            // Annotation Methods

  #region MultiPV Support
  public Eval AddPV(Eval mAlpha, Eval mValue, Move move, List<Move> line) {
    var bWTM = WTM();
    //[Lock]UCI may change this at any time.  See GameState.newVariations()
    var bHasValue = 0 < State.VariationCount;
    var bGrow = State.VariationCount < State.MultiPVLength;

    if (bGrow)
      State.VariationCount++;

    var bRoomToGrow = State.VariationCount < State.MultiPVLength;
    var vn = State.Variation;
    var nFinal = State.VariationCount - 1;
    var bPlace = bGrow || bHasValue && mValue > vn[nFinal].Value;

    if (bPlace) {
      vn[nFinal].Value = mValue;
      bHasValue = true;

      var lineMoves = vn[nFinal].Moves;

      if (lineMoves == null) {
        lineMoves = new List<Move>();
        vn[nFinal].Moves = lineMoves;
      }
      else
        lineMoves.Clear();

      if (!IsDefined(move)) {
        Debug.Assert(IsDefined(move), $"Undefined Move [{nameof(AddPV)}]");
        move = Move.NullMove;
      }

      lineMoves.Add(move);

      var bPonder = line.Count > 0;     //!bChildFinal
      if (bPonder)
        lineMoves.AddRange(line);

      //
      // Insert at correct position:
      //
      var nPlace = vn.Insert(nFinal);

      if (nPlace == 0) {
        var sb = new StringBuilder();
        var mEval = ReflectValue(bWTM, mValue);
        sb.UpdateBestInfo(State.BestMoves, lineMoves, mEval, bPonder, Side, State.IsChess960)
          .FlushLine();
      }
#if DebugPlace
      if (UCI.IsDebug) {
        var sb = new StringBuilder();
        var sGrow = bGrow ? "Placed" : "Replaced";
        sb.AppendFormat($"{sGrow} vn[{nPlace}]");
        LogInfo(Level.note, sb.ToString());
        sb.Clear();
        State.MovePosition.writePV(sb, nPlace, bWTM);
      }
#endif
    }

    //
    // Avoid raising Alpha until we have a candidate for the weakest Variation:
    //
    return bRoomToGrow ? mAlpha : bHasValue ? vn[nFinal].Value : mValue;
  }

  private void lookupPV(Eval mAlpha, Eval mBeta, List<Move> moves) {
    //[Note]LoadMove() and abbreviate() require the parent position to be supplied by resetMove():
    probeMove(mAlpha, mBeta, out Move moveFound);
    // moveFound not always defined for EvalType.Upper [Fail Low]
    if (IsDefinite(moveFound)) {        //[Safe]Also prevent unexpected EmptyMove
      var moveNoted = moveFound;
      if (!State.IsPure) {              // Standard Algebraic Notation (AN) supports abbreviation
#if AbbreviateLookup
        moveNoted = abbreviate(moveNoted);
#else
        moveNoted &= ~Move.HideFrom;    // Suppress abbreviation to indicate moves recovered by probeMove()
#endif
      }
#if DebugMove
      unpackMove1(moveNoted, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
      //unpackMove2(moveNoted, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
#if DebugMoveColor
      var bWTM = WTM();
      var bWhiteMove = moveNoted.Has(Move.WTM);
      if (bWTM != bWhiteMove) {
        Debug.Assert(bWTM == bWhiteMove, $"WTM != WhiteMove [{nameof(lookupPV)}]");
      }
#endif
      moves.Add(moveNoted);

      var bLegal = tryOrSkip(ref moveNoted);
      Debug.Assert(bLegal, $"{nameof(lookupPV)} obtained an Illegal Move");
#if TraceVal
      // CurrentMove set in [null|try]Move()
      if (IsTrace())
        DisplayCurrent(nameof(lookupPV));
#endif
      SetDraw50();                      // Mark Draw50 after having made the move

      //[Note]If Draw3 is set, this lookupPV() recursion must terminate!
      if (!IsDraw()) {
        //
        // Recursion vs iteration links each Position to its parent;
        // and allows findRepetition() to operate correctly:
        //
        var child = Push();             // Push Position to make the moves
        try {
          child.resetMove();            // Usually called via [null|try]Move()
          child.lookupPV((Eval)(-mBeta), (Eval)(-mAlpha), moves);
        }
        finally {
          Pop(ref child);               // Pop Position
        }
      }
    }
  }

  public void AbbreviateRefresh(List<Move> moves, Int32 nMove, Int32 nDepth, Eval mValue) {
    if (moves.Count <= nMove) {
      //[Safe]lookupPV() should not be called if a draw is detected
      if (!IsDraw())
        lookupPV(MinusInfinity, PlusInfinity, moves);
    }
    else {
      var moveNoted = moves[nMove];
      if (!IsDefined(moveNoted)) {
        Debug.Assert(IsDefined(moveNoted), $"Undefined Move [{nameof(AbbreviateRefresh)}]");
        moveNoted = Move.NullMove;
      }
      else if (!State.IsPure)           // Standard Algebraic Notation (AN) supports abbreviation
        moves[nMove] = abbreviate(moveNoted);
#if DebugMove
      unpackMove1(moveNoted, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
      //unpackMove2(moveNoted, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
#if DebugMoveColor
      var bWTM = WTM();
      var bWhiteMove = moveNoted.Has(Move.WTM);
      if (bWTM != bWhiteMove) {
        Debug.Assert(bWTM == bWhiteMove, $"WTM != WhiteMove [{nameof(AbbreviateRefresh)}]");
      }
#endif
      const EvalType et = EvalType.Exact;
      if (moveNoted.Has(Move.Qxnt))
        storeQXP(mValue, et, moveNoted);
      else if (nDepth >= 0)
        storeXP((Depth)nDepth, mValue, et, moveNoted);

      var bLegal = tryOrSkip(ref moveNoted);
      Debug.Assert(bLegal, $"{nameof(AbbreviateRefresh)} obtained an Illegal Move");
#if TraceVal
      // CurrentMove set in [null|try]Move()
      if (IsTrace())
        DisplayCurrent(nameof(AbbreviateRefresh));
#endif
      SetDraw50();                      // Mark Draw50 after having made the move

      //
      // Recursion vs iteration links each Position to its parent;
      // and allows findRepetition() to operate correctly:
      //
      var child = Push();               // Push Position to make the moves
      try {
        child.resetMove();              // Usually called via [null|try]Move()
        child.AbbreviateRefresh(moves, nMove + 1, nDepth - 1, (Eval)(-mValue));
      }
      finally {
        Pop(ref child);                 // Pop Position
      }
    }
  }

  [Conditional("RefreshPV")]
  private void refreshPV(Depth wDepth) {
    var child = Push();                 // Push Position to make the moves
    try {
      var bWTM = WTM();
      for (var nLine = 0; nLine < State.VariationCount; nLine++) {
        var nVInverse = State.VariationCount - (nLine + 1);
        var vn = State.Variation[nVInverse];
        var mValue = ReflectValue(bWTM, vn.Value);
        if (vn.Moves != null) {
          child.resetMove();            // Usually called via [null|try]Move()
          child.AbbreviateRefresh(vn.Moves, 0, wDepth, mValue);
        }
      }
    }
    finally {
      Pop(ref child);                   // Pop Position
    }
  }
  #endregion                            // MultiPV Support

  #region Writer Methods
  private void writePV(StringBuilder sb, Int32 nLine, Boolean bWTM) {
    sb.WriteVariation(State.Variation[nLine], nLine, State.MultiPVLength > 1,
                      bWTM, GamePly, State.IsPure, Side, State.IsChess960)
      .FlushLine();
  }

  [Conditional("WritePV")]
  private void writeMultiPV() {
    var bWTM = WTM();
    var sb = new StringBuilder();
    for (var nLine = 0; nLine < State.VariationCount; nLine++)
      writePV(sb, nLine, bWTM);
  }

  public List<Move> MovesFromParent(Position? parent, Boolean bAbbreviate) {
    var moves = new List<Move>();
    for (var position = this;           // toPosition
         position is not null &&        //[Safe]
         !ReferenceEquals(position, parent);
         position = position.Parent) {
      var move = position.CurrentMove;
      if (!IsDefined(move)) {
        Debug.Assert(IsDefined(move), "Undefined CurrentMove");
      }
      var mov = (bAbbreviate && position.Parent is not null) ?
        position.Parent.abbreviate(move) : move;
      moves.Insert(0, mov);
    }
    return moves;
  }

  //
  // Displays current position and the moves leading up to it
  //
  protected void DisplayCurrent(String sLabel) {
    Display(sLabel);
    // The following invokes Position.MovesFromParent()
    var bAbbreviate = false;
    State.ListMovesFromRoot(this, State.IsPure, bAbbreviate);
  }
  #endregion                            // Writer Methods
}
