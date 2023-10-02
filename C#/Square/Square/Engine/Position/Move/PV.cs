﻿//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2012-03-01 CNHume]Created File
//
//#define AbbreviateLookup
#define DebugMove
//#define DebugMoveIsLegal
//#define DebugSideToMove
//#define DebugPlace
#define FilterCandidates
//#define Magic
//#define TestBest
#define TestDraw3
//#define TraceVal

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;

using MoveOrder;

namespace Engine;

using static Engine.Board;
#if DebugPlace
using Command;                          // For UCI.IsDebug
#endif
using static Logging.Logger;

//
// Type Aliases:
//
using Depth = UInt16;
using Eval = Int16;
using Plane = UInt64;

partial class Position : Board {
  #region Test Methods
  [Conditional("DebugSideToMove")]
  private void verifySideToMove(Move move, String? methodName = null) {
    if (!IsDefinite(move)) return;
    var bWTM = WTM();
    var bWhiteMove = move.Has(Move.WTM);
    if (bWTM != bWhiteMove) {
      var message = $"WTM != WhiteMove [{methodName}]";
      Debug.Assert(bWTM == bWhiteMove, message);
      DisplayCurrent(message);
    }
  }

  [Conditional("DebugMoveIsLegal")]
  private void verifyMoveIsLegal(Move move, String? methodName = null) {
    if (!IsDefinite(move)) return;
    var child = Push();                 // Push Position to make the moves
    try {
      var mov = move;
      var bValid = child.tryOrSkip(ref mov);

      if (!bValid) {
        unpackMove1(mov, out Sq sqFrom, out Sq sqTo,
                    out Piece piece, out Piece promotion, out Boolean bCapture);
        child.Display($"Illegal Move from {sqFrom} to {sqTo} [{methodName}]");
      }
    }
    finally {
      Pop(ref child);                   // Pop Position used for this Ply
    }
  }
  #endregion                            // Test Methods

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
    return CurrentMove = move;
  }

  private Move annotateFinal(Move move) {
    if (IsFinal()) move |= Move.NoteFinal;
#if TestDraw3
    if (IsDraw()) move |= Move.NoteDraw;
#endif
    return CurrentMove = move;
  }

  private Move abbreviate(Move move) {
    const String methodName = nameof(abbreviate);
    if (IsNullMove(move) || IsUndefined(move))  //[Safe]
      return move;

    unpack1(move, out Int32 nFrom, out Int32 nTo,
            out UInt32 uPiece, out Boolean bCapture);

    var piece = (Piece)uPiece;

    verifySideToMove(move, methodName);

    var qpAtxTo = PieceAtxTo(nFrom, nTo, piece, bCapture);

    //[Conditional]
    filterCandidates(move, ref qpAtxTo);

    if (qpAtxTo == 0) {
      var sAction = bCapture ? "capture" : "move";
      var message = $"There is no piece that can {sAction} from {(Sq)nFrom} to {(Sq)nTo} [{methodName}]";
      Debug.Assert(qpAtxTo != 0, message);
      Display(message);
    }

    //
    // qpAtxTo holds Pieces of the appropriate type which "attack" nTo.
    //
    if (piece == Piece.P && bCapture)
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
        if (!child.tryCandidate(moveTo | MoveFrom(n)))
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
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void addVariationMove(List<Move> moves, Move move, String? methodName = default) {
    verifySideToMove(move, methodName);
    moves.Add(move);
  }

#if TestBest
  private Eval addPV(Eval mAlpha, Eval mValue, Move move, List<BestMove> bestLine, Depth wDepth) {
#else
  private Eval addPV(Eval mAlpha, Eval mValue, Move move, List<Move> bestLine) {
#endif
    const String methodName = nameof(addPV);
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

      var vnMoves = vn[nFinal].Moves;

      if (vnMoves == null) {
        vnMoves = new List<Move>();
        vn[nFinal].Moves = vnMoves;
      }
      else
        vnMoves.Clear();

      if (IsUndefined(move)) {
        Debug.Assert(IsDefined(move), $"Undefined Move [{methodName}]");
        move = Move.NullMove;           //[Safe]
      }

      addVariationMove(vnMoves, move, methodName);

      var bPonder = bestLine.Count > 0;
      if (bPonder) {
#if TestBest
        var moves = bestLine.Select(x => x.Move);
        vnMoves.AddRange(moves);
#else
        vnMoves.AddRange(bestLine);
#endif
        //!child.IsFinal()
        //[ToDo]Verify bestLine here
#if TestBest
        if (wDepth == 13) {
          DisplayCurrent(methodName);
          var sb = new StringBuilder();
          State.MovePosition?.writePV(sb, nFinal, WTM());
        }
#endif
      }

      //
      // Insert at correct position:
      //
      var bWTM = WTM();
      var nPlace = vn.Insert(nFinal);
      if (nPlace == 0) {
        var sb = new StringBuilder();
        var mEval = ReflectValue(bWTM, mValue);
        sb.UpdateBestInfo(State.BestLine, vnMoves, mEval, bPonder, Side, State.IsChess960)
          .FlushLine();
      }
#if DebugPlace
      if (UCI.IsDebug) {
        var sb = new StringBuilder();
        var sGrow = bGrow ? "Placed" : "Replaced";
        sb.AppendFormat($"{sGrow} vn[{nPlace}]");
        LogInfo(LogLevel.note, sb.ToString());
        sb.Clear();
        State.MovePosition?.writePV(sb, nPlace, bWTM);
      }
#endif
    }

    //
    // Avoid raising Alpha until we have a candidate for the weakest Variation:
    //
    return bRoomToGrow ?
      mAlpha : bHasValue ? vn[nFinal].Value : mValue;
  }

  private void lookupPV(List<Move> vnMoves, Eval mAlpha, Eval mBeta) {
    const String methodName = nameof(lookupPV);
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
      unpackMove1(moveNoted, out Sq sqFrom, out Sq sqTo,
                  out Piece piece, out Piece promotion, out Boolean bCapture);
      //unpackMove2(moveNoted, out Sq sqFrom, out Sq sqTo,
      //            out Piece piece, out Piece promotion, out Piece capture,
      //            out Boolean bCastles, out Boolean bCapture);
#endif
      addVariationMove(vnMoves, moveNoted, methodName);

      var bLegal = tryOrSkip(ref moveNoted);
      if (!bLegal) {
        const String message = $"Illegal Move [{methodName}]";
        Debug.Assert(bLegal, message);
        Display(message);
      }
#if TraceVal
      // CurrentMove set in [null|try]Move()
      if (IsTrace())
        DisplayCurrent(methodName);
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
          child.lookupPV(vnMoves, (Eval)(-mBeta), (Eval)(-mAlpha));
        }
        finally {
          Pop(ref child);               // Pop Position
        }
      }
    }
  }

  private void abbreviateRefresh(
    List<Move> vnMoves, Int32 nMove, Int32 nDepth, Eval mValue) {
    const String methodName = nameof(abbreviateRefresh);
    resetMove();                        // Usually called via [null|try]Move()
    if (vnMoves.Count <= nMove) {
      //[Safe]lookupPV() should not be called if a draw is detected
      if (!IsDraw())
        lookupPV(vnMoves, MinusInfinity, PlusInfinity);
    }
    else {
      var moveNoted = vnMoves[nMove];
      if (IsUndefined(moveNoted)) {
        const String message = $"Undefined Move [{methodName}]";
        Debug.Assert(IsDefined(moveNoted), message);
        moveNoted = Move.NullMove;
      }
      else if (!State.IsPure) {         // Standard Algebraic Notation (AN) supports abbreviation
        //[Debug]moveNoted illegal here!
        //[Conditional]
        verifyMoveIsLegal(moveNoted, methodName);
        vnMoves[nMove] = abbreviate(moveNoted);
      }
#if DebugMove
      unpackMove1(moveNoted, out Sq sqFrom, out Sq sqTo,
                  out Piece piece, out Piece promotion, out Boolean bCapture);
      //unpackMove2(moveNoted, out Sq sqFrom, out Sq sqTo,
      //            out Piece piece, out Piece promotion, out Piece capture,
      //            out Boolean bCastles, out Boolean bCapture);
#endif
      verifySideToMove(moveNoted, methodName);

      const EvalType et = EvalType.Exact;
      if (moveNoted.Has(Move.Qxnt))
        storeQXP(mValue, et, moveNoted);
      else if (nDepth >= 0)
        storeXP((Depth)nDepth, mValue, et, moveNoted);

      var bLegal = tryOrSkip(ref moveNoted);
      if (!bLegal) {
        const String message = $"Illegal Move [{methodName}]";
        Debug.Assert(bLegal, message);
        Display(message);
      }
#if TraceVal
      // CurrentMove set in [null|try]Move()
      if (IsTrace())
        DisplayCurrent(nameof(abbreviateRefresh));
#endif
      SetDraw50();                      // Mark Draw50 after having made the move

      //
      // Recursion vs iteration links each Position to its parent;
      // and allows findRepetition() to operate correctly:
      //
      var child = Push();               // Push Position to make the moves
      try {
        child.abbreviateRefresh(vnMoves, nMove + 1, nDepth - 1, (Eval)(-mValue));
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
          child.abbreviateRefresh(vn.Moves, 0, wDepth, mValue);
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
    const String methodName = nameof(MovesFromParent);
    var moves = new List<Move>();
    for (var position = this;           // toPosition
         position is not null &&        //[Safe]
         !ReferenceEquals(position, parent);
         position = position.Parent) {
      var move = position.CurrentMove;
      if (IsUndefined(move)) {
        Debug.Assert(IsDefined(move), $"CurrentMove Undefined [{methodName}]");
      }

      if (bAbbreviate && position.Parent is not null) {
        //[Conditional]
        position.Parent.verifyMoveIsLegal(move);
        var mov = position.Parent.abbreviate(move);
        moves.Insert(0, mov);
      }
      else
        moves.Insert(0, move);
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
