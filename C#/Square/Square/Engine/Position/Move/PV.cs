//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2012-03-01 CNHume]Created File
//
//#define AbbreviateLookup
//#define DebugMoveIsLegal
//#define DebugSideToMove
//#define DebugPlace
//#define Magic
#define FilterCandidates
#define PreventLookupCycle
//#define TestBest
#define TestDraw3
//#define TraceVal

using System.Diagnostics;
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
  private void illegalMove(Move move, String? methodName) {
    unpackMove2(move, out Sq sqFrom, out Sq sqTo,
                out Piece piece, out Piece promotion, out Piece capture,
                out Boolean bCastles, out Boolean bCapture);
    var message = $"Illegal Move from {sqFrom} to {sqTo} [{methodName}]";
    Debug.Assert(false, message);
    Display(message);
  }

  [Conditional("DebugMoveIsLegal")]
  private void verifyMoveIsLegal(Move move, String? methodName = null) {
    if (IsIndefinite(move)) return;
    var child = Push();                 // Push Position to make the moves
    try {
      var mov = move;
      if (!child.tryOrSkip(ref mov)) {
        child.illegalMove(mov, methodName);
      }
    }
    finally {
      Pop(ref child);                   // Pop Position used for this Ply
    }
  }

  [Conditional("DebugSideToMove")]
  private void verifySideToMove(Move move, String? methodName = null) {
    if (IsIndefinite(move)) return;
    var bWTM = WTM();
    var bWhiteMove = move.Has(Move.WTM);
    if (bWTM != bWhiteMove) {
      var message = $"WTM != WhiteMove [{methodName}]";
      Debug.Assert(bWTM == bWhiteMove, message);
      Display(message);
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
    if (IsNullMove(move) || IsIndefinite(move)) //[Safe]
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
        var moveFrom = moveTo | MoveFrom(n);
        if (!child.tryCandidate(moveFrom))
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

      if (IsIndefinite(move)) {
        Debug.Assert(IsDefinite(move), $"Indefinite Move [{methodName}]");
        move = Move.NullMove;           //[Safe]
      }

      verifySideToMove(move, methodName);
      vnMoves.Add(move);

      var bPonder = bestLine.Count > 0;
      if (bPonder) {
#if TestBest
        var enBME = bestLine.Select(x => x.BME);
        var enFEN = bestLine.Select(x => x.FEN);
        var enMove = bestLine.Select(x => x.Move);
        vnMoves.AddRange(enMove);
#else
        vnMoves.AddRange(bestLine);
#endif
        // Verify vnMoves
        replay(vnMoves);                //[Conditional]
#if TestBest
        if (wDepth == 13) {
          DisplayCurrent(methodName);
          var sb = new StringBuilder();
          State.MovePosition?.writePV(sb, nFinal, WTM());
        }
#endif
      }

      #region Insert Variation
      //
      // Insert relies on IComparable which is based on Variation.Value
      //
      var nPlace = vn.Insert(nFinal);
      if (nPlace == 0) {
        var sb = new StringBuilder();
        var mEval = ReflectValue(WTM(), mValue);
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
        State.MovePosition?.writePV(sb, nPlace, WTM());
      }
#endif
      #endregion                        // Insert Variation
    }

    //
    // Avoid raising Alpha until we have a candidate for the weakest Variation:
    //
    var bRoomToGrow = State.VariationCount < State.MultiPVLength;
    return bRoomToGrow ?
      mAlpha : bHasValue ? vn[nFinal].Value : mValue;
  }

  private void lookupPV(List<Move> vnMoves, Eval mAlpha, Eval mBeta) {
    const String methodName = nameof(lookupPV);

    //[Note]LoadMove() and abbreviate() require the parent position to be supplied by resetMove():
    probeMove(mAlpha, mBeta, out Move moveFound);

    // Avoid infinite recursion due to a sequence of two Null Moves in a row:
    if (IsNullMove(moveFound) && Parent is not null && Parent.IsNullMade())
      return;

    // moveFound not always defined for EvalType.Upper [Fail Low]
    //[Safe]Also prevent unexpected EmptyMove
    if (IsIndefinite(moveFound)) return;

    var moveNoted = moveFound;
    if (!State.IsPure) {                // Standard Algebraic Notation (AN) supports abbreviation
#if AbbreviateLookup
      moveNoted = abbreviate(moveNoted);
#else
      moveNoted &= ~Move.HideFrom;      // Suppress abbreviation to indicate moves recovered by probeMove()
#endif
    }

    verifySideToMove(moveNoted, methodName);
    vnMoves.Add(moveNoted);

    // CurrentMove set in [null|try]Move()
    if (!tryOrSkip(ref moveNoted)) {
      illegalMove(moveNoted, methodName);
      return;
    }
#if PreventLookupCycle
    const Boolean bLookupCycle = true;
    var position = findRepetition1(bLookupCycle);
    if (position is not null) {
      DisplayCurrent($"Found Lookup Cycle [{methodName}]");
      return;
    }
#endif
#if TraceVal
    if (IsTrace())
      DisplayCurrent(methodName);
#endif
    Test50MoveRule();                   // Set Draw50 after making the move

    //[Note]Terminate recursion if Draw3 set.
    if (!IsDraw()) {
      //
      // Recursion vs iteration links each Position to its parent,
      // allowing findRepetition() and findCycle() to operate correctly:
      //
      var child = Push();               // Push Position to make the moves
      try {
        child.resetMove();              // Usually called via [null|try]Move()
        child.lookupPV(vnMoves, (Eval)(-mBeta), (Eval)(-mAlpha));
      }
      finally {
        Pop(ref child);                 // Pop Position
      }
    }
  }

  [Conditional("DebugMoveIsLegal")]
  private void replay(List<Move> moves, Int32 nIndex = 0) {
    const String methodName = nameof(replay);
    if (moves.Count <= nIndex)
      return;

    var move = moves[nIndex];
    if (IsDefinite(move)) {             //[Safe]Also prevent unexpected EmptyMove
      var moveNoted = move;
      if (!State.IsPure) {              // Standard Algebraic Notation (AN) supports abbreviation
#if AbbreviateLookup
        moveNoted = abbreviate(moveNoted);
#else
        moveNoted &= ~Move.HideFrom;    // Suppress abbreviation to indicate moves recovered by probeMove()
#endif
      }

      //[Note]Terminate recursion if Draw3 set.
      if (!IsDraw()) {
        //
        // Recursion vs iteration links each Position to its parent,
        // allowing findRepetition() and findCycle() to operate correctly:
        //
        var child = Push();             // Push Position to make the moves
        try {
          if (!child.tryOrSkip(ref moveNoted)) {
            child.illegalMove(moveNoted, methodName);
            return;
          }

          child.Test50MoveRule();       // Set Draw50 after making the move
          child.replay(moves, nIndex + 1);
        }
        finally {
          Pop(ref child);               // Pop Position
        }
      }
    }
  }

  private void abbreviateRefresh(
    List<Move> vnMoves, Int32 nDepth, Eval mValue, Int32 nIndex = 0) {
    const String methodName = nameof(abbreviateRefresh);
    resetMove();                        // Usually called via [null|try]Move()
    if (vnMoves.Count <= nIndex) {
      //[Safe]lookupPV() should not be called if a draw is detected
      if (!IsDraw())
        lookupPV(vnMoves, MinusInfinity, PlusInfinity);
    }
    else {
      var moveNoted = vnMoves[nIndex];
      if (IsIndefinite(moveNoted)) {
        var message = $"Indefinite Move, vnMoves.Count = {vnMoves.Count} [{methodName}]";
        Debug.Assert(IsDefinite(moveNoted), message);
        return;
      }
      else if (!State.IsPure) {         // Standard Algebraic Notation (AN) supports abbreviation
        //[Debug]moveNoted illegal here!
        //[Conditional]
        verifyMoveIsLegal(moveNoted, methodName);
        vnMoves[nIndex] = abbreviate(moveNoted);
      }

      verifySideToMove(moveNoted, methodName);

      const EvalType et = EvalType.Exact;
      if (moveNoted.Has(Move.Qxnt))
        storeQXP(mValue, et, moveNoted);
      else if (nDepth >= 0)
        storeXP((Depth)nDepth, mValue, et, moveNoted);

      if (!tryOrSkip(ref moveNoted)) {
        illegalMove(moveNoted, methodName);
        return;
      }
#if TraceVal
      // CurrentMove set in [null|try]Move()
      if (IsTrace())
        DisplayCurrent(nameof(abbreviateRefresh));
#endif
      Test50MoveRule();                 // Set Draw50 after making the move

      //
      // Recursion vs iteration links each Position to its parent,
      // allowing findRepetition() and findCycle() to operate correctly:
      //
      var child = Push();               // Push Position to make the moves
      try {
        child.abbreviateRefresh(vnMoves, nDepth - 1, (Eval)(-mValue), nIndex + 1);
      }
      finally {
        Pop(ref child);                 // Pop Position
      }
    }
  }

  [Conditional("RefreshPV")]
  private void refreshPV(Depth wDepth) {
    const String methodName = nameof(refreshPV);
    var child = Push();                 // Push Position to make the moves
    try {
      var bWTM = WTM();
      for (var nLine = 0; nLine < State.VariationCount; nLine++) {
        var nVInverse = State.VariationCount - (nLine + 1);
        var vn = State.Variation[nVInverse];
        var mValue = ReflectValue(bWTM, vn.Value);
        if (vn.Moves == null) {
          var message = $"Variation[{nVInverse}].Moves Null [{methodName}]";
          Debug.Assert(vn.Moves != null, message);
        }
        else
          child.abbreviateRefresh(vn.Moves, wDepth, mValue);
      }
    }
    finally {
      Pop(ref child);                   // Pop Position
    }
  }

  private void writePV(StringBuilder sb, Int32 nLine, Boolean bWTM) {
    sb.WriteVariation(State.Variation[nLine], nLine, State.MultiPVLength > 1,
                      bWTM, GamePly, State.IsPure, Side, State.IsChess960)
      .FlushLine();
  }

  [Conditional("WriteMultiPV")]
  private void writeMultiPV() {
    var bWTM = WTM();
    var sb = new StringBuilder();
    for (var nLine = 0; nLine < State.VariationCount; nLine++)
      writePV(sb, nLine, bWTM);
  }
  #endregion                            // MultiPV Support

  #region Move List Methods
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
        break;
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

  public void ListMovesFromParent(
    Position? parent, Boolean bPure, Boolean bChess960, Boolean bAbbreviate = false) {
    var moves = MovesFromParent(parent, bAbbreviate);
    var sb = new StringBuilder();
    var wGamePly = parent?.GamePly ?? 0;
    sb.WriteMoves(moves, wGamePly, Side, bPure, bChess960)
      .FlushLine();
  }

  //
  // Displays current position and the moves leading up to it
  //
  protected void DisplayCurrent(String sLabel) {
    Display(sLabel);
    // The following invokes MovesFromParent()
    ListMovesFromParent(State.RootPosition, State.IsPure, State.IsChess960);
  }
  #endregion                            // Move List Methods
}
