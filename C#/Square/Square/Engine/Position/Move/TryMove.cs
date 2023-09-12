//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split Move Processor into its own file
//
// Conditionals:
//
//#define DebugDraw2
//#define DebugNodeTotal
//#define DisplayPosition
//#define TimePlayMove
//#define TestHash
//#define TurnTest

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;

namespace Engine;

using Exceptions;

partial class Position : Board {
  #region Move Processor
  // ~32 MHz on old PC
  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void resetMove() {
    if (Parent is null)
      throw new PositionException("resetMove() called at the Root Position");
    Copy(Parent);

    // Reset En Passant state just prior to calling ExecuteMove()
    ResetEP();
#if DebugNodeTotal
    var qNodes = State.TotalMoves + State.NullMoves;
    if (State.Nodes != qNodes) {
      Debug.Assert(State.Nodes == qNodes, "Nodes != TotalMoves + NullMoves");
      DisplayCurrent(nameof(resetMove));
    }
#endif
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Boolean tryCandidate(Move move) {
    resetMove();
    PlayMove(ref move);
    IncrementGamePly();
    var bLegal = IsLegal();
    State.IncMove(bLegal);              // Account for overhead
    return bLegal;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Boolean tryOrSkip(ref Move move) {
    return IsNullMove(move) ? nullMove() : tryMove(ref move);
  }

  // ~2.3 MHz: slowed mostly by IsLegal() and only slightly by resetMove()
  private Boolean tryMove(ref Move move, Boolean bFindRepetition = true, Boolean bQxnt = false) {
    CurrentMove = move;                 // Current Pseudo Move
    var (bPrevented, bRestricted) = isPinned(move);
    if (bPrevented) {                   // Skip moves which violate known pins
      GameState.AtomicIncrement(ref State.PinSkipTotal);
      return false;
    }

    //
    //[Note]resetMove() is called here because it is needed for every subsequent move.  This leaves
    // a window between the time initNode() initializes a node and when resetMove() is first called.
    //
    timeExecuteMove(move);              //[Conditional]
    resetMove();
    ExecuteMove(ref move);

    if (IsDraw0())
      clrEval();                        // Captures and Pawn moves invalidate staticEval()

    expireEnPassant();
#if TestHash
    if (!TestHash())
      DisplayCurrent(nameof(tryMove));
#endif
    var bLegal = IsLegal(bFindRepetition, bRestricted);
    if (IsDefined(move)) {
      if (bLegal)
        move = annotateEarly(move);
      else
        restrictPiece(move);
    }

    State.IncMove(bLegal, bQxnt);
    State.MonitorHeartbeat(this);
    return bLegal;
  }

  private Boolean nullMove() {
    CurrentMove = Move.NullMove;        // Current Pseudo Move
    resetMove();
    SkipTurn();

    expireEnPassant();
#if TestHash
    if (!TestHash())
      DisplayCurrent(nameof(nullMove));
#endif
    var bInCheck = InCheck();
    if (bInCheck)
      throw new BoardException("Illegal Null Move");

    State.IncNullMove();
    State.MonitorHeartbeat(this);
    return !bInCheck;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private (Boolean bPrevented, Boolean bRestricted) isPinned(Move move) {
    var nFrom = From(move);
    var nTo = To(move);

    var bRestricted = (pinnedPiece & bit(nFrom)) != 0;
    var bPrevented = bRestricted && (restricted[nFrom] & bit(nTo)) == 0;
    return (bPrevented, bRestricted);
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void expireEnPassant() {
    //
    //[Note]Any move begins a new Repetition Cycle when En Passant was possible
    // because the right to En Passant expires whether it is exercised or not.
    //
    if (Parent is not null && Parent.IsEPLegal())
      SetDraw0();
  }

  // IsLegal() detects Checks and sets Draw Flags when moves are tried.
  //[Note]IncrementGamePly() inverts the sense of Friend and Foe.
  public Boolean IsLegal(Boolean bFindRepetition = false, Boolean bRestricted = false) {
#if TurnTest
    var bWhiteMoved = !WTM();
    var bBlackTurn = IsOdd(GamePly);
    Debug.Assert(bWhiteMoved == bBlackTurn, "Skipped Turn");
#endif
    //[Assume]restricted Moves are Legal
    var bLegal = bRestricted || !Friend.IsAttacked(King & Foe.Piece);
    SetLegal(bLegal);

    if (bLegal) {                       // Perform InCheck Test for Legal Moves
      SetInCheck(Foe.IsAttacked(King & Friend.Piece));

      //
      // Draw Flags are included in dynamicHash() to maintain Search Stability,
      // because they influence eval().  They are also needed by probeXP().
      //
      SetDrawIM();

      ClrRepetition();
      if (bFindRepetition)
        findRepetition();
    }
#if DisplayPosition
    var sb = new StringBuilder();
    Display(sb);
#endif
    return bLegal;
  }
  #endregion

  #region Draw By Repetition
  private void findRepetition() {
    GameState.AtomicIncrement(ref State.RepetitionSearches);
    if (IsDraw0()) return;
    var bNullMade = IsNullMade();

    //
    //[Note]This search for 3-fold repetition extends to the initial position;
    // not just to State.MovePosition where the current search began.
    //
    for (var position = Parent; position is not null; position = position.Parent) {
      GameState.AtomicIncrement(ref State.RepetitionPlies);

      //
      // Include Positions for both sides, stopping when the Repetition Cycle ends.
      //
      if (Equals(position)) {
        if (bNullMade)
          //
          // Null Moves do not count as repetition of the position; but the
          // Draw3 and Draw2 flags are copied to expedite subsequent search:
          //
          FlagsDraw |= position.fdraw();
        else
          SetRepetition(position.fdraw() != 0);

        break;
      }
      else if (position.IsDraw0())
        break;                          // End of Repetition Cycle
    }
#if DebugDraw2
    validateDraw2();
#endif
  }
#if DebugDraw2
  private Boolean validateDraw2() {
    var bDraw2 = fdraw() != 0;

    if (bDraw2) {
      var nCount = 1;

      for (var position = Parent; position != null; position = position.Parent) {
        if (Equals(position)) nCount++;
      }

      var bValid = nCount > 1 ? bDraw2 : !bDraw2;

      if (!bValid)
        DisplayCurrent(nameof(validateDraw2));

      return bValid;
    }
    else
      return true;
  }
#endif                                  // DebugDraw2
  #endregion                            // Draw By Repetition
}
