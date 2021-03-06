﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split Move Processor into its own file
//
// Conditionals:
//
//#define TestHash                        //[Enable]testHash() Conditional in Hash.cs
//#define TurnTest
//#define DebugDraw2
//#define DebugNodeTotal
//#define DisplayPosition

namespace Engine {
  using Exceptions;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Text;

  partial class Position : Board {
    #region Move Processor
    // ~32 MHz on old PC
    protected void resetMove() {
      if (Parent is null)
        throw new PositionException("resetMove() called at the Root Position");

      Parent.CopyTo(this);
      resetEP();                      // Clear En Passant state just prior to making a move
#if DebugNodeTotal
      var qNodeTotal = State.MoveTotal + State.NullMoveTotal;
      if (State.NodeTotal != qNodeTotal) {
        Debug.Assert(State.NodeTotal == qNodeTotal, "NodeTotal != MoveTotal + NullMoveTotal");
        DisplayCurrent("resetMove()");
      }
#endif
    }

    // ~2.3 MHz: slowed mostly by IsLegal() and only slightly by resetMove()
    protected Boolean tryMove(ref Move move, Boolean bFindRepetition = true, Boolean bQxnt = false) {
      CurrentMove = move;               // Current Pseudo Move

      var nFrom = from(move);
      var nTo = to(move);

      var bPinned = (PinnedPiece & BIT0 << nFrom) != 0;
      var bSkip = bPinned && (Restricted[nFrom] & BIT0 << nTo) == 0;
      if (bSkip) { // Skip moves which are in violation of a pin
        GameState.AtomicIncrement(ref State.PinSkipTotal);
        return false;
      }
      else {
        //[Timer]timeMove(move);
        //
        //[Note]resetMove() is called here because it is needed for every subsequent move.  This leaves
        // a window between the time initNode() initializes a node and when resetMove() is first called.
        //
        resetMove();
        this.move(ref move);

        if (IsFence()) clrEval();       // Captures and Pawn moves invalidate staticEval()
        //[Note]If En Passant was possible, any move ends a Transposition Group
        if (Parent.IsPassed()) setFence();

        testHash();                     // Conditional
        var bLegal = IsLegal(bFindRepetition, bPinned);
        if (isDefined(move)) {
          if (bLegal)
            CurrentMove = move = annotateEarly(move);
          else
            restrictPiece(move);
        }

        State.IncMove(bLegal, bQxnt);
        State.MonitorBound(this);       // Pass position so heartbeat() can build getCurrentMoves()
        return bLegal;
      }
    }

    protected Boolean nullMove() {
      CurrentMove = Move.NullMove;      // Current Pseudo Move
      resetMove();
      skipTurn();

      //[Note]If En Passant was possible, any move ends a Transposition Group
      if (Parent.IsPassed()) setFence();
      testHash();                       // Conditional
      var bLegal = !InCheck();
      if (!bLegal)
        throw new BoardException("Illegal Null Move");

      GameState.AtomicIncrement(ref State.NullMoveTotal);
      State.MonitorBound(this);
      return bLegal;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Boolean tryOrSkip(ref Move move) {
      return isNullMove(move) ? nullMove() : tryMove(ref move);
    }

    // Called whenever a move is tried to detect Checks and to set Draw Flags
    public Boolean IsLegal(Boolean bFindRepetition = false, Boolean bRestricted = false) {
      var bWTM = WTM();
#if TurnTest
      var bWhiteMoved = !bWTM;
      var bWhiteTurn = IsOdd(GamePly);
      Debug.Assert(bWhiteMoved == bWhiteTurn, "Skipped Turn");
#endif
      (BoardSide friend, BoardSide foe) = getSides(bWTM);

      //[Assume]Restricted Moves are Legal
      var bLegal = bRestricted || !isAttacked(friend, King & foe.Piece);
      setLegal(bLegal);

      if (bLegal) {                     // Perform InCheck Test for Legal Moves
        var bInCheck = isAttacked(foe, King & friend.Piece);
        setInCheck(bInCheck);

        //
        // Draw Flags are included in dynamicHash() to maintain Search Stability,
        // because they influence eval().  They will be needed by probeXP().
        //
        setInsufficient();

        if (bFindRepetition)
          findRepetition();
        else
          clrRepetition();
      }
#if DisplayPosition
      var sb = new StringBuilder();
      Display(sb);
#endif
      return bLegal;
    }
    #endregion

    #region Draw By Repetition
    protected void findRepetition() {
      GameState.AtomicIncrement(ref State.RepetitionSearches);
      clrRepetition();
      if (IsFence()) return;
      var bNullMade = IsNullMade();

      //
      //[Note]This search for 3-fold repetition extends to the initial position;
      // not just to State.MovePosition where the current search began.
      //
      for (var position = Parent; position is not null; position = position.Parent) {
        GameState.AtomicIncrement(ref State.RepetitionPlies);

        //
        // Include Positions for both sides, stopping when the Transposition Group ends.
        //
        if (Equals(position)) {
          if (bNullMade)
            //
            // Null Moves do not count as repetition of the position; but the
            // Draw3 and Draw2 flags are copied to expedite subsequent search:
            //
            FlagsDraw |= position.fdr();
          else
            setRepetition(position.fdr() != 0);

          break;
        }
        else if (position.IsFence())
          break;                        // End of Transposition Group
      }
#if DebugDraw2
      validateDraw2();
#endif
    }
#if DebugDraw2
    protected Boolean validateDraw2() {
      var bDraw2 = fdr() != 0;

      if (bDraw2) {
        var nCount = 1;

        for (var position = Parent; position is not null; position = position.Parent) {
          if (Equals(position)) nCount++;
        }

        var bValid = nCount > 1 ? bDraw2 : !bDraw2;

        if (!bValid)
          DisplayCurrent("validateDraw2()");

        return bValid;
      }
      else
        return true;
    }
#endif
    #endregion
  }
}
