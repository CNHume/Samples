//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
#define InheritMoveTypes

namespace Engine {
  using System;

  using static MoveOrder.TypedMove;

  //
  // Type Aliases:
  //
  using ExtensionCounter = UInt16;

  partial class Position : Board {
    #region Workspace Methods
    protected void initNode() {
      if (Parent is null) {
        GamePly = 0;                    //[Init]
        NullPly = 0;
        FlagsTurn = default;            //[Safe]These are reinitialized when ParseEPD|FEN() call InitPosition()
        foreach (var side in Side)
          side.FlagsSide = default;     //[Safe]
        FlagsEG = default;              //[Safe]
        FlagsDraw = default;            //[Safe]
        FlagsMode = default;            //[Safe]

        //
        // Initialize Extension Counts at the Root
        //
        ExtensionCounts = 0;
        MoveTypeOrdering = DefaultMoveTypeOrdering;
      }
      else {
        //
        // Ensure that FlagsTurn starts with Parent WTM.  This allows
        // IsLegal() to ascertain whether tryMove() was ever called:
        //
        //[Note]Flags will be reset when resetMove() calls CopyTo()
        //
        Parent.CopyFlagsTo(this);
#if InheritMoveTypes
        MoveTypeOrdering = Parent.MoveTypeOrdering;
#else
        MoveTypeOrdering = DefaultMoveTypeOrdering;
#endif
      }

      //
      // The following are initialized for all nodes;
      // but moves are not made at the Root Position:
      //
      CurrentMove = Move.Undefined;
      PinnedPiece = 0UL;
      AttackedSum =
        BlackControlled =
        WhiteControlled = 0UL;

      HalfMoveClock = 0;
      clrEval();
      Name = default;
    }

    public override void Clear() {      // Called after any setPosition()
      base.Clear();
      initNode();
    }
    #endregion

    #region Wrapper Methods for State.Push() and State.Pop()
    public Position Push() {
      return State.Push(this);
    }

    public void Pop(ref Position child) {
      child.State.Pop(ref child);
    }
    #endregion
  }
}
