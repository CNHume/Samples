//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
#define InheritMoveTypes

namespace Engine {
  partial class Position : Board {
    #region Workspace Methods
    private void initNode() {
      if (Parent is null) {             // Root Position
        State!.IsChess960 = false;      //[Init]

        GamePly = 0;                    //[Init]
        NullPly = 0;
        FlagsTurn = default;            //[Safe]
        FlagsGame = default;            //[Safe]
        FlagsDraw = default;            //[Safe]
        FlagsMode = default;            //[Safe]
        foreach (var side in Side)
          side.FlagsSide = default;     //[Safe]

        //
        // Initialize Extension Counts at the Root:
        //
        extensionCounts = 0;

        moveTypeOrdering = defaultMoveTypeOrdering;
      }
      else {
        //
        // Ensure that FlagsTurn starts with Parent WTM.  This allows
        // IsLegal() to ascertain whether tryMove() was ever called:
        //
        //[Note]Flags will be reset when resetMove() calls CopyTo()
        //
        CopyFlags(Parent);
#if InheritMoveTypes
        moveTypeOrdering = Parent.moveTypeOrdering;
#else
        MoveTypeOrdering = defaultMoveTypeOrdering;
#endif
      }

      //
      // The following are initialized for all nodes;
      // but moves are not made at the Root Position:
      //
      CurrentMove = Move.Undefined;
      pinnedPiece = 0UL;
      AttackedSum =
        BlackControlled =
        WhiteControlled = 0UL;

      HalfMoveClock = 0;
      clrEval();
      Name = default;
    }

    // Called by Push()
    public override void Clear() {
      base.Clear();
      initNode();
    }
    #endregion

    #region Wrapper Methods for State.Push() and State.Pop()
    public Position Push() {
      return State!.Push(this);
    }

    public void Pop(ref Position child) {
      child.State!.Pop(ref child);
    }
    #endregion
  }
}
