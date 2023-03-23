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
    public void InitNode() {
      //
      // The following are initialized for all nodes;
      // but moves are not made at the Root Position:
      //
      CurrentMove = Move.Undefined;
      pinnedPiece = 0UL;

      clrEval();

      if (Parent is null) {             // Root Position
        State.IsChess960 = false;       //[Init]
        GamePly = 0;                    //[Init]

        extensionCounts = 0;            //[Init]
        moveTypeOrdering = defaultMoveTypeOrdering;
      }
      else {
        //
        //[Note]All Move Commands call OnMoveCommand()
        // which calls IsLegal(), which requires WTM()
        //
        GamePly = Parent.GamePly;
#if InheritMoveTypes
        moveTypeOrdering = Parent.moveTypeOrdering;
#else
        MoveTypeOrdering = defaultMoveTypeOrdering;
#endif
      }
    }

    // Called by Push()
    public override void Clear() {
      base.Clear();
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
