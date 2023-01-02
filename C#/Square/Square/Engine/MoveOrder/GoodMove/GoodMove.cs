//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2017-03-24 CNHume]Created Class as GoodMove
//

namespace Engine.MoveOrder {
  using System;

  using static Board;
  using static Position;

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Eval = Int16;

  struct GoodMove : IComparable {
    #region Fields
    public Move Move;
    public Eval Value;
    public EvalType Type;
    public Depth Depth;
    #endregion

    #region Constructors
    public GoodMove(Move move,
      Depth wDepth = 0,
      Eval mValue = EvalUndefined,
      EvalType et = EvalType.Undefined) {
      Move = move;
      Value = mValue;
      Type = et;
      Depth = wDepth;
    }

    //
    // Copy Constructor:
    //
    public GoodMove(GoodMove gm) {
      // Value Type precludes use of gm.CopyTo(this) here:
      Move = gm.Move;
      Value = gm.Value;
      Type = gm.Type;
      Depth = gm.Depth;
    }

    //public void CopyTo(GoodMove gm) {
    //  gm.Move = Move;
    //  gm.Value = Value;
    //  gm.Type = Type;
    //  gm.Depth = Depth;
    //}
    #endregion

    #region IComparable Interface Methods
    // Returns -1: if this < obj, 1: if this > obj, else 0: if this == obj
    public Int32 CompareTo(Object obj) {
      //if (obj == null) return 1;
      if (obj is GoodMove) {
        var gm = (GoodMove)obj;
        // Prefer greater Score
        var sense = Value.CompareTo(gm.Value);
        return sense;
      }
      else
        throw new ArgumentException(nameof(obj), "Is not a GoodMove");
    }
    #endregion
  }
}
