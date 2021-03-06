﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2017-03-24 CNHume]Created Class as GoodMove
//

namespace Engine.MoveOrder {
  using static Board;
  using static Position;

  using System;

  //
  // Type Aliases:
  //
  using Depth = System.UInt16;
  using Eval = System.Int16;

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
      //if (obj is null) return 1;
      if (obj is GoodMove) {
        var gm = (GoodMove)obj;
        // Prefer greater Score
        var sense = Value.CompareTo(gm.Value);
        return sense;
      }
      else
        throw new ArgumentException("Object is not a GoodMove");
    }
    #endregion
  }
}
