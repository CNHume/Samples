//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2011-05-02 CNHume]Created Class
//
// Conditionals:
//

namespace Engine.MoveOrder {
  using static Board;

  using System;
  using System.Collections.Generic;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;

  class Variation : IComparable {
    #region Constructors
    public Variation(Eval mValue, List<Move> moves) {
      Value = mValue;
      Moves = moves;
    }

    public Variation() {
    }
    #endregion

    #region Fields
    public List<Move> Moves;
    public Eval Value;
    #endregion

    #region IComparable Interface Methods
    // Returns -1: if this < obj, 1: if this > obj, else 0: if this == obj
    public Int32 CompareTo(Object obj) {
      //if (obj is null) return 1;
      var variation = obj as Variation;

      if (variation is null)
        throw new ArgumentException("Object is not a Variation");

      return Value.CompareTo(variation.Value);
    }
    #endregion
  }
}
