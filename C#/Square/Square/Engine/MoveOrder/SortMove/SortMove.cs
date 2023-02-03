//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2017-03-24 CNHume]Renamed Class to SortMove
//[2012-03-28 CNHume]Created Class as GoodMove
//
// Conditionals:
//
#define StableSort

namespace Engine.MoveOrder {
  using static Board;
  using static Position;

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Eval = Int16;

  struct SortMove : IComparable {
    #region Fields
    public Move Move;
    public Int32 Index;
    public Eval Value;
    public Depth Depth;
    #endregion

    #region Constructors
    public SortMove(Move move, Int32 nIndex, Eval mValue = EvalUndefined, Depth wDepth = 0) {
      Move = move;
      Index = nIndex;
      Value = mValue;
      Depth = wDepth;
    }
    #endregion

    #region IComparable Interface Methods
    // Returns -1: if this < obj, 1: if this > obj, else 0: if this == obj
    public Int32 CompareTo(Object? obj) {
      //if (obj == null) return 1;
      if (obj is SortMove) {
        var sm = (SortMove)obj;
        // Prefer greater Score
        var better = Value.CompareTo(sm.Value);
#if StableSort
        // Prefer lesser Index
        var sense = better == 0 ? -Index.CompareTo(sm.Index) : better;
#else
        var sense = better;
#endif
        return sense;
      }
      else
        throw new ArgumentException("Object is not a SortMove");
    }
    #endregion
  }
}
