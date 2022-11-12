//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2010-11-26 CNHume]Created Class
//
// Conditionals:
//
//#define PreAllocated
#define TranspositionByValue
//#define DebugExactMatch
//#define XPHash128
//#define XPMoveTypes

namespace Engine.CacheValue {
  using static Board;
  using Cache;
  using static Logging.Logger;
  using static Position;

  using System;
  using System.Diagnostics;

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Bval = Int16;
  using Eval = Int16;
  using Hashcode = UInt64;
  using MoveTypeOrdering = UInt64;
  using Ply = UInt16;
#if TranspositionByValue
  struct Transposition : ITankable<Transposition> {
#else
  class Transposition : ITankable<Transposition> {
#endif
    #region Constructors
    public void Init() {
      ibv = IBV(EvalUndefined, EvalType.Undefined);
      BestMove = Move.Undefined;
    }
#if PreAllocated && !TranspositionByValue
    //[Note]Structs cannot contain explicit parameterless constructors
    public Transposition() {
      Init();
    }
#endif
    public Transposition(Hashcode qHash,
#if XPHash128
                         Hashcode qHashPawn,
#endif
#if XPMoveTypes
                         MoveTypeOrdering mtOrdering,
#endif
                         Ply wMovePly,
                         Depth wDepth,
                         Eval mValue = EvalUndefined,
                         EvalType et = EvalType.Undefined,
                         Move moveBest = Move.Undefined) {
      Hash = qHash;
      Debug.Assert(Hash != 0, "Zero Hash [Transposition]");
#if XPHash128
      HashPawn = qHashPawn;
#endif
#if XPMoveTypes
      MoveTypeOrdering = mtOrdering;
#endif
      MovePly = wMovePly;
      Depth = wDepth;
      ibv = IBV(mValue, et);
      BestMove = moveBest & Move.StoreMask;
    }
    #endregion

    #region ITankable Interface Methods
    public Boolean Match(Transposition xp) {
#if !TranspositionByValue
      if (xp is null)
        return false;
#endif
#if XPHash128
      return Hash == xp.Hash && HashPawn == xp.HashPawn;
#else
      return Hash == xp.Hash;
#endif
    }
    #endregion

    #region ITankable Interface Properties
    public Hashcode Hash { get; set; }
#if XPMoveTypes
    public MoveTypeOrdering MoveTypeOrdering { get; set; }
#endif
    public Boolean IsEmpty {
      get { return Hash == 0; }
    }
    #endregion

    #region ITankable Interface Methods
    public ProbeResult Result(ref Transposition match) {
      //
      // 1) Is the match Valid at the current Search Depth?
      // 2) Is the match from the previous Search Depth?  Renew it if so; and return its Move.
      // 3) Does the match provide a Value?
      //
      var pr = ProbeResult.Match;
      var bValid = false;

      var bRenew = match.Quality <= Quality + 1;
      if (bRenew) {
        pr |= ProbeResult.Renew;        // Useful for BestMove, worth renewing even if not Valid
        match.BestMove = BestMove;

        if (bValid = match.Quality <= Quality)
          pr |= ProbeResult.Valid;
      }

      if (bValid ||
          Type == EvalType.Lower ||
          Type == EvalType.Exact) {     // Value may be used to "stand pat" even if not Valid
        pr |= ProbeResult.Value;        //[Debug]Use of EvalUndefined obviates the need for this Value Flag
        match.ibv = ibv;
      }

      return pr;
    }

    public Boolean IsNew(Transposition store) {
      var bNew = false;                 // Assume Satisfactory
      var bShow = false;
      if (Quality < store.Quality)
        bNew = true;
      else if (Quality == store.Quality) {
        var et = IBType(store.ibv);
        if (et == IBType(ibv)) {
          if (et == EvalType.Lower)
            bNew = store.ibv > ibv;     // Improve GLB
          else if (et == EvalType.Upper)
            bNew = store.ibv < ibv;     // Improve LUB
#if DebugExactMatch
          else
            bShow = store.Value != Value;
#endif
        }
        else {                          // Upper or Lower can become Exact; but not vice versa
          bNew = et == EvalType.Exact;
#if DebugExactMatch
          bShow = !bNew;
#endif
        }
      }
#if DebugExactMatch
      if (bNew && bShow) {
        // Noticed when filtering Draw2 Nodes
        LogLine($"EvalType changed from {IBType(ibv)} to {IBType(store.ibv)}");
        LogLine($"Value is changed from {ibv} to {store.ibv}");
      }
#endif
      return bNew;
    }
    #endregion

    #region Methods
#if !TranspositionByValue
    // Recycle to reduce garbage:
    public void Recycle(Transposition store) {
      Hash = store.Hash;
#if XPHash128
      HashPawn = store.HashPawn;
#endif
#if XPMoveTypes
      MoveTypeOrdering = store.MoveTypeOrdering;
#endif
      MovePly = store.MovePly;
      Depth = store.Depth;
      ibv = store.ibv;
      BestMove = store.BestMove;
    }
#endif
    public Ply Age(Ply wGamePly) {
      return (Ply)(wGamePly - MovePly);
    }
    #endregion

    #region Virtual Fields
#if XPHash128
    public Hashcode HashPawn;
#endif
    public Ply MovePly;                 // To determine age
    public Depth Depth;
    private Bval ibv;
    public Move BestMove;
    #endregion

    #region Properties
    public Ply Quality {
      //
      // Deeper searches for preceding game plies are currently allowed for shallower searches from subsequent plies.
      //
      get { return (Ply)(MovePly + Depth); }
    }

    public EvalType Type {
      get { return IBType(ibv); }
    }

    public Eval Value {
      get { return IBEval(ibv); }
    }

    private const Int32 nIBVBits = 16;
    private const Int32 nMoveBits = nOmitFileBit;
    private const Int32 nPlyBits = 10;
    private const Int32 nDepthBits = 10;

    private const UInt16 wIBVMask = (UInt16)((1U << nIBVBits) - 1);
    private const UInt16 wPlyMask = (UInt16)((1U << nPlyBits) - 1);
    private const UInt16 wDepthMask = (UInt16)((1U << nDepthBits) - 1);

    private const Int32 nMoveLoBit = nIBVBits;
    private const Int32 nMovePlyBit = nMoveLoBit + nMoveBits;
    private const Int32 nDepthBit = nMovePlyBit + nPlyBits;
    private const Int32 nDataBits = nDepthBit + nDepthBits;

    public UInt64 Data {
      get {
        return ((UInt64)(/*wDepthMask & */Depth) << nDepthBit) |
                ((UInt64)(wPlyMask & MovePly) << nMovePlyBit) |
                ((UInt64)BestMove << nMoveLoBit) |
                (UInt16)(/*wIBVMask & */ibv);
      }

      set {
        ibv = (Bval)(value/* & wIBVMask*/);
        BestMove = (Move)(value >> nMoveLoBit) & Move.StoreMask;
        MovePly = (Ply)((UInt16)(value >> nMovePlyBit) & wPlyMask);
        Depth = (Depth)((UInt16)(value >> nDepthBit)/* & wDepthMask*/);
      }
    }
    #endregion
  }
}
