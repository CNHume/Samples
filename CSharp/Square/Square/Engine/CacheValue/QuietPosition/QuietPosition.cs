﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-03-31 CNHume]Created Class
//
// Conditionals:
//
//#define QXPHash128
//#define PreAllocated
#define QuietPositionByValue
//#define DebugExactMatch

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
  using Bval = System.Int16;
  using Eval = System.Int16;
  using Hashcode = System.UInt64;
  using Ply = System.UInt16;
#if QuietPositionByValue
  struct QuietPosition : ITankable<QuietPosition> {
#else
  class QuietPosition : ITankable<QuietPosition> {
#endif
    #region Constructors
    public void Init() {
      ibv = IBV(EvalUndefined, EvalType.Undefined);
      BestMove = Move.Undefined;
    }
#if PreAllocated && !QuietPositionByValue
    //[Note]Structs cannot contain explicit parameterless constructors
    public QuietPosition() {
      Init();
    }
#endif
    public QuietPosition(Hashcode qHash,
#if QXPHash128
                         Hashcode qHashPawn,
#endif
                         Ply wMovePly,
                         Eval mValue = EvalUndefined,
                         EvalType et = EvalType.Undefined,
                         Move moveBest = Move.Undefined) {
      Hash = qHash;
      Debug.Assert(Hash != 0, "Zero Hash [QuietPosition]");
#if QXPHash128
      HashPawn = qHashPawn;
#endif
      MovePly = wMovePly;
      ibv = IBV(mValue, et);
      BestMove = moveBest & Move.StoreMask;
    }
    #endregion

    #region ITankable Interface Methods
    public Boolean Match(QuietPosition qxp) {
#if !QuietPositionByValue
      if (qxp is null)
        return false;
#endif
#if QXPHash128
      return Hash == qxp.Hash && HashPawn == qxp.HashPawn;
#else
      return Hash == qxp.Hash;
#endif
    }
    #endregion

    #region ITankable Interface Properties
    public Hashcode Hash { get; set; }

    public Boolean IsEmpty {
      get { return Hash == 0; }
    }
    #endregion

    #region ITankable Interface Methods
    public ProbeResult Result(ref QuietPosition match) {
      var pr = ProbeResult.Match;

      if (!IsEmpty) {
        pr |= ProbeResult.Valid;
        match.BestMove = BestMove;
        match.ibv = ibv;
      }

      return pr;
    }

    public Boolean IsNew(QuietPosition store) {
      var bNew = false;                 // Assume Satisfactory
      var bShow = false;
      var et = IBType(store.ibv);
      if (et == IBType(ibv)) {
        if (et == EvalType.Lower)
          bNew = store.ibv > ibv;       // Improve GLB
        else if (et == EvalType.Upper)
          bNew = store.ibv < ibv;       // Improve LUB
#if DebugExactMatch
        else
          bShow = store.ibv != ibv;
#endif
      }
      else {                            // Upper or Lower can become Exact; but not vice versa
        bNew = et == EvalType.Exact;
#if DebugExactMatch
        bShow = !bNew;
#endif
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
#if !QuietPositionByValue
    // Recycle QXPs to reduce garbage:
    public void Recycle(QuietPosition store) {
      Hash = store.Hash;
#if QXPHash128
      HashPawn = store.HashPawn;
#endif
      MovePly = store.MovePly;
      ibv = store.ibv;
      BestMove = store.BestMove;
    }
#endif
    public Ply Age(Ply wGamePly) {
      return (Ply)(wGamePly - MovePly);
    }
    #endregion

    #region Virtual Fields
#if QXPHash128
    public Hashcode HashPawn;
#endif
    public Ply MovePly;                 // To determine age
    private Bval ibv;
    public Move BestMove;
    #endregion

    #region Properties
    public EvalType Type {
      get { return IBType(ibv); }
    }

    public Eval Value {
      get { return IBEval(ibv); }
    }

    private const Int32 nIBVBits = 16;
    private const Int32 nMoveBits = nOnlyFileBit;
    private const Int32 nPlyBits = 10;

    private const UInt16 wIBVMask = (UInt16)((1U << nIBVBits) - 1);
    private const UInt16 wPlyMask = (UInt16)((1U << nPlyBits) - 1);

    private const Int32 nMoveLoBit = nIBVBits;
    private const Int32 nMovePlyBit = nMoveLoBit + nMoveBits;
    private const Int32 nDataBits = nMovePlyBit + nPlyBits;

    public UInt64 Data {
      get {
        return ((UInt64)(wPlyMask & MovePly) << nMovePlyBit) |
                ((UInt64)BestMove << nMoveLoBit) |
                (UInt16)(/*wIBVMask & */ibv);
      }

      set {
        ibv = (Bval)(value/* & wIBVMask*/);
        BestMove = (Move)(value >> nMoveLoBit) & Move.StoreMask;
        MovePly = (Ply)((UInt16)(value >> nMovePlyBit)/* & wPlyMask*/);
      }
    }
    #endregion
  }
}
