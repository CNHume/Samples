//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2016-01-25 CNHume]Split class from MoveHistory
//
// Conditionals:
//
#define BottleBothSides                 // Prevents odd extension from referencing opponent's Killer

using System.Diagnostics.CodeAnalysis;

namespace MoveOrder;

using Engine;

using static Engine.Board;

//
// Type Aliases:
//
using BottleHash = UInt32;

class MoveBottle {
  #region Constants
#if BottleBothSides
  private static readonly Int32 nBottleSides = nSides;
#else
  private static readonly Int32 nBottleSides = 1;
#endif
  internal static readonly Int32 nKillers = 1;
  #endregion

  #region Constructors
  public MoveBottle(UInt32 uBottleLength) {
    allocate(uBottleLength);
  }
  #endregion

  #region Killer Methods
  private UInt32 index(BottleHash uBottleHash) {
    return uBottleHash % BottleLength;
  }

  private void allocate(UInt32 uBottleLength) {
    if (nKillers > 0) {
      Killers = new GoodMove[nBottleSides][][];
      for (var nSide = 0; nSide < nBottleSides; nSide++) {
        Killers[nSide] = new GoodMove[nKillers][];
        for (var nKiller = 0; nKiller < nKillers; nKiller++)
          Killers[nSide][nKiller] = new GoodMove[uBottleLength];
      }
    }

    BottleLength = uBottleLength;
  }

  public void Clear() {
    if (Killers == null) return;

    for (var nSide = 0; nSide < nBottleSides; nSide++)
      for (var nKiller = 0; nKiller < nKillers; nKiller++) {
        for (var uIndex = 0U; uIndex < BottleLength; uIndex++)
          Killers[nSide][nKiller][uIndex] = new();
      }
  }

  public void Save(GoodMove store, Move uMaskedMove, BottleHash uBottleHash, Int32 nSide) {
    if (Killers == null) return;

    var uIndex = index(uBottleHash);
    var bReplace = true;                // Assume final write needed
    var bFound = false;
    var nKiller = 0;
    for (; nKiller < nKillers; nKiller++) {
      var gm = Killers[nSide][nKiller][uIndex];
      if (IsUndefined(gm.Move))
        break;
      else if (EqualMoves(uMaskedMove, gm.Move)) {
        bFound = true;
        break;
      }
    }

    if (bFound)
      bReplace = false;
    else {
      if (nKiller < nKillers) {         // Bottle Not Full: Add newest and return
        Killers[nSide][nKiller][uIndex] = store;
        return;
      }
      else
        nKiller = 0;                    // Bottle Full: Remove Oldest Entry
    }

    for (; nKiller + 1 < nKillers; nKiller++) {
      var gm = Killers[nSide][nKiller + 1][uIndex];
      if (IsUndefined(gm.Move))
        break;

      Killers[nSide][nKiller][uIndex] = gm;
      bReplace = true;
    }

    if (bReplace)
      Killers[nSide][nKiller][uIndex] = store;
  }

  public List<GoodMove> Load(BottleHash uBottleHash, Int32 nSide) {
    var uIndex = index(uBottleHash);
    List<GoodMove> killers = [];

    if (Killers != null) {
      for (var nKiller = 0; nKiller < nKillers; nKiller++) {
        var killer = Killers[nSide][nKiller][uIndex];
        killers.Add(killer);
      }
    }

    return killers;
  }
  #endregion

  #region Fields
  public UInt32 BottleLength;
  public GoodMove[][][]? Killers;
  #endregion
}
