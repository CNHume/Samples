﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
#define Fractional
//#define RoundPly
#define LinearReduction

namespace Engine;

using static Logging.Logger;

//
// Type Aliases:
//
using Depth = UInt16;
using Draft = UInt16;
using ExtensionCounter = UInt16;
using PlyDepth = Byte;

partial class Position : Board {
  #region Constants
  private const Int32 nDepthScale = nPerByte;
  private const Draft wFourPly = wTwoPly << 1;
  private const Draft wThreePly = wTwoPly + wFullPly;
  private const Draft wTwoPly = 1 << nDepthScale + 1;
  private const Draft wFullPly = 1 << nDepthScale;
  private const Draft wHalfPly = 1 << nDepthScale - 1;
  #endregion                            // Constants

  #region Enumerations
  public enum SearchExtensions : byte {
    Late, Check, Threat, Singular
  };

  internal const Byte vLate = (Byte)SearchExtensions.Late;
  internal const Byte vCheck = (Byte)SearchExtensions.Check;
  internal const Byte vThreat = (Byte)SearchExtensions.Threat;
  internal const Byte vSingular = (Byte)SearchExtensions.Singular;
  #endregion                            // Enumerations

  #region Fractional Depth Extension Methods
  //
  // Draft is a Fixed Point representation of Depth,
  // which allows Fractional Depth Extensions to be
  // represented via the 8-bit fractional component.
  //
  private static Draft draft(Depth wDepth) {
    return (Draft)(wDepth << nDepthScale);
  }

  //
  // nextDraft() returns a fractional representation of the
  // next lower Ply Depth, used to limit search() recursion.
  //
  private static Draft nextDraft(Draft wDraft) {
    return (Draft)(wDraft > wFullPly ? wDraft - wFullPly : 0);
  }

  private static Depth depth(Draft wDraft) {
    //[Warning]Zero depth must be allowed in order for
    // Search recursion to end with a Quiescent Search.
#if RoundPly
    return (Depth)(wDraft + wHalfPly >> nDepthScale);
#else
    return (Depth)(wDraft >> nDepthScale);
#endif
  }

  private static Draft extensionDraft(Int32 nExt) {
#if Fractional
    //
    // SearchExtensions: Late, Check, Threat, Singular
    //
    var se = (SearchExtensions)nExt;
    return se switch {
      SearchExtensions.Late => wFullPly,
      SearchExtensions.Check => wFullPly * 7 / 8,
      SearchExtensions.Threat => wFullPly * 3 / 2,
      SearchExtensions.Singular => wFullPly,
      _ => wFullPly,
    };
#else
    return wFullPly;
#endif
  }

  private void decExtension(ref Draft wDraft, Int32 nExt) {
    // Govern dec as well as inc
    extensionCounts += (ExtensionCounter)uBit(nExt * nPerNibble);
    wDraft -= extensionDraft(nExt);     // Decrement
  }

  private void incExtension(ref Draft wDraft, Int32 nExt) {
    // Govern usage
    extensionCounts += (ExtensionCounter)uBit(nExt * nPerNibble);
    wDraft += extensionDraft(nExt);     // Increment
  }

  private Boolean canExtend(Int32 nExt) {
    var vCount = GetNibble(extensionCounts, nExt);
    var vLimit = GetNibble(State.ExtensionLimit, nExt);
    return vCount < vLimit;
  }

  private Boolean extended(ref Draft wDraft, SearchExtensions extension) {
    var nExt = (Int32)extension;
    var bExtended = canExtend(nExt);

    if (bExtended)
      incExtension(ref wDraft, nExt);

    return bExtended;
  }

  //
  // Reduced Draft used for Null Move and other Heuristic Searches
  //
  private Draft reduceShallow(Draft wDraft) {
#if LinearReduction
    var wDraft1 = (Draft)((wDraft + wFullPly) / 3);     // 1/3 vs 3/8
#else
    Draft wDraft1;
    if (wFourPly <= wDraft)
      wDraft1 = wThreePly;
    else if (wThreePly <= wDraft)
      wDraft1 = wTwoPly;
    else //if (wTwoPly <= wDraft)
      wDraft1 = wFullPly;
#endif
    return wDraft1 < wDraft ? wDraft1 : (Draft)0;
  }

  private Draft reduceDeep(Draft wDraft) {
#if LinearReduction
    var wDraft1 = (Draft)(4 * (wDraft + wFullPly) / 5); // 4/5 vs 1/2
#else
    Draft wDraft1;
    if (wFourPly <= wDraft)
      wDraft1 = wThreePly;
    else if (wThreePly <= wDraft)
      wDraft1 = wTwoPly;
    else //if (wTwoPly <= wDraft)
      wDraft1 = wFullPly;
#endif
    return wDraft1 < wDraft ? wDraft1 : (Draft)0;
  }

  private void writeShallow(PlyDepth vPliesLimit) {
    for (PlyDepth vPlies = 0; vPlies < vPliesLimit; vPlies++) {
      var wDraft = draft((Depth)vPlies);
      var wShallowDepth = depth(reduceShallow(wDraft));
      LogLine($"Shallow({vPlies,2}) = {wShallowDepth,2}");
    }
  }

  private void writeDeep(PlyDepth vPliesLimit) {
    for (PlyDepth vPlies = 0; vPlies < vPliesLimit; vPlies++) {
      var wDraft = draft((Depth)vPlies);
      var wDeepDepth = depth(reduceDeep(wDraft));
      LogLine($"Deep({vPlies,2}) = {wDeepDepth,2}");
    }
  }
  #endregion                            // Fractional Depth Extension Methods
}
