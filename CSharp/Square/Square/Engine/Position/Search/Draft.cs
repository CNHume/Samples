//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
#define Fractional
//#define RoundPly
#define LinearReduction

namespace Engine {
  using static Logging.Logger;

  using System;

  //
  // Type Aliases:
  //
  using PlyDepth = System.Byte;
  using Depth = System.UInt16;
  using Draft = System.UInt16;
  using ExtensionCounter = System.UInt16;

  partial class Position : Board {
    #region Constants
    protected const Int32 nDepthScale = nPerByte;
    protected const Draft wFourPly = wTwoPly << 1;
    protected const Draft wThreePly = wTwoPly + wFullPly;
    protected const Draft wTwoPly = 1 << nDepthScale + 1;
    protected const Draft wFullPly = 1 << nDepthScale;
    protected const Draft wHalfPly = 1 << nDepthScale - 1;
    #endregion

    #region Enumerations
    public enum SearchExtensions : byte { Late, Check, Threat, Singular };

    public const Byte vLate = (Byte)SearchExtensions.Late;
    public const Byte vCheck = (Byte)SearchExtensions.Check;
    public const Byte vThreat = (Byte)SearchExtensions.Threat;
    public const Byte vSingular = (Byte)SearchExtensions.Singular;
    #endregion

    #region Fractional Depth Extension Methods
    //
    // Draft is a Fixed Point representation of Depth,
    // which allows Fractional Depth Extensions to be
    // represented via the 8-bit fractional component.
    //
    protected static Draft draft(Depth wDepth) {
      return (Draft)(wDepth << nDepthScale);
    }

    private static Draft nextDraft(Draft wDraft) {
      return (Draft)(wDraft > wFullPly ? wDraft - wFullPly : 0);
    }

    protected static Depth depth(Draft wDraft) {
      //[Warning]Zero depth must be allowed in order for
      // Search recursion to end with a Quiescent Search.
#if RoundPly
      return (Depth)(wDraft + wHalfPly >> nDepthScale);
#else
      return (Depth)(wDraft >> nDepthScale);
#endif
    }

    private static Draft extensionDraft(Int32 nExt) {
      var wDelta = wFullPly;
#if Fractional
      //
      // SearchExtensions: Late, Check, Threat, Singular
      //
      switch ((SearchExtensions)nExt) {
      case SearchExtensions.Late:
        wDelta = wFullPly;
        break;

      case SearchExtensions.Check:
        wDelta = wFullPly * 7 / 8;
        break;

      case SearchExtensions.Threat:
        wDelta = wFullPly * 3 / 2;
        break;

      case SearchExtensions.Singular:
        wDelta = wFullPly;
        break;
      }
#endif
      return wDelta;
    }

    protected void decExtension(ref Draft wDraft, Int32 nExt) {
      // Govern dec as well as inc
      ExtensionCounts += (ExtensionCounter)(1 << nExt * nPerNibble);
      wDraft -= extensionDraft(nExt);    // Decrement
    }

    protected void incExtension(ref Draft wDraft, Int32 nExt) {
      // Govern usage
      ExtensionCounts += (ExtensionCounter)(1 << nExt * nPerNibble);
      wDraft += extensionDraft(nExt);   // Increment
    }

    protected Boolean canExtend(Int32 nExt) {
      var vCount = getNibble(ExtensionCounts, nExt);
      var vLimit = getNibble(State.ExtensionLimit, nExt);
      return vCount < vLimit;
    }

    protected Boolean extended(ref Draft wDraft, SearchExtensions extension) {
      var nExt = (Int32)extension;
      var bExtended = canExtend(nExt);

      if (bExtended)
        incExtension(ref wDraft, nExt);

      return bExtended;
    }

    //
    // Reduced Draft used for Null Move and other Heuristic Searches
    //
    protected Draft reduceShallow(Draft wDraft) {
#if LinearReduction
      var wDraft1 = (Draft)((wDraft + wFullPly) / 3);         // 1/3 vs 3/8
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

    protected Draft reduceDeep(Draft wDraft) {
#if LinearReduction
      var wDraft1 = (Draft)(4 * (wDraft + wFullPly) / 5);     // 4/5 vs 1/2
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

    internal void writeDepthShallow(PlyDepth vPliesLimit) {
      for (PlyDepth vPlies = 0; vPlies < vPliesLimit; vPlies++) {
        var wDraft = draft((Depth)vPlies);
        LogLine($"Shallow({vPlies,2}) = {depth(reduceShallow(wDraft)),2}");
      }
    }

    internal void writeDepthDeep(PlyDepth vPliesLimit) {
      for (PlyDepth vPlies = 0; vPlies < vPliesLimit; vPlies++) {
        var wDraft = draft((Depth)vPlies);
        LogLine($"Deep({vPlies,2}) = {depth(reduceDeep(wDraft)),2}");
      }
    }
    #endregion
  }
}
