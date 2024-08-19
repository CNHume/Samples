//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2013-06-24 CNHume]Created File
//
// Conditionals:
//
//#define MaterialBalance

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Engine;

using Cache;

using CacheValue;

using MoveOrder;                        // For Variation

using static Board;
using static Position;

//
// Type Aliases:
//
using Depth = UInt16;
using Eval = Int16;
using Ply = UInt16;

partial class GameState {
  #region Constants
  private const Int32 nMultiPVLengthDefault = 12;

  private const Int32 nQXPSelectionDefault = 32;
  private const Int32 nXPSelectionDefault = 96;
  private const Int32 nXPMSelectionDefault = 16;
  #endregion                            // Constants

  #region Memo Table Dimensions
  //
  // The following table lists the largest Primes fitting into the given # of bits.
  //
  // However, tests have consistently shown that masking out low order bits of Zobrist
  // Sums performs slightly better than obtaining a remainder with one of these Primes.
  //
  // (1U << 25) - 39  // Largest 25-bit prime
  // (1U << 24) - 3   // Largest 24-bit prime - 16M
  // (1U << 23) - 15  // Largest 23-bit prime
  // (1U << 22) - 3   // Largest 22-bit prime - 4M
  // (1U << 21) - 9   // Largest 21-bit prime
  // (1U << 20) - 3   // Largest 20-bit prime - 1M
  // (1U << 19) - 1   // Largest 19-bit prime
  // (1U << 18) - 5   // Largest 18-bit prime - 262K
  // (1U << 17) - 1   // Largest 17-bit prime
  // (1U << 16) - 15  // Largest 16-bit prime - 65.5K
  //
  private const UInt32 uDefaultCompositions = 8179;       // 8191, 8179, 4093, 4091, 2999, 2039 and 1999 are prime; and so are 1021, 1019
  private const UInt32 uDefaultComposition2 = 1 << nHashPieceBits;
  private const UInt32 uDefaultPawnPositions = 1U << 16;  // +1 is prime
  #endregion                            // Memo Table Dimensions

  #region Table Initializers
  [MemberNotNull(
    nameof(NodeDelta),
    nameof(NodeDeltaLog)
    )]
  private void newNodeDelta(Depth wDepths) {
    NodeDelta = new UInt64[wDepths];
    NodeDeltaLog = new Double[wDepths];
  }

  private void clearNodeDelta() {
    Array.Clear(NodeDelta, 0, NodeDelta.Length);
    Array.Clear(NodeDeltaLog, 0, NodeDeltaLog.Length);
  }

  [Conditional("CountEarlyMoves")]
  [MemberNotNull(nameof(EarlyMoveCount))]
  private void newEarlyMoveCounts(Ply wPlies) {
    EarlyMoveCount = new Int64[wPlies];
  }

  [Conditional("CountEarlyMoves")]
  private void clearEarlyMoveCounts() {
    EarlyMoveMinPly = Ply.MaxValue;
    EarlyMoveMaxPly = Ply.MinValue;
    WhiteSearchedPositionCount = WhiteEarlyMoveTotal = 0;
    BlackSearchedPositionCount = BlackEarlyMoveTotal = 0;
    Array.Clear(EarlyMoveCount, 0, EarlyMoveCount.Length);
  }

  [Conditional("CountPVDoubles")]
  [MemberNotNull(nameof(PVDoubleCount))]
  private void newPVDoubleCounts(Ply wPlies) {
    PVDoubleCount = new Int64[wPlies];
  }

  [Conditional("CountPVDoubles")]
  private void clearPVDoubleCounts() {
    PVDoubleMinPly = Ply.MaxValue;
    PVDoubleMaxPly = Ply.MinValue;
    PVDoubleTotal = 0;
    Array.Clear(PVDoubleCount, 0, PVDoubleCount.Length);
  }

  private void loadEndgameValue() {
    //
    // The following allows QP v RBN as an endgame; but not
    // the QN v RBN with 517 DTC [Bourzutschky and Konoval]
    //
    EndgameValue = 0;
    for (var vPiece = vP6; vPiece < vK6; vPiece++)
      EndgameValue += (Eval)PieceWeight[vPiece];
  }

  private void loadExtensionLimit() {
    //
    // 4 x 4-bit nibbles are currently held by a 16-bit limit value.
    // Defaults are specified by Option.Default properties in GameState.Controls
    //
    ExtensionLimit = 0;
  }

  [MemberNotNull(nameof(CXPMemo))]
  private void newCXPMemo(UInt32 uLength) {
    if (CXPMemo == null)
#if MaterialBalance
      CXPMemo = new Memo2<Composition2>("CX2", uLength);
#else
      CXPMemo = new Memo<Composition>("CXP", uLength);
#endif
  }

  [MemberNotNull(nameof(PXPMemo))]
  private void newPXPMemo(UInt32 uLength) {
    if (PXPMemo == null)
      PXPMemo = new Memo2<PawnPosition>("PXP", uLength);
  }

  [MemberNotNull(nameof(QXPTank))]
  private void newQXPTank(Int32 nSelection = nQXPSelectionDefault) {
    if (QXPTank == null)
      QXPTank = new Tank<QuietPosition>("QXP", nSelection);
  }

  [MemberNotNull(nameof(XPTank))]
  private void newXPTank(Int32 nSelection = nXPSelectionDefault) {
    if (XPTank == null)
      XPTank = new Tank<Transposition>("XP", nSelection);
  }

  [MemberNotNull(nameof(XPMTank))]
  private void newXPMTank(Int32 nSelection = nXPMSelectionDefault) {
    if (XPMTank == null)
      XPMTank = new Tank<PositionMove>("XPM", nSelection);
  }

  [MemberNotNull(nameof(Variation))]
  private void newVariations(Int32 nMultiPVLength = nMultiPVLengthDefault) {
    if (Variation == null || Variation.Length < nMultiPVLength) {
      //
      // Pre-allocate nMultiPVLength Variation elements.
      //
      // Increasing MultiPVLength invalidates the elements
      // when a search is in progress.
      //
      var nVariations = nMultiPVLength < nMultiPVLengthDefault ?
        nMultiPVLengthDefault : nMultiPVLength;
      allocateVariations(nVariations);
    }

    // Update # of variations sought
    MultiPVLength = (Byte)nMultiPVLength;
  }

  [MemberNotNull(nameof(Variation))]
  private void allocateVariations(Int32 nVariations) {
    MultiPVCount = 0;                   //[Init]
    Variation = new Variation[nVariations];
    for (var nVariation = 0; nVariation < nVariations; nVariation++)
      Variation[nVariation] = new Variation();
  }
  #endregion                            // Table Initializers
}
