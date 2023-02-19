//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-06-24 CNHume]Created File
//
// Conditionals:
//
//#define MaterialBalance

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Engine {
  using Cache;

  using CacheValue;

  using MoveOrder;                      // For Variation

  using static Board;
  using static Position;

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Eval = Int16;
  using Ply = UInt16;

  partial class GameState {
    private const Int32 nVariationsDefault = 12;    // Initial Allocation

    private const Int32 nQXPSelectionDefault = 16;
    private const Int32 nXPSelectionDefault = 48;
    private const Int32 nXPMSelectionDefault = 8;

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
    private const UInt32 uDefaultCompositions = 8179;         // 8191, 8179, 4093, 4091, 2999, 2039 and 1999 are prime; and so are 1021, 1019
    private const UInt32 uDefaultComposition2 = 1 << nHashPieceBits;
    private const UInt32 uDefaultPawnPositions = 1U << 16;    // +1 is prime
    #endregion

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
      // The following allows QP vs RBN as an endgame; but not
      // the QN vs RBN with 517 DTC [Bourzutschky and Konoval]
      //
      EndgameValue = 0;
      for (var vPiece = vP6; vPiece < vK6; vPiece++)
        EndgameValue += (Eval)PieceWeight[vPiece];
    }

    private void loadExtensionLimit() {
      //
      //[Note]4 x 4-bit nibbles are currently held by a 16-bit limit value:
      //
      ExtensionLimit = 0;

#if OldExtensionLimitDefaults
      //
      //[Note]The following defaults are now specified via Option.Default properties in GameState.Controls:
      //
      // CheckExtensionLimit = 4 sufficient for Caruana v Gustafsson Mate [in 12-ply] w zMateDepthMin = 4
      // 6 sufficient to solve Johannessen vs Fischer #8 [in 11-ply]
      // 6 solves Perpetual [in 13-ply]
      // 8 solves Perpetual faster and finds Kramnik v Meier 2012-07-22 [in 12-ply]
      //
      setNibble(ref ExtensionLimit, vCheck, 6);
      setNibble(ref ExtensionLimit, vLate, 2);
      setNibble(ref ExtensionLimit, vThreat, 1);
      setNibble(ref ExtensionLimit, vSingular, 1);
#endif
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
    private void newVariations(Int32 nSelection = nVariationsDefault) {
      if (Variation == null || Variation.Length < nSelection) {
        //
        // Currently, nVariationsDefault pre-allocates MultiPV Max value.
        // Otherwise, a Search in progress might need its entries copied.
        //
        var nVariations = nSelection < nVariationsDefault ?
          nVariationsDefault : nSelection;
        newVariations2(nVariations);
      }

      // MultiPVLength === nVariations in use
      MultiPVLength = (Byte)nSelection;
    }

    [MemberNotNull(nameof(Variation))]
    private void newVariations2(Int32 nVariations) {
      VariationCount = 0;               //[Init]
      Variation = new Variation[nVariations];
      for (var nVariation = 0; nVariation < nVariations; nVariation++)
        Variation[nVariation] = new Variation();
    }
    #endregion
  }
}
