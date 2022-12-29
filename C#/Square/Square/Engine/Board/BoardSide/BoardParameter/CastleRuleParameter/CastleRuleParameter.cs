//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2018-10-28 CNHume]Created Class
//
namespace Engine {
  using System;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Board {
    internal partial class BoardSide {
      internal partial class BoardParameter {
        internal class CastleRuleParameter {
          #region Constructors
          public CastleRuleParameter(Int32 nSetupRank) {
            SetupRank = nSetupRank;

            KingOOTo = SetupRank + (Int32)Sq.g1;
            RookOOTo = SetupRank + (Int32)Sq.f1;
            KingOOOTo = SetupRank + (Int32)Sq.c1;
            RookOOOTo = SetupRank + (Int32)Sq.d1;

            Clear();
          }
          #endregion                    // Constructors

          #region Methods
          //
          // Clear - Called from parseCastlingRights() and setupCastlingRights()
          // Init - Called by Position.Init() following ParsePosition()
          // rankPath - Returns mask for squares that must not be obstructed (or attacked)
          //
          public void Clear() {
            //
            //[Chess 960]Castles From squares are set by BoardSide.GrantCastling():
            //
            CastlesFrom = default;
            RookOOFrom = default;
            RookOOOFrom = default;

            //[Safe]
            OO = Move.Undefined;
            OOSafe = default;
            OOPath = default;

            //[Safe]
            OOO = Move.Undefined;
            OOOSafe = default;
            OOOPath = default;
          }

          //
          // Setup Dependent CanOO() and CanOOO() Initialization
          //
          // Castling requires that the corresponding CanOO or CanOOO flag be set in SidFlags.
          //
          // No Path square can be obstructed by a Piece from either side other than the King and
          // castling Rook themselves; and no Safe square (through which the King travels) can be
          // under attack by the Foe.
          //
          public void Init() {
            //
            //[Assume]BoardSide.GrantCastling() has been called.
            //
            //[Chess 960]Castles bit is needed to distinguish castles from ambiguous King moves:
            //
            const Move castles = Move.Castles | KingMove;

            if (!CastlesFrom.HasValue) return;

            var OOTo = castles | ToMove(KingOOTo);
            var OOOTo = castles | ToMove(KingOOOTo);
            var qpKing = bit(CastlesFrom.Value);

            if (RookOOFrom.HasValue) {
              //[Safe]addCastles() should not be called when InCheck
              OOSafe = rankPath(CastlesFrom.Value, KingOOTo) | qpKing;
              var qpPath = rankPath(RookOOFrom.Value, RookOOTo);
              var qpMask = bit(RookOOFrom.Value) | qpKing;
              OOPath = (OOSafe | qpPath) & ~qpMask;
              OO = OOTo | FromMove(CastlesFrom.Value);
            }

            if (RookOOOFrom.HasValue) {
              //[Safe]addCastles() should not be called when InCheck
              OOOSafe = rankPath(CastlesFrom.Value, KingOOOTo) | qpKing;
              var qpPath = rankPath(RookOOOFrom.Value, RookOOOTo);
              var qpMask = bit(RookOOOFrom.Value) | qpKing;
              OOOPath = (OOOSafe | qpPath) & ~qpMask;
              OOO = OOOTo | FromMove(CastlesFrom.Value);
            }
          }

          //
          // Return squares that must be unobstructed for a
          // King or Rook to reach nTo, along the base rank.
          // nFrom is skipped but nTo is included.
          //
          private static Plane rankPath(Int32 nFrom, Int32 nTo) {
            if (nFrom < nTo)
              nFrom++;                      // Skip Lo
            else if (nFrom > nTo) {
              nFrom--;                      // Skip Hi
              (nFrom, nTo) = (nTo, nFrom);  // Swap
            }

            var nBits = nTo + 1 - nFrom;
            var nMask = bit(nBits) - 1;
            var qpPath = nMask << nFrom;
            return qpPath;
          }

          public Move Castles(Int32 nTo) {
            if (nTo == KingOOTo)
              return OO;
            else if (nTo == KingOOOTo)
              return OOO;
            else
              return Move.Undefined;
          }
          #endregion                    // Methods

          #region Fields
          //
          // The following are readonly fields set by the Constructor:
          //
          protected Int32 SetupRank { get; init; }

          public readonly Int32 KingOOTo;
          public readonly Int32 RookOOTo;
          public readonly Int32 KingOOOTo;
          public readonly Int32 RookOOOTo;

          //
          //[Chess 960]Castles From squares are set by BoardSide.GrantCastling():
          //
          public Int32? CastlesFrom;
          public Int32? RookOOFrom;
          public Int32? RookOOOFrom;

          //
          // The following are set by the Init() method:
          //
          public Move OO = Move.Undefined;
          public Plane? OOSafe;
          public Plane? OOPath;

          public Move OOO = Move.Undefined;
          public Plane? OOOSafe;
          public Plane? OOOPath;
          #endregion                    // Fields
        }                               // CastleRuleParameter
      }                                 // BoardParameter
    }                                   // BoardSide
  }                                     // Board
}
