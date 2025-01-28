//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2018-10-28 CNHume]Created Class
//
namespace Engine;
//
// Type Aliases:
//
using Plane = UInt64;

partial class Board {
  internal partial class BoardSide {
    internal partial class BoardParameter {
      internal class CastleRuleParameter {
        #region Constructors
        public CastleRuleParameter(Int32 nPieceRank) {
          PieceRank = nPieceRank;

          KingOOTo = (Byte)sqr(x((Int32)Sq.g1), PieceRank);
          RookOOTo = (Byte)sqr(x((Int32)Sq.f1), PieceRank);
          KingOOOTo = (Byte)sqr(x((Int32)Sq.c1), PieceRank);
          RookOOOTo = (Byte)sqr(x((Int32)Sq.d1), PieceRank);

          Clear();
        }
        #endregion                    // Constructors

        #region Methods
        //
        // Clear - Called from parseCastlingFlags() and setupCastling()
        // Init - Called by Position.initCastleRules() following ParsePosition()
        // rankPath - Returns mask for squares that must not be obstructed (or attacked)
        //
        public void Clear() {
          //
          //[Chess960]Castles From squares are set by BoardSide.GrantCastling():
          //
          CastlesFrom = default;
          RookOOFrom = default;
          RookOOOFrom = default;

          //[Safe]
          OOMove = Move.Undefined;
          OOSafe = default;
          OOPath = default;

          //[Safe]
          OOOMove = Move.Undefined;
          OOOSafe = default;
          OOOPath = default;
        }

        //
        // Setup-dependent Initialization for CanOO() and CanOOO()
        //
        // Castling requires that the corresponding CanOO or CanOOO flag be set in SideFlags.
        //
        // No Path square can be obstructed by a Piece from either side other than the King and
        // castling Rook themselves; and no Safe square (through which the King travels) can be
        // under attack by the Foe.
        //
        public void Init() {
          //
          //[Assume]BoardSide.GrantCastling() has been called.
          //
          //[Chess960]Castles bit needed to resolve ambiguity between castling and King moves:
          //
          const Move castles = Move.Castles | MoveKing;

          if (!CastlesFrom.HasValue) return;

          var OOMoveTo = castles | MoveTo(KingOOTo);
          var OOOMoveTo = castles | MoveTo(KingOOOTo);
          var qpKing = bit(CastlesFrom.Value);

          if (RookOOFrom.HasValue) {
            //[Safe]addCastles() should not be called when InCheck
            OOSafe = rankPath(CastlesFrom.Value, KingOOTo) | qpKing;
            var qpPath = rankPath(RookOOFrom.Value, RookOOTo);
            var qpMask = bit(RookOOFrom.Value) | qpKing;
            OOPath = (OOSafe | qpPath) & ~qpMask;
            OOMove = OOMoveTo | MoveFrom(CastlesFrom.Value);
          }

          if (RookOOOFrom.HasValue) {
            //[Safe]addCastles() should not be called when InCheck
            OOOSafe = rankPath(CastlesFrom.Value, KingOOOTo) | qpKing;
            var qpPath = rankPath(RookOOOFrom.Value, RookOOOTo);
            var qpMask = bit(RookOOOFrom.Value) | qpKing;
            OOOPath = (OOOSafe | qpPath) & ~qpMask;
            OOOMove = OOOMoveTo | MoveFrom(CastlesFrom.Value);
          }
        }

        //
        // Return squares that must be unobstructed for a
        // King or Rook to reach nTo, along the base rank.
        // nFrom is skipped but nTo is included.
        //
        private static Plane rankPath(Int32 nFrom, Int32 nTo) {
          if (nFrom < nTo)
            nFrom++;                    // Skip Lo
          else if (nFrom > nTo) {
            nFrom--;                    // Skip Hi
            (nFrom, nTo) = (nTo, nFrom);// Swap
          }

          var nBits = nTo + 1 - nFrom;
          var nMask = bit(nBits) - 1;
          var qpPath = nMask << nFrom;
          return qpPath;
        }

        public Move Castles(Int32 nTo) {
          if (nTo == KingOOTo)
            return OOMove;
          else if (nTo == KingOOOTo)
            return OOOMove;
          else
            return Move.Undefined;
        }

        public Boolean IsOrthodoxCastling() {
          var bOrthodox =
            (!CastlesFrom.HasValue || x(CastlesFrom.Value) == x((Int32)Sq.e1)) &&
            (!RookOOOFrom.HasValue || x(RookOOOFrom.Value) == x((Int32)Sq.a1)) &&
            (!RookOOFrom.HasValue || x(RookOOFrom.Value) == x((Int32)Sq.h1));

          return bOrthodox;
        }
        #endregion                    // Methods

        #region Fields
        //
        // The following are readonly fields set by the Constructor:
        //
        protected Int32 PieceRank { get; init; }

        public readonly Byte KingOOTo;
        public readonly Byte RookOOTo;
        public readonly Byte KingOOOTo;
        public readonly Byte RookOOOTo;

        //
        //[Chess960]Castles From squares are set by BoardSide.GrantCastling():
        //
        public Byte? CastlesFrom;
        public Byte? RookOOFrom;
        public Byte? RookOOOFrom;

        //
        // The following are set by the Init() method:
        //
        public Move OOMove = Move.Undefined;
        public Plane? OOSafe;
        public Plane? OOPath;

        public Move OOOMove = Move.Undefined;
        public Plane? OOOSafe;
        public Plane? OOOPath;
        #endregion                      // Fields
      }                                 // CastleRuleParameter
    }                                   // BoardParameter
  }                                     // BoardSide
}                                       // Board
