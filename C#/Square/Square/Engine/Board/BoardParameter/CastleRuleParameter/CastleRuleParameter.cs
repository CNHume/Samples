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
    public partial class BoardParameter {
      public class CastleRuleParameter {
        #region Constructors
        public CastleRuleParameter(Int32 nSetupRank) {
          SetupRank = nSetupRank;

          KingOOTo = SetupRank + (Int32)sq.g1;
          RookOOTo = SetupRank + (Int32)sq.f1;
          KingOOOTo = SetupRank + (Int32)sq.c1;
          RookOOOTo = SetupRank + (Int32)sq.d1;

          Clear();
        }
        #endregion

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
        //[Assume]BoardSide.GrantCastling() has been called.
        //
        public void Init() {
          //
          //[Chess 960]Castles bit is needed to distinguish castles from ambiguous King moves:
          //
          var castles = Move.Castles | (Move)((UInt32)Piece.K << nPieceBit);
          var OOTo = castles | (Move)(KingOOTo << nToBit);
          var OOOTo = castles | (Move)(KingOOOTo << nToBit);

          if (!CastlesFrom.HasValue) return;

          var qpKing = BIT0 << CastlesFrom.Value;

          if (RookOOFrom.HasValue) {
            var qpRook = BIT0 << RookOOFrom.Value;
            var qpMask = ~(qpKing | qpRook);
            var qpPath = rankPath(RookOOFrom.Value, RookOOTo);
            OOSafe = rankPath(CastlesFrom.Value, KingOOTo);
            OOPath = qpMask & (qpPath | OOSafe);
            //[Safe]addCastles() should not be called InCheck
            OOSafe |= qpKing;
            OO = OOTo | (Move)(CastlesFrom << nFromBit);
          }

          if (RookOOOFrom.HasValue) {
            var qpRook = BIT0 << RookOOOFrom.Value;
            var qpMask = ~(qpKing | qpRook);
            var qpPath = rankPath(RookOOOFrom.Value, RookOOOTo);
            OOOSafe = rankPath(CastlesFrom.Value, KingOOOTo);
            OOOPath = qpMask & (qpPath | OOOSafe);
            //[Safe]addCastles() should not be called InCheck
            OOOSafe |= qpKing;
            OOO = OOOTo | (Move)(CastlesFrom << nFromBit);
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
          var nMask = (BIT0 << nBits) - 1;
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
        #endregion

        #region Virtual Fields
        //
        // The following are readonly fields set by the Constructor:
        //
        public Int32 SetupRank { get; init; }

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
        // The following will be set by the Init() method:
        //
        public Move OO = Move.Undefined;
        public Plane? OOSafe;
        public Plane? OOPath;

        public Move OOO = Move.Undefined;
        public Plane? OOOSafe;
        public Plane? OOOPath;
        #endregion
      }
    }
  }
}