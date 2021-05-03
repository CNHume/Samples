//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2018-10-28 CNHume]Created Class
//
namespace Engine {
  using Exceptions;

  using System;

  using static Board;
  using static Position;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class CastleRule {
    public class CastleRuleParameter {
      #region Constructors
      public CastleRuleParameter(PositionParameter parameter) {
        Parameter = parameter;

        KingOOTo = (Int32)sq.g1 + Parameter.BaseRank;
        RookOOTo = (Int32)sq.f1 + Parameter.BaseRank;
        KingOOOTo = (Int32)sq.c1 + Parameter.BaseRank;
        RookOOOTo = (Int32)sq.d1 + Parameter.BaseRank;
      }
      #endregion

      #region Methods
      public void Clear() {
        //
        // Clear Castling State for parseFEN():
        //
        CastlesFrom = default;
        RookOOFrom = default;
        RookOOOFrom = default;

        OOSafe = default;
        OOPath = default;
        OO = Move.Undefined;

        OOOSafe = default;
        OOOPath = default;
        OOO = Move.Undefined;
      }

      //
      //[Chess 960]parseCastleRights() and InitCastleRules() allow for Chess 960 castling
      //
      public void Init() {
        //
        //[Chess 960]Castles bit required to distinguish castling from simple King moves:
        //
        var castles = Move.Castles | (Move)((UInt32)Piece.K << nPieceBit);
        var OOTo = castles | (Move)(KingOOTo << nToBit);
        var OOOTo = castles | (Move)(KingOOOTo << nToBit);

        if (CastlesFrom.HasValue) {
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
      }

      //
      // Return squares that must be unobstructed for a
      // King or Rook to reach nTo, along the base rank.
      // nFrom is skipped but nTo is included.
      //
      private static Plane rankPath(Int32 nFrom, Int32 nTo) {
        if (nFrom < nTo)
          nFrom++;                        // Skip Lo
        else if (nFrom > nTo) {
          nFrom--;                        // Skip Hi

          var n = nFrom;                  // Swap
          nFrom = nTo;
          nTo = n;
        }

        var nBits = nTo + 1 - nFrom;
        var nMask = (BIT0 << nBits) - 1;
        var qpPath = nMask << nFrom;
        return qpPath;
      }

      public Move Castles(Int32 nTo) {
        if (nTo == KingOOTo)
          return OO;

        if (nTo == KingOOOTo)
          return OOO;

        return Move.Undefined;
      }

      public HiFlags GrantCastling(Int32? nKingFrom, Int32 nRookFrom, Plane qpRook, Boolean bChess960) {
        var fhiCanCastle = (HiFlags)0;

        if (!CastlesFrom.HasValue) {
          if (!nKingFrom.HasValue)
            throw new ParsePositionException($"{Parameter.SideName} must have a King to castle");

          if (bChess960) {
            if (nKingFrom <= (Int32)sq.a1 + Parameter.BaseRank || (Int32)sq.h1 + Parameter.BaseRank <= nKingFrom)
              throw new ParsePositionException($"{Parameter.SideName} King cannot castle");
          }
          else {
            if (nKingFrom != (Int32)sq.e1 + Parameter.BaseRank)
              throw new ParsePositionException($"{Parameter.SideName} King must castle from {sq.e1}");
          }

          CastlesFrom = nKingFrom;
        }

        if (nRookFrom < nKingFrom) {
          if (RookOOOFrom.HasValue)
            throw new ParsePositionException($"Redundant {Parameter.SideName} OOO Ability");

          if ((qpRook & BIT0 << nRookFrom) == 0)
            throw new ParsePositionException($"No {Parameter.SideName} Rook for OOO");

          RookOOOFrom = nRookFrom;
          fhiCanCastle |= HiFlags.CanOOO;
        }
        else {
          if (RookOOFrom.HasValue)
            throw new ParsePositionException($"Redundant {Parameter.SideName} OO Ability");

          if ((qpRook & BIT0 << nRookFrom) == 0)
            throw new ParsePositionException($"No {Parameter.SideName} Rook for OO");

          RookOOFrom = nRookFrom;
          fhiCanCastle |= HiFlags.CanOO;
        }

        return fhiCanCastle;
      }
      #endregion

      #region Virtual Fields
      public readonly PositionParameter Parameter;

      public readonly Int32 KingOOTo;
      public readonly Int32 RookOOTo;
      public readonly Int32 KingOOOTo;
      public readonly Int32 RookOOOTo;

      public Plane? OOSafe;
      public Plane? OOPath;
      public Move OO = Move.Undefined;

      public Plane? OOOSafe;
      public Plane? OOOPath;
      public Move OOO = Move.Undefined;

      //
      //[Chess 960]Castles From squares are set by parseCastleRights():
      //
      public Int32? CastlesFrom;
      public Int32? RookOOFrom;
      public Int32? RookOOOFrom;
      #endregion
    }
  }
}
