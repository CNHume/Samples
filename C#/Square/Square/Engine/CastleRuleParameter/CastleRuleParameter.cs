//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2018-10-28 CNHume]Created Class
//
namespace Engine {
  using System;

  using static Board;
  using static Position;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  class CastleRuleParameter {
    #region Constructors
    public CastleRuleParameter(BoardParameter parameter) {
      Parameter = parameter;
      var nRank = Parameter.StartRank;
      KingOOTo = nRank + (Int32)sq.g1;
      RookOOTo = nRank + (Int32)sq.f1;
      KingOOOTo = nRank + (Int32)sq.c1;
      RookOOOTo = nRank + (Int32)sq.d1;

      OO = Move.Undefined;
      OOO = Move.Undefined;
    }
    #endregion

    #region Methods
    //
    // rankPath - Returns mask for squares that must not be obstructed (or attacked)
    //
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
    public BoardParameter Parameter { get; init; }

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
