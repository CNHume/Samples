//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2021-05-01 CNHume]Created Class
//

using System.Runtime.CompilerServices;

namespace Engine;
//
// Type Aliases:
//
using Hashcode = UInt64;
using Plane = UInt64;

partial class Board {
  internal partial class BoardSide {
    internal partial class BoardParameter {
      #region Constructors
      public BoardParameter(SideName sideName) {
        SideName = sideName;

        switch (SideName) {
        case SideName.Black:
          PawnSense = -1;
          PawnA1H8 = PawnSense * nA1H8;
          PawnA8H1 = PawnSense * nA8H1;
          PawnStep = PawnSense * nFiles;

          PieceRank = 7;
          PawnRank = 6;
          PassRank = 5;
          BelowRank = 4;

          EnPassantMask = qpRank6;
          PromotionMask = qpRank1;
          (FileLeft, FileRight) = (qpFileH, qpFileA);

          Zobrist = zobristBlack;
          ZobristRights = zobristRightsBlack;
          break;

        case SideName.White:
          PawnSense = 1;
          PawnA1H8 = PawnSense * nA1H8;
          PawnA8H1 = PawnSense * nA8H1;
          PawnStep = PawnSense * nFiles;

          PieceRank = 0;
          PawnRank = 1;
          PassRank = 2;
          BelowRank = 3;

          EnPassantMask = qpRank3;
          PromotionMask = qpRank8;
          (FileLeft, FileRight) = (qpFileA, qpFileH);

          Zobrist = zobristWhite;
          ZobristRights = zobristRightsWhite;
          break;

        default:
          throw new ArgumentException(nameof(sideName));
        }

        Rule = new(PieceRank);
      }
      #endregion                        // Constructors

      #region Pawn Advancement Fields
      protected readonly Int32 PawnSense;

      public readonly Int32 PawnA1H8;
      public readonly Int32 PawnA8H1;
      public readonly Int32 PawnStep;

      //[Note]Ranks are Zero-based
      public readonly Int32 PieceRank;
      public readonly Int32 PawnRank;
      public readonly Int32 PassRank;
      public readonly Int32 BelowRank;

      public readonly Plane EnPassantMask;
      public readonly Plane PromotionMask;
      public readonly Plane FileLeft;
      public readonly Plane FileRight;
      #endregion                        // Pawn Advancement Fields

      #region Fields
      public readonly SideName SideName;

      public String? Symbol;

      public readonly Hashcode[][] Zobrist;
      public readonly Hashcode[] ZobristRights;
      #endregion                        // Fields

      #region Properties
      public CastleRuleParameter Rule { get; set; }
      #endregion                        // Properties

      #region Methods
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Boolean IsAbove(Int32 nTo) {
        switch (SideName) {
        case SideName.Black:
          return y(nTo) < BelowRank;
        case SideName.White:
          return y(nTo) > BelowRank;
        default:
          throw new ArgumentException(nameof(SideName));
        }
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Boolean IsPromotion(Int32 nTo) {
        var qp = bit(nTo);
        return (qp & PromotionMask) != 0;
      }
      #endregion                      // Methods
    }                                   // BoardParameter
  }                                     // BoardSide
}                                       // Board
