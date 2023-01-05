//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2021-05-01 CNHume]Created Class
//

namespace Engine {
  using System;
  using System.Runtime.CompilerServices;

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

            PieceRank = y((Int32)Sq.a8);
            PawnRank = y((Int32)Sq.a7);
            EnPassantRank = y((Int32)Sq.a3);

            (FileLeft, FileRight) = (qpFileH, qpFileA);
            RankLast = qpRank1;
            RankPass = qpRank6;

            Zobrist = zobristBlack;
            ZobristRights = zobristRightsBlack;
            break;

          case SideName.White:
            PawnSense = 1;
            PawnA1H8 = PawnSense * nA1H8;
            PawnA8H1 = PawnSense * nA8H1;
            PawnStep = PawnSense * nFiles;

            PieceRank = y((Int32)Sq.a1);
            PawnRank = y((Int32)Sq.a2);
            EnPassantRank = y((Int32)Sq.a6);

            RankLast = qpRank8;
            RankPass = qpRank3;
            (FileLeft, FileRight) = (qpFileA, qpFileH);

            Zobrist = zobristWhite;
            ZobristRights = zobristRightsWhite;
            break;

          default:
            throw new ArgumentException(nameof(sideName));
          }

          Rule = new CastleRuleParameter(PieceRank);
        }
        #endregion                      // Constructors

        #region Pawn Advancement Fields
        protected readonly Int32 PawnSense;

        public readonly Int32 PieceRank;
        public readonly Int32 PawnRank;
        public readonly Int32 EnPassantRank;

        public readonly Int32 PawnA1H8;
        public readonly Int32 PawnA8H1;
        public readonly Int32 PawnStep;

        public readonly Plane RankLast;
        public readonly Plane RankPass;
        public readonly Plane FileLeft;
        public readonly Plane FileRight;
        #endregion                      // Pawn Advancement Fields

        #region Fields
        public readonly SideName SideName;

        public String? Symbol;

        public readonly Hashcode[][] Zobrist;
        public readonly Hashcode[] ZobristRights;
        #endregion                      // Fields

        #region Properties
        public CastleRuleParameter Rule { get; set; }
        #endregion                      // Properties

        #region Methods
        [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
        public Boolean IsAbove(Int32 nTo) {
          switch (SideName) {
          case SideName.Black:
            return nTo <= (Int32)Sq.h4;
          case SideName.White:
            return nTo >= (Int32)Sq.a5;
          default:
            throw new ArgumentException(nameof(SideName));
          }
        }

        [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
        public bool IsLastRank(Int32 nTo) {
          switch (SideName) {
          case SideName.Black:
            return nTo <= (Int32)Sq.h1;
          case SideName.White:
            return nTo >= (Int32)Sq.a8;
          default:
            throw new ArgumentException(nameof(SideName));
          }
        }
        #endregion                      // Methods
      }                                 // BoardParameter
    }                                   // BoardSide
  }                                     // Board
}
