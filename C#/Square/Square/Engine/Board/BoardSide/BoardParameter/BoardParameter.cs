//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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

            SetupRank = (Int32)sq.a8;
            EnPassantRank = 2;

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

            SetupRank = (Int32)sq.a1;
            EnPassantRank = InvertRank(2);

            RankLast = qpRank8;
            RankPass = qpRank3;
            (FileLeft, FileRight) = (qpFileA, qpFileH);

            Zobrist = zobristWhite;
            ZobristRights = zobristRightsWhite;
            break;

          default:
            throw new ArgumentException(nameof(sideName));
          }

          Rule = new CastleRuleParameter(SetupRank);
        }
        #endregion                      // Constructors

        #region Pawn Advancement Fields
        protected readonly Int32 PawnSense;

        public readonly Int32 SetupRank;
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
            return nTo <= (Int32)sq.h4;
          case SideName.White:
            return nTo >= (Int32)sq.a5;
          default:
            throw new ArgumentException(nameof(SideName));
          }
        }

        [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
        public bool IsLastRank(Int32 nTo) {
          switch (SideName) {
          case SideName.Black:
            return nTo <= (Int32)sq.h1;
          case SideName.White:
            return nTo >= (Int32)sq.a8;
          default:
            throw new ArgumentException(nameof(SideName));
          }
        }
        #endregion                      // Methods
      }                                 // BoardParameter
    }                                   // BoardSide
  }                                     // Board
}
