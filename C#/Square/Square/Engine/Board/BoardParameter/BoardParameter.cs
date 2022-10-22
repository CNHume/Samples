//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2021-05-01 CNHume]Created Class
//

namespace Engine {
  using System;

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;
  using Plane = UInt64;

  partial class Board {
    #region Constants
    public const Int32 Black = (Int32)SideName.Black;
    public const Int32 White = (Int32)SideName.White;
    public static readonly Int32 nSides = Enum.GetNames(typeof(SideName)).Length;
    #endregion

    public partial class BoardParameter {
      #region Constructors
      public BoardParameter(SideName sideName) {
        SideName = sideName;

        switch (SideName) {
        case SideName.Black:
          Direction = -1;
          ShiftA1H8 = Direction * nA1H8;
          ShiftA8H1 = Direction * nA8H1;
          ShiftRank = Direction * nFiles;

          StartRank = nRankLast;
          EnPassantRank = 2;

          (FileLeft, FileRight) = (qpFileH, qpFileA);
          RankLast = qpRank1;
          RankPass = qpRank6;
          Above = qpRank1 | qpRank2 | qpRank3 | qpRank4;

          Zobrist = ZobristBlack;
          ZobristRights = ZobristRightsBlack;
          break;

        case SideName.White:
          Direction = 1;
          ShiftA1H8 = Direction * nA1H8;
          ShiftA8H1 = Direction * nA8H1;
          ShiftRank = Direction * nFiles;

          StartRank = 0;
          EnPassantRank = invertRank(2);

          (FileLeft, FileRight) = (qpFileA, qpFileH);
          RankLast = qpRank8;
          RankPass = qpRank3;
          Above = qpRank8 | qpRank7 | qpRank6 | qpRank5;

          Zobrist = ZobristWhite;
          ZobristRights = ZobristRightsWhite;
          break;

        default:
          throw new ArgumentException(nameof(sideName));
        }

        Rule = new CastleRuleParameter(StartRank);
      }
      #endregion

      #region Pawn Advancement Fields
      public readonly Int32 Direction;

      public readonly Int32 StartRank;
      public readonly Int32 EnPassantRank;

      public readonly Int32 ShiftA1H8;
      public readonly Int32 ShiftA8H1;
      public readonly Int32 ShiftRank;

      public readonly Plane RankLast;
      public readonly Plane RankPass;
      public readonly Plane FileLeft;
      public readonly Plane FileRight;

      public readonly Plane Above;
      #endregion

      #region Virtual Fields
      public readonly SideName SideName;

      public CastleRuleParameter Rule { get; set; }
      public String? Symbol;

      public readonly Hashcode[][] Zobrist;
      public readonly Hashcode[] ZobristRights;
      #endregion
    }
  }
}
