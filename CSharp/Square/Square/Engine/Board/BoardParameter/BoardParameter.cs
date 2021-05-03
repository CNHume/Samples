//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2021-05-01 CNHume]Created Class
//

namespace Engine {
  using System;

  //
  // Type Aliases:
  //
  using Hashcode = System.UInt64;
  using Plane = System.UInt64;

  partial class Board {
    #region Constants
    public const Int32 Black = (Int32)SideName.Black;
    public const Int32 White = (Int32)SideName.White;
    public static readonly Int32 nSides = Enum.GetNames(typeof(SideName)).Length;
    #endregion

    public class BoardParameter {
      #region Constructors
      public BoardParameter(SideName sideName) {
        SideName = sideName;

        switch (SideName) {
        case SideName.Black:
          StartRank = nRankLast;
          ShiftA1H8 = -nA1H8;
          ShiftA8H1 = -nA8H1;
          ShiftRank = -nFiles;

          RankLast = qpRank1;
          RankPass = qpRank6;
          FileLeft = qpFileH;
          FileRight = qpFileA;
          Above = qpRank1 | qpRank2 | qpRank3 | qpRank4;

          Zobrist = ZobristBlack;
          break;

        case SideName.White:
          StartRank = 0;
          ShiftA1H8 = nA1H8;
          ShiftA8H1 = nA8H1;
          ShiftRank = nFiles;

          RankLast = qpRank8;
          RankPass = qpRank3;
          FileLeft = qpFileA;
          FileRight = qpFileH;
          Above = qpRank8 | qpRank7 | qpRank6 | qpRank5;

          Zobrist = ZobristWhite;
          break;
        }
      }
      #endregion

      #region Pawn Advancement Fields
      public readonly Int32 StartRank;
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

      public String Symbol;

      public readonly Hashcode[][] Zobrist;
      #endregion
    }
  }
}
