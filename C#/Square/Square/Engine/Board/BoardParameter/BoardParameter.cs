﻿//
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
          PawnSense = -1;
          ShiftA1H8 = PawnSense * nA1H8;
          ShiftA8H1 = PawnSense * nA8H1;
          ShiftRank = PawnSense * nFiles;

          StartRank = nRankLast;
          EnPassantRank = 2;

          (FileLeft, FileRight) = (qpFileH, qpFileA);
          RankLast = qpRank1;
          RankPass = qpRank6;

          Zobrist = ZobristBlack;
          ZobristRights = ZobristRightsBlack;
          break;

        case SideName.White:
          PawnSense = 1;
          ShiftA1H8 = PawnSense * nA1H8;
          ShiftA8H1 = PawnSense * nA8H1;
          ShiftRank = PawnSense * nFiles;

          StartRank = 0;
          EnPassantRank = invertRank(2);

          RankLast = qpRank8;
          RankPass = qpRank3;
          (FileLeft, FileRight) = (qpFileA, qpFileH);

          Zobrist = ZobristWhite;
          ZobristRights = ZobristRightsWhite;
          break;

        default:
          throw new ArgumentException(nameof(sideName));
        }

        Rule = new CastleRuleParameter(StartRank);
      }
      #endregion                        // Constructors

      #region Pawn Advancement Fields
      public readonly Int32 PawnSense;

      public readonly Int32 StartRank;
      public readonly Int32 EnPassantRank;

      public readonly Int32 ShiftA1H8;
      public readonly Int32 ShiftA8H1;
      public readonly Int32 ShiftRank;

      public readonly Plane RankLast;
      public readonly Plane RankPass;
      public readonly Plane FileLeft;
      public readonly Plane FileRight;
      #endregion

      #region Virtual Fields
      public readonly SideName SideName;

      public String? Symbol;

      public readonly Hashcode[][] Zobrist;
      public readonly Hashcode[] ZobristRights;
      #endregion                        // Virtual Fields

      #region Properties
      public CastleRuleParameter Rule { get; set; }
      #endregion                        // Properties

      #region Methods
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Boolean IsAbove(Int32 nTo) {
        var bAbove = PawnSense > 0 ?
          nTo >= (Int32)sq.a5 : nTo <= (Int32)sq.h4;
#if TestAbove
        var sAbove = bAbove ? "above" : "below";
        var sqTo = (sq)nTo;
        if (sqTo == sq.a5 || sqTo == sq.a4)
          Console.WriteLine($"{sqTo} is {sAbove} for {SideName}");
#endif
        return bAbove;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public bool IsPromotion(int nTo) {
        var bPromotion = PawnSense > 0 ?
          nTo >= (Int32)sq.a8 : nTo <= (Int32)sq.h1;
        return bPromotion;
      }
      #endregion                        // Methods
    }
  }
}
