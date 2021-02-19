//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2018-10-27 CNHume]Created Class
//
// Conditionals:
//
#define HashPieces

namespace Engine {
  using System;

  //
  // Type Aliases:
  //
  using Hashcode = System.UInt64;
  using PieceCounter = System.UInt32;
  using PieceHashcode = System.UInt16;  // 10 bits
  using Plane = System.UInt64;

  partial class Board {
    public class BoardSide {
      #region Constants
      public const Int32 Black = (Int32)SideName.Black;
      public const Int32 White = (Int32)SideName.White;
      public static readonly Int32 nSides = Enum.GetNames(typeof(SideName)).Length;
      #endregion

      #region Constructors
      public BoardSide(SideName sideName, Hashcode[][] zobrist) {
        this.SideName = sideName;
        this.Zobrist = zobrist;
        switch (this.SideName) {
        case SideName.Black:
          Above = qpRank1 | qpRank2 | qpRank3 | qpRank4;
          A1H8 = -nA1H8;
          A8H1 = -nA8H1;
          Rank = -nFiles;
          RankLast = qpRank1;
          RankPass = qpRank6;
          FileLeft = qpFileH;
          FileRight = qpFileA;
          break;
        case SideName.White:
          Above = qpRank8 | qpRank7 | qpRank6 | qpRank5;
          A1H8 = nA1H8;
          A8H1 = nA8H1;
          Rank = nFiles;
          RankLast = qpRank8;
          RankPass = qpRank3;
          FileLeft = qpFileA;
          FileRight = qpFileH;
          break;
        }
      }
      #endregion

      #region Virtual Methods
      public void Clear() {
        PawnA8H1Atx = PawnA1H8Atx = Piece = 0UL;
        KingPos = default;

        //
        // Counts is used by IsValid() and eval()
        //
        Counts = 0U;
#if HashPieces
        PieceHash = 0;
#endif
      }
      #endregion

      #region Static Methods
      //
      //[C#]The << and >> operators treat negative exponents
      // as unsigned p-bit values, where p is the PBL of the
      // data type size.  The shift overloads implement more
      // intuitive semantics of additive, signed exponents:
      //
      public static Plane shiftl(Plane qp, Int32 n) {
        return n < 0 ? qp >> -n : qp << n;
      }

      public static Plane shiftr(Plane qp, Int32 n) {
        return shiftl(qp, -n);
      }
      #endregion

      #region Static Fields
      public static String BlackSymbol;
      public static String WhiteSymbol;
      #endregion

      #region Pawn Advancement Fields
      public readonly Plane Above;
      public readonly Int32 A1H8;
      public readonly Int32 A8H1;
      public readonly Int32 Rank;
      public readonly Plane RankLast;
      public readonly Plane RankPass;
      public readonly Plane FileLeft;
      public readonly Plane FileRight;
      #endregion

      #region Virtual Fields
      public SideName SideName;
      public HiFlags FlagsHi;           //[fhi]BishopMask | CanCastleMask

      public PieceCounter Counts;

      public Byte? KingPos;

      public Plane PawnA1H8Atx;         // Attacked by Pawns
      public Plane PawnA8H1Atx;

      public Plane Piece;               // Pieces belonging to Side
      public Hashcode[][] Zobrist;
#if HashPieces
      public PieceHashcode PieceHash;
#endif
      #endregion
    }
  }
}
