//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2021-01-29 CNHume]Created Class
//
// Conditionals:
//
#define HashPieces

namespace Engine {
  using System;

  using static Engine.Position;

  //
  // Type Aliases:
  //
  using PieceCounter = UInt32;
  using PieceHashcode = UInt16;                // 10 bits
  using Plane = UInt64;

  partial class Board {
    public class BoardSide {
      #region Constructors
      public BoardSide(PositionParameter parameter, CastleRuleParameter ruleParameter) {
        Parameter = parameter;
        Rule = ruleParameter;
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

      #region Virtual Fields
      public PositionParameter Parameter;
      public CastleRuleParameter Rule;

      public HiFlags FlagsHi;           //[fhi]BishopMask | CanCastleMask

      public PieceCounter Counts;

      public Byte? KingPos;

      public Plane PawnA1H8Atx;         // Attacked by Pawns
      public Plane PawnA8H1Atx;

      public Plane Piece;               // Pieces belonging to Side
#if HashPieces
      public PieceHashcode PieceHash;
#endif
      #endregion
    }
  }
}
