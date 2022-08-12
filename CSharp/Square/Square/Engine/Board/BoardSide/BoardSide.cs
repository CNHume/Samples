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
  using PieceHashcode = UInt16;         // 10 bits
  using Plane = UInt64;

  partial class Board {
    public partial class BoardSide {
      #region Virtual Fields
      public SideFlags FlagsSide;       //[fside]BishopMask | CanCastleMask

      public PieceCounter Counts;

      public Byte? KingPos;

      public Plane PawnA1H8Atx;         // Attacked by Pawns
      public Plane PawnA8H1Atx;

      public Plane Piece;               // Pieces belonging to side
#if HashPieces
      public PieceHashcode PieceHash;
#endif
      #endregion

      #region Read-Only Properties
      public Board Board { get; }
      public PositionParameter Parameter { get; }
      public CastleRuleParameter Rule { get; }
      #endregion

      #region Constructors
      public BoardSide(
        Board board,
        PositionParameter parameter,
        CastleRuleParameter ruleParameter) {
        Board = board;
        Parameter = parameter;
        Rule = ruleParameter;
      }
      #endregion                        // Constructors

      #region Init Methods
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
      #endregion                        // Init Methods
    }
  }
}
