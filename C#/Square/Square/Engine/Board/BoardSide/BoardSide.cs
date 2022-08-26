//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2021-01-29 CNHume]Created Class
//
// Conditionals:
//
#define HashPieces
#define HashCastlingRights

namespace Engine {
  using System;
  using System.Diagnostics;

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
      public SideFlags FlagsSide;       //[fside]Pair | CanCastle

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
      #endregion

      #region Constructors
      public BoardSide(
        Board board,
        PositionParameter parameter) {
        Board = board;
        Parameter = parameter;
      }
      #endregion                        // Constructors

      #region Methods
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

      #region SideFlags Methods
      protected void ClrCanOO() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanOO;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      public void SetCanOO() {
        FlagsSide |= SideFlags.CanOO;
      }

      protected void ClrCanOOO() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanOOO;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      public void SetCanOOO() {
        FlagsSide |= SideFlags.CanOOO;
      }

      public void InitCanCastle() {
        FlagsSide &= ~SideFlags.CanCastle;
      }

      public void ClrCanCastle() {
        var fsideOld = FlagsSide;
        InitCanCastle();
        hashCastlingRights(fsideOld, FlagsSide);
      }

      protected void ClrDark() {
        FlagsSide &= ~SideFlags.Dark;
      }

      protected void SetDark() {
        FlagsSide |= SideFlags.Dark;
      }

      protected void ClrLite() {
        FlagsSide &= ~SideFlags.Lite;
      }

      protected void SetLite() {
        FlagsSide |= SideFlags.Lite;
      }

      private void setInsufficient() {
        FlagsSide &= ~SideFlags.Insufficient;
        if (Board.IsInsufficient(Piece))
          FlagsSide |= SideFlags.Insufficient;
      }

      [Conditional("TestInsufficient")]
      public void TestInsufficient() {
        var sideInsufficient = Board.IsInsufficient(Piece);
        var fsideInsufficient = FlagsSide.Has(SideFlags.Insufficient);
        if (fsideInsufficient != sideInsufficient) {
          var sideName = Parameter.SideName.ToString();
          var message = $"f{sideName}SideInsufficient != {sideName.ToLower()}SideInsufficient";
          Debug.Assert(fsideInsufficient == sideInsufficient, message);
        }
      }
      #endregion                        // SideFlags Methods
      #endregion                        // Methods
    }
  }
}
