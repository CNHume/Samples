//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
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
  using System.Runtime.CompilerServices;

  using static Position.PositionSide;

  //
  // Type Aliases:
  //
  using PieceCounter = UInt32;
  using PieceHashcode = UInt16;         // 10 bits
  using Plane = UInt64;

  partial class Board {
    internal partial class BoardSide {
      #region Fields
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

      #region Properties
      public Position Position { get; init; }
      public Board Board => Position;

      public BoardParameter Parameter { get; init; }
      #endregion                        // Properties

      #region Constructors
      public BoardSide(
        Position position,
        BoardParameter parameter) {
        Position = position;
        Parameter = parameter;
      }
      #endregion                        // Constructors

      #region Methods
      #region Init Methods
      // Called for every new child node by Position.Push()
      public void Clear() {
        PawnA8H1Atx = PawnA1H8Atx = Piece = 0UL;
        KingPos = default;

        //
        // Counts is used by Validate() and eval()
        //
        Counts = 0U;
#if HashPieces
        PieceHash = 0;
#endif
      }

      // Call only when ParsePosition() creates a new Root Position.
      public void ClearCastleRule() {
        Parameter.Rule.Clear();
      }

      // Called from InitRoot() to complete a Root Position.
      public void InitCastleRule() {
        Parameter.Rule.Init();
      }
      #endregion                        // Init Methods

      #region SideFlags Methods
      protected void ClrCanOO() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanOO;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void SetCanOO() {
        FlagsSide |= SideFlags.CanOO;
      }

      protected void ClrCanOOO() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanOOO;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void SetCanOOO() {
        FlagsSide |= SideFlags.CanOOO;
      }

      public void ClrCanCastle() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanCastle;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void ClrDark() {
        FlagsSide &= ~SideFlags.Dark;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void SetDark() {
        FlagsSide |= SideFlags.Dark;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void ClrLite() {
        FlagsSide &= ~SideFlags.Lite;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
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
    }                                   // BoardSide
  }                                     // Board
}
