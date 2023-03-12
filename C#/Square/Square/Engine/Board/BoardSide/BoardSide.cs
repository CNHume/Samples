//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2021-01-29 CNHume]Created Class
//
// Conditionals:
//
#define HashPieces
#define HashCastlingRights

using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Engine {
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

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void Copy(BoardSide side) {
        // 34 bytes + 1 nullable byte
#if HashPieces
        PieceHash = side.PieceHash;     // 8-bytes
#endif
        Counts = side.Counts;           // 2-bytes
        KingPos = side.KingPos;         // 1-byte (nullable)

        Piece = side.Piece;             // 8-bytes
        PawnA1H8Atx = side.PawnA1H8Atx; // 8-bytes optimizing resetPawnAtx()
        PawnA8H1Atx = side.PawnA8H1Atx; // 8-bytes
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void CopyFlags(BoardSide side) {
        FlagsSide = side.FlagsSide & SideFlags.Copy;
      }
      #endregion                        // Constructors

      #region Methods
      #region Init Methods
      // Called for every new child node by Position.Push()
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
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

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Boolean EqualFlags(BoardSide side) {
        var fSideDelta = FlagsSide ^ side.FlagsSide;
        return !fSideDelta.Has(SideFlags.Copy);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Boolean Equals(BoardSide side) {
        return
          Piece == side.Piece &&
          EqualFlags(side);
      }

      // Call only when ParsePosition() creates a new Root Position.
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void ClearCastleRule() {
        Parameter.Rule.Clear();
      }

      // Called from InitRoot() to complete a Root Position.
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void InitCastleRule() {
        Parameter.Rule.Init();
      }
      #endregion                        // Init Methods

      #region SideFlags Methods
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void ClrCanOO() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanOO;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void SetCanOO() {
        FlagsSide |= SideFlags.CanOO;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void ClrCanOOO() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanOOO;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void SetCanOOO() {
        FlagsSide |= SideFlags.CanOOO;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void ClrCanCastle() {
        var fsideOld = FlagsSide;
        FlagsSide &= ~SideFlags.CanCastle;
        hashCastlingRights(fsideOld, FlagsSide);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      private void clrDark() {
        FlagsSide &= ~SideFlags.Dark;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      private void setDark() {
        FlagsSide |= SideFlags.Dark;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      private void clrLite() {
        FlagsSide &= ~SideFlags.Lite;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      private void setLite() {
        FlagsSide |= SideFlags.Lite;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      private void setInsufficient() {
        FlagsSide &= ~SideFlags.AloneOrInsufficient;

        if (IsOneOrNone(Piece))
          FlagsSide |= SideFlags.Alone;
        else if (Board.IsInsufficient(Piece))
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

      public String MoveError(UInt16 wGamePly, String sMessage) {
        var wMove = MoveNumber(wGamePly);
        return $"Move {wMove} {Parameter.SideName}: {sMessage}";
      }
      #endregion                        // Methods
    }                                   // BoardSide
  }                                     // Board
}
