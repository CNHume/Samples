﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2021-01-29 CNHume]Created Class
//
// Conditionals:
//
#define HashPieces
#define HashCastlingRights

using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Engine;

using Exceptions;

//
// Type Aliases:
//
using PieceCounter = UInt32;
using PieceHashcode = UInt16;           // 10 bits
using Plane = UInt64;

partial class Board {
  internal partial class BoardSide {
    #region Fields
    public SideFlags FlagsSide;       //[fside]Pair | CanCastle
    public Byte? KingPos;             //[Nullable]
    public PieceCounter Counts;       // For Validate() and eval()

    public Plane Piece;               // Pieces belonging to Side
    public Plane PawnA1H8Atx;         // Pawn Attack Squares to optimize resetPawnAtx()
    public Plane PawnA8H1Atx;
#if HashPieces
    public PieceHashcode PieceHash;   // Composition Hash
#endif
    #endregion                        // Fields

    #region Properties
    public Position Position { get; init; }
    public Board Board => Position;

    public BoardParameter Parameter { get; init; }
    #endregion                          // Properties

    #region Constructors
    public BoardSide(
      Position position,
      BoardParameter parameter) {
      Position = position;
      Parameter = parameter;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void copyFlags(BoardSide side) {
      FlagsSide = side.FlagsSide & SideFlags.Copy;
    }

    public void Copy(BoardSide side) {
      // 35 bytes + 1 nullable byte
      copyFlags(side);                  // 1-byte for BoardSide Flags
      KingPos = side.KingPos;           // 1-byte (nullable)
      Counts = side.Counts;             // 2-bytes

      Piece = side.Piece;               // 8-bytes
      PawnA1H8Atx = side.PawnA1H8Atx;   // 8-bytes
      PawnA8H1Atx = side.PawnA8H1Atx;   // 8-bytes
#if HashPieces
      PieceHash = side.PieceHash;       // 8-bytes
#endif
    }
    #endregion                          // Constructors

    #region Methods
    #region Init Methods
    // Called for every new child node by Position.Push()
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void Clear() {
      FlagsSide = default;
      KingPos = default;

      PawnA8H1Atx = PawnA1H8Atx = Piece = 0UL;

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

    // Called from Setup() to complete the Root Position.
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void InitCastleRule() {
      Parameter.Rule.Init();
    }
    #endregion                          // Init Methods

    #region Move Methods
    public Move BuildMove(
      String sMove, Piece promotion, Int32 nFrom, Int32 nTo,
      Plane qpTo, Byte vPiece, Byte vCapture, Boolean bCapture) {

      //
      // Validate Non-Castling Move
      //
      var qpAtxTo = Board.PieceAtx(vPiece, nFrom, bCapture);
      var piece = IndexPiece(vPiece);

      if (!qpAtxTo.HasValue)            //[Safe]
        throw new ParseException(
          MoveError($"Unexpected move of {piece}", (Sq)nFrom, (Sq)nTo));

      qpAtxTo &= ~Piece;
      if ((qpAtxTo & qpTo) == 0)
        throw new MoveException(
          MoveError($"Cannot move {piece}", (Sq)nFrom, (Sq)nTo));

      //
      // Validate Promotion
      //
      var bRequired = vPiece == vP6 && Parameter.IsPromotion(nTo);
      var bSupplied = promotion != default;
      if (bRequired != bSupplied) {
        var sDiagnosis = bRequired ? "Required" : "Illegal";
        throw new MoveException(
          MoveError($"Promotion {sDiagnosis} in {sMove}"));
      }

      var move = PromotionPiece(promotion) | movePiece(piece) | MoveFromTo(nFrom, nTo);
      if (bCapture) move |= CapturePiece(IndexPiece(vCapture));
      return move;
    }

    public String MoveError(String sMessage) {
      var wMove = MoveNumber(Board.GamePly);
      return $"Move {wMove} {Parameter.SideName}: {sMessage}";
    }

    public String MoveError(String sMessage, Sq sqFrom, Sq sqTo) {
      return MoveError($"{sMessage} from {sqFrom} to {sqTo}");
    }
    #endregion                          // Move Methods

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
      const Boolean bHelpmate = true;
      FlagsSide &= ~SideFlags.AloneOrInsufficient;

      if (IsOneOrLess(Piece))
        FlagsSide |= SideFlags.Alone;
      else if (Board.IsInsufficient(Piece, bHelpmate))
        FlagsSide |= SideFlags.Insufficient;
    }

    [Conditional("TestInsufficient")]
    public void TestInsufficient() {
      const Boolean bHelpmate = true;
      var sideInsufficient = Board.IsInsufficient(Piece, bHelpmate);
      var fsideInsufficient = FlagsSide.Has(SideFlags.Insufficient);
      if (fsideInsufficient != sideInsufficient) {
        var sideName = Parameter.SideName.ToString();
        var message = $"f{sideName}SideInsufficient != {sideName.ToLower()}SideInsufficient";
        Debug.Assert(fsideInsufficient == sideInsufficient, message);
      }
    }
    #endregion                          // SideFlags Methods
    #endregion                          // Methods
  }                                     // BoardSide
}                                       // Board
