//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2014-10-18 CNHume]Split Captures and Moves into this file
//
// Conditionals:
//
//#define DebugSquares
//#define DebugMoveColor
#define UnshadowRay
#define UnshadowRay2

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute

  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    #region Constant Move Masks
    protected const Move PawnMove = (Move)((Byte)Piece.P << nPieceBit);
    protected const Move KnightMove = (Move)((Byte)Piece.N << nPieceBit);
    protected const Move BishopMove = (Move)((Byte)Piece.B << nPieceBit);
    protected const Move RookMove = (Move)((Byte)Piece.R << nPieceBit);
    protected const Move QueenMove = (Move)((Byte)Piece.Q << nPieceBit);
    protected const Move KingMove = (Move)((Byte)Piece.K << nPieceBit);

    protected const Move PieceCapture = (Move)((UInt32)Piece.Capture << nCaptiveBit);
    #endregion                          // Constant Move Masks

    #region Methods
    // getPieceIndex - Returns Piece at any square
    //
    // Move Generators:
    //
    // addCastles
    // addPieceMoves
    // addPieceCaptures
    // addPawnMoves
    // addPawnCaptures
    //
    #region Castling Moves
    protected void addCastles() {
      Debug.Assert(!InCheck(), "addCastles() called while InCheck");
      var friendRule = Friend.Rule;

      if (CanOO()) {
#if DebugMoveColor
        PseudoCastles.Add(Move.WTM | friendRule.OO);
#else
        PseudoCastles.Add(friendRule.OO);
#endif
      }

      if (CanOOO()) {
#if DebugMoveColor
        PseudoCastles.Add(Move.WTM | friendRule.OOO);
#else
        PseudoCastles.Add(friendRule.OOO);
#endif
      }
    }
    #endregion                          // Castling Moves

    #region Pawn Moves
    public void AddPawnCapture(
      Int32 nFrom, Int32 nTo, Boolean bAbove, Boolean bPromote, Boolean bEnPassant) {
#if DebugSquares
      Debug.Assert(getPieceIndex(nFrom) == vP6, "Piece not a Pawn");
      Debug.Assert((nTo & uSquareMask) == nTo, "To Overflow");
#endif
      var capture = bEnPassant ? Piece.EP : Piece.Capture;
      var move = captureMove(capture) | PawnMove | fromToMove(nFrom, nTo);
#if DebugMoveColor
      if (WTM()) move |= Move.WTM;
#endif
      if (bPromote)
        foreach (var p in Promotions) {
          var moves = p == Piece.Q ?
            PseudoQueenPromotionCapture : PseudoUnderPromotionCapture;
          moves.Add(promotionMove(p) | move);
        }
      else if (bEnPassant) {
#if DebugSquares
        Debug.Assert(getPieceIndex(nTo) == vPieceNull, "Passed Square not Empty");
#endif
        PseudoEPCapture.Add(move);
      }
      else {
        var moves = bAbove ? PseudoPawnAboveCapture : PseudoPawnBelowCapture;
        moves.Add(move);
      }
    }

    public void AddPawnMove(
      Int32 nFrom, Int32 nTo, Boolean bAbove, Boolean bPromote) {
#if DebugSquares
      Debug.Assert(getPieceIndex(nFrom) == vP6, "Piece not a Pawn");
      Debug.Assert((nTo & uSquareMask) == nTo, "To Overflow");
#endif
      var move = PawnMove | fromToMove(nFrom, nTo);
#if DebugMoveColor
      if (WTM()) move |= Move.WTM;
#endif
      if (bPromote)
        foreach (var p in Promotions) {
          var moves = p == Piece.Q ? PseudoQueenPromotion : PseudoUnderPromotion;
          moves.Add(promotionMove(p) | move);
        }
      else {
        var moves = bAbove ? PseudoPawnAboveMove : PseudoPawnBelowMove;
        moves.Add(move);
      }
    }
    #endregion                          // Pawn Moves

    #region Piece Moves
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void addPieceCaptures(
      List<Move> aboveCaptures, List<Move> belowCaptures, Move moveFrom, Plane qpMoveTo) {
      var bWTM = WTM();
      qpMoveTo &= RankPiece;            // Find Captures
      while (qpMoveTo != 0) {
        var nTo = RemoveLo(ref qpMoveTo);
        var move = PieceCapture | toMove(nTo) | moveFrom;
#if DebugMoveColor
        if (bWTM) move |= Move.WTM;
#endif
        var moves = isAbove(nTo, bWTM) ? aboveCaptures : belowCaptures;
        moves.Add(move);
      }
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void addPieceMoves(
      List<Move> aboveMoves, List<Move> belowMoves, Move moveFrom, Plane qpMoveTo) {
      var bWTM = WTM();
      qpMoveTo &= ~RankPiece;           // Find Moves
      while (qpMoveTo != 0) {
        var nTo = RemoveLo(ref qpMoveTo);
        var move = toMove(nTo) | moveFrom;
#if DebugMoveColor
        if (bWTM) move |= Move.WTM;
#endif
        var moves = isAbove(nTo, bWTM) ? aboveMoves : belowMoves;
        moves.Add(move);
      }
    }

    //[Note]toggleWTM() inverts the conventional sense of Friend and Foe.
#if UnshadowRay2
    private void addKingCapturesAndMoves(Plane qpTo, Byte vKingPos, Boolean bRayCheck) {
#else
    private void addKingCapturesAndMoves(Plane qpTo, Byte vKingPos) {
#endif
      // Each side has one King
      var moveFrom = KingMove | fromMove(vKingPos);
      var qpMoveTo = KingAtx[vKingPos] & qpTo;
#if UnshadowRay
      if (bRayCheck) {
        //
        // Briefly remove the King being moved from position to
        // unshadow its destination squares from ray attacks:
        //
        clrRayState(vKingPos);
        qpMoveTo &= Foe.Safe(qpMoveTo);
        setRayState(vKingPos);
      }
      else
#endif
        qpMoveTo &= Foe.Safe(qpMoveTo);

      addPieceCaptures(PseudoKingCapture, PseudoKingCapture, moveFrom, qpMoveTo);
      addPieceMoves(PseudoKingMove, PseudoKingMove, moveFrom, qpMoveTo);
    }

    //[Note]toggleWTM() inverts the conventional sense of Friend and Foe.
#if UnshadowRay2
    private void addKingCaptures(Plane qpTo, Byte vKingPos, Boolean bRayCheck) {
#else
    private void addKingCaptures(Plane qpTo, Byte vKingPos) {
#endif
      // Each side has one King
      var moveFrom = KingMove | fromMove(vKingPos);
      var qpMoveTo = KingAtx[vKingPos] & qpTo;
#if UnshadowRay
      if (bRayCheck) {
        //
        // Briefly remove the King being moved from position to
        // unshadow its destination squares from ray attacks:
        //
        clrRayState(vKingPos);
        qpMoveTo &= Foe.Safe(qpMoveTo);
        setRayState(vKingPos);
      }
      else
#endif
        qpMoveTo &= Foe.Safe(qpMoveTo);

      addPieceCaptures(PseudoKingCapture, PseudoKingCapture, moveFrom, qpMoveTo);
    }

    private void addKnightCapturesAndMoves(Plane qpTo) {
      var qpKnight = Friend.Piece & Knight;
      while (qpKnight != 0) {
        var nFrom = RemoveLo(ref qpKnight);
        var moveFrom = KnightMove | fromMove(nFrom);
        var qpMoveTo = KnightAtx[nFrom] & qpTo;
        addPieceCaptures(PseudoKnightCapture, PseudoKnightCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoKnightMove, PseudoKnightMove, moveFrom, qpMoveTo);
      }
    }

    private void addKnightCaptures(Plane qpTo) {
      var qpKnight = Friend.Piece & Knight;
      while (qpKnight != 0) {
        var nFrom = RemoveLo(ref qpKnight);
        var moveFrom = KnightMove | fromMove(nFrom);
        var qpMoveTo = KnightAtx[nFrom] & qpTo;
        addPieceCaptures(PseudoKnightCapture, PseudoKnightCapture, moveFrom, qpMoveTo);
      }
    }

    private void addBishopCapturesAndMoves(Plane qpTo) {
      var qpBishop = Friend.Piece & Bishop;
      while (qpBishop != 0) {
        var nFrom = RemoveLo(ref qpBishop);
        var moveFrom = BishopMove | fromMove(nFrom);
        var qpMoveTo = diagAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoDiagAboveMove, PseudoDiagBelowMove, moveFrom, qpMoveTo);
      }
    }

    private void addBishopCaptures(Plane qpTo) {
      var qpBishop = Friend.Piece & Bishop;
      while (qpBishop != 0) {
        var nFrom = RemoveLo(ref qpBishop);
        var moveFrom = BishopMove | fromMove(nFrom);
        var qpMoveTo = diagAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpMoveTo);
      }
    }

    private void addRookCapturesAndMoves(Plane qpTo) {
      var qpRook = Friend.Piece & Rook;
      while (qpRook != 0) {
        var nFrom = RemoveLo(ref qpRook);
        var moveFrom = RookMove | fromMove(nFrom);
        var qpMoveTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoRectAboveMove, PseudoRectBelowMove, moveFrom, qpMoveTo);
      }
    }

    private void addRookCaptures(Plane qpTo) {
      var qpRook = Friend.Piece & Rook;
      while (qpRook != 0) {
        var nFrom = RemoveLo(ref qpRook);
        var moveFrom = RookMove | fromMove(nFrom);
        var qpMoveTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpMoveTo);
      }
    }

    private void addQueenCapturesAndMoves(Plane qpTo) {
      var qpQueen = Friend.Piece & Queen;
      while (qpQueen != 0) {
        var nFrom = RemoveLo(ref qpQueen);
        var moveFrom = QueenMove | fromMove(nFrom);
        var qpDiagTo = diagAtx(nFrom) & qpTo;
        var qpRectTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpDiagTo);
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpRectTo);
        addPieceMoves(PseudoDiagAboveMove, PseudoDiagBelowMove, moveFrom, qpDiagTo);
        addPieceMoves(PseudoRectAboveMove, PseudoRectBelowMove, moveFrom, qpRectTo);
      }
    }

    private void addQueenCaptures(Plane qpTo) {
      var qpQueen = Friend.Piece & Queen;
      while (qpQueen != 0) {
        var nFrom = RemoveLo(ref qpQueen);
        var moveFrom = QueenMove | fromMove(nFrom);
        var qpDiagTo = diagAtx(nFrom) & qpTo;
        var qpRectTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpDiagTo);
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpRectTo);
      }
    }

    //[Time]Called by performTests()
    //~600 KHz, ~900 KHz sans List<Move>.Add()
    private void addPieceCapturesAndMoves2(Plane qpTo) {
      addKnightCapturesAndMoves(qpTo);  //~1670 KHz
      addBishopCapturesAndMoves(qpTo);  //~1940 KHz
      addRookCapturesAndMoves(qpTo);    //~1700 KHz
      addQueenCapturesAndMoves(qpTo);   //~1050 KHz
      //~400 KHz
    }

    // Used by generate()
    private void addPieceCapturesAndMoves(Plane qpTo) {
      addKnightCapturesAndMoves(qpTo);
      addBishopCapturesAndMoves(qpTo);
      addRookCapturesAndMoves(qpTo);
      addQueenCapturesAndMoves(qpTo);
    }

    // Used by generateSwaps()
    private void addPieceCaptures(Plane qpTo) {
      addKnightCaptures(qpTo);
      addBishopCaptures(qpTo);
      addRookCaptures(qpTo);
      addQueenCaptures(qpTo);
    }
    #endregion                          // Piece Moves
    #endregion                          // Methods
  }
}
