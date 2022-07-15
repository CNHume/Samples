﻿//
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
    #endregion

    #region Castling Moves
    protected void addCastles(BoardSide friend, BoardSide foe) {
      Debug.Assert(!InCheck(), "addCastles() called while InCheck");
      var friendRule = friend.Rule;

      if (canOO(friend, foe)) {
#if DebugMoveColor
        PseudoCastles.Add(Move.WTM | friendRule.OO);
#else
        PseudoCastles.Add(friendRule.OO);
#endif
      }

      if (canOOO(friend, foe)) {
#if DebugMoveColor
        PseudoCastles.Add(Move.WTM | friendRule.OOO);
#else
        PseudoCastles.Add(friendRule.OOO);
#endif
      }
    }
    #endregion

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
    #endregion

    #region Piece Moves
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void addPieceCaptures(List<Move> aboveCaptures, List<Move> belowCaptures, Move moveFrom, Plane qpMoveTo) {
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
    protected void addPieceMoves(List<Move> aboveMoves, List<Move> belowMoves, Move moveFrom, Plane qpMoveTo) {
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
#if UnshadowRay2
    private void addKingCapturesAndMoves(Plane qpTo, Byte vKingPos, Boolean bRayCheck) {
#else
    private void addKingCapturesAndMoves(Plane qpTo, Byte vKingPos) {
#endif
      // Each side has one King
      var moveFrom = KingMove | fromMove(vKingPos);
      var qpMoveTo = KingAtx[vKingPos] & qpTo;

      var bWTM = WTM();
      var foe = getSide(!bWTM);
#if UnshadowRay
      if (bRayCheck) {
        //
        // Briefly remove the King being moved from position to
        // unshadow its destination squares from ray attacks:
        //
        clrRayState(vKingPos);
        qpMoveTo &= foe.Safe(qpMoveTo);
        setRayState(vKingPos);
      }
      else
#endif
        qpMoveTo &= foe.Safe(qpMoveTo);

      addPieceCaptures(PseudoKingCapture, PseudoKingCapture, moveFrom, qpMoveTo);
      addPieceMoves(PseudoKingMove, PseudoKingMove, moveFrom, qpMoveTo);
    }
#if UnshadowRay2
    private void addKingCaptures(Plane qpTo, Byte vKingPos, Boolean bRayCheck) {
#else
    private void addKingCaptures(Plane qpTo, Byte vKingPos) {
#endif
      // Each side has one King
      var moveFrom = KingMove | fromMove(vKingPos);
      var qpMoveTo = KingAtx[vKingPos] & qpTo;

      var bWTM = WTM();
      var foe = getSide(!bWTM);
#if UnshadowRay
      if (bRayCheck) {
        //
        // Briefly remove the King being moved from position to
        // unshadow its destination squares from ray attacks:
        //
        clrRayState(vKingPos);
        qpMoveTo &= foe.Safe(qpMoveTo);
        setRayState(vKingPos);
      }
      else
#endif
        qpMoveTo &= foe.Safe(qpMoveTo);

      addPieceCaptures(PseudoKingCapture, PseudoKingCapture, moveFrom, qpMoveTo);
    }

    private void addKnightCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = KnightMove | fromMove(nFrom);
        var qpMoveTo = KnightAtx[nFrom] & qpTo;
        addPieceCaptures(PseudoKnightCapture, PseudoKnightCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoKnightMove, PseudoKnightMove, moveFrom, qpMoveTo);
      }
    }

    private void addKnightCaptures(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = KnightMove | fromMove(nFrom);
        var qpMoveTo = KnightAtx[nFrom] & qpTo;
        addPieceCaptures(PseudoKnightCapture, PseudoKnightCapture, moveFrom, qpMoveTo);
      }
    }

    private void addBishopCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = BishopMove | fromMove(nFrom);
        var qpMoveTo = diagAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoDiagAboveMove, PseudoDiagBelowMove, moveFrom, qpMoveTo);
      }
    }

    private void addBishopCaptures(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = BishopMove | fromMove(nFrom);
        var qpMoveTo = diagAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpMoveTo);
      }
    }

    private void addRookCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = RookMove | fromMove(nFrom);
        var qpMoveTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoRectAboveMove, PseudoRectBelowMove, moveFrom, qpMoveTo);
      }
    }

    private void addRookCaptures(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = RookMove | fromMove(nFrom);
        var qpMoveTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpMoveTo);
      }
    }

    private void addQueenCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = QueenMove | fromMove(nFrom);
        var qpDiagTo = diagAtx(nFrom) & qpTo;
        var qpRectTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpDiagTo);
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpRectTo);
        addPieceMoves(PseudoDiagAboveMove, PseudoDiagBelowMove, moveFrom, qpDiagTo);
        addPieceMoves(PseudoRectAboveMove, PseudoRectBelowMove, moveFrom, qpRectTo);
      }
    }

    private void addQueenCaptures(Plane qpTo, Plane qpPiece) {
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = QueenMove | fromMove(nFrom);
        var qpDiagTo = diagAtx(nFrom) & qpTo;
        var qpRectTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpDiagTo);
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpRectTo);
      }
    }

    // Used by generate()
    private void addPieceCapturesAndMoves(Plane qpTo, Plane qpFriend) {  //~600 KHz, ~900 KHz sans List<Move>.Add()
      addKnightCapturesAndMoves(qpTo, qpFriend & Knight);
      addBishopCapturesAndMoves(qpTo, qpFriend & Bishop);
      addRookCapturesAndMoves(qpTo, qpFriend & Rook);
      addQueenCapturesAndMoves(qpTo, qpFriend & Queen);
    }

    //[Time]Called by performTests()
    private void addPieceCapturesAndMoves2(Plane qpTo, Plane qpFriend) {  //~600 KHz, ~900 KHz sans List<Move>.Add()
      addKnightCapturesAndMoves(qpTo, qpFriend & Knight);     //~1670 KHz
      addBishopCapturesAndMoves(qpTo, qpFriend & Bishop);     //~1940 KHz
      addRookCapturesAndMoves(qpTo, qpFriend & Rook);         //~1700 KHz
      addQueenCapturesAndMoves(qpTo, qpFriend & Queen);       //~1050 KHz
      //~400 KHz
    }

    // Used by generateSwaps()
    private void addPieceCaptures(Plane qpTo, Plane qpFriend) {
      addKnightCaptures(qpTo, qpFriend & Knight);
      addBishopCaptures(qpTo, qpFriend & Bishop);
      addRookCaptures(qpTo, qpFriend & Rook);
      addQueenCaptures(qpTo, qpFriend & Queen);
    }
    #endregion
  }
}
