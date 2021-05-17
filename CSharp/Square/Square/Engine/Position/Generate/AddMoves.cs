//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2014-10-18 CNHume]Split Captures and Moves into this file
//
// Conditionals:
//
//#define DebugSquares
//#define DebugMoveColor
//#define TestPawnAdvances
#define UnshadowRay
#define UnshadowRay2

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute

  using static CastleRule;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Plane = System.UInt64;

  partial class Position : Board {
    #region Constant Move Masks
    protected const Move PawnMask = (Move)((Byte)Piece.P << nPieceBit);
    protected const Move KnightMask = (Move)((Byte)Piece.N << nPieceBit);
    protected const Move BishopMask = (Move)((Byte)Piece.B << nPieceBit);
    protected const Move RookMask = (Move)((Byte)Piece.R << nPieceBit);
    protected const Move QueenMask = (Move)((Byte)Piece.Q << nPieceBit);
    protected const Move KingMask = (Move)((Byte)Piece.K << nPieceBit);

    protected const Move PieceCapture = (Move)((UInt32)Piece.Capture << nCaptiveBit);
    #endregion

    #region Castling Moves
    protected void addCastles(BoardSide friend, CastleRuleParameter friendRule, BoardSide foe) {
      Debug.Assert(!InCheck(), "addCastles() called while InCheck");

      if (canOO(friend, friendRule, foe)) {
#if DebugMoveColor
        PseudoCastles.Add(Move.WTM | friendRule.OO);
#else
        PseudoCastles.Add(friendRule.OO);
#endif
      }

      if (canOOO(friend, friendRule, foe)) {
#if DebugMoveColor
        PseudoCastles.Add(Move.WTM | friendRule.OOO);
#else
        PseudoCastles.Add(friendRule.OOO);
#endif
      }
    }
    #endregion

    #region Pawn Moves
    protected void addPawnCapture(Int32 nFrom, Int32 nTo, Boolean bAbove, Boolean bPromote, Boolean bEnPassant) {
#if DebugSquares
      Debug.Assert(getPiece(nFrom) == vP6, "Piece not a Pawn");
      Debug.Assert((nTo & uSquareMask) == nTo, "To Overflow");
#endif
      var capture = bEnPassant ? Piece.EP : Piece.Capture;
      var pawnCapture = PawnMask | (Move)((UInt32)capture << nCaptiveBit);
      var move = pawnCapture | (Move)((UInt32)nTo << nToBit | ((UInt32)nFrom << nFromBit));
#if DebugMoveColor
      if (WTM()) move |= Move.WTM;
#endif
      if (bPromote)
        foreach (var p in Promotions) {
          var moves = p == Piece.Q ?
            PseudoQueenPromotionCapture : PseudoUnderPromotionCapture;
          moves.Add(move | (Move)((UInt32)p << nPromoteBit));
        }
      else if (bEnPassant) {
#if DebugSquares
        Debug.Assert(getPiece(nTo) == vPieceNull, "Passed Square not Empty");
#endif
        PseudoEPCapture.Add(move);
      }
      else {
        var moves = bAbove ? PseudoPawnAboveCapture : PseudoPawnBelowCapture;
        moves.Add(move);
      }
    }

    protected void addPawnMove(Int32 nFrom, Int32 nTo, Boolean bAbove, Boolean bPromote) {
#if DebugSquares
      Debug.Assert(getPiece(nFrom) == vP6, "Piece not a Pawn");
      Debug.Assert((nTo & uSquareMask) == nTo, "To Overflow");
#endif
      var move = PawnMask | (Move)((UInt32)nTo << nToBit | ((UInt32)nFrom << nFromBit));
#if DebugMoveColor
      if (WTM()) move |= Move.WTM;
#endif
      if (bPromote)
        foreach (var p in Promotions) {
          var moves = p == Piece.Q ? PseudoQueenPromotion : PseudoUnderPromotion;
          moves.Add(move | (Move)((UInt32)p << nPromoteBit));
        }
      else {
        var moves = bAbove ? PseudoPawnAboveMove : PseudoPawnBelowMove;
        moves.Add(move);
      }
    }

    private void addPawnCaptures(BoardSide side, Plane qpTo) {
      var nEP = IsPassed() ? ep(FlagsLo) : nSquares;
      addPawnCaptures2(side, side.PawnA1H8Atx & qpTo, side.Parameter.ShiftA1H8, nEP);
      addPawnCaptures2(side, side.PawnA8H1Atx & qpTo, side.Parameter.ShiftA8H1, nEP);
    }

    private void addPawnCaptures2(BoardSide side, Plane qpAtx, int nDiag, int nEP) {
      var qpFrom = shiftr(qpAtx, nDiag);
      while (qpFrom != 0) {
        var nFrom = RemoveLo(ref qpFrom);
        var nTo = nFrom + nDiag;
        var qpMoveTo = BIT0 << nTo;
        var bAbove = (side.Parameter.Above & qpMoveTo) != 0;
        var bPromote = (side.Parameter.RankLast & qpMoveTo) != 0;
        var bEnPassant = nTo == nEP;
        addPawnCapture(nFrom, nTo, bAbove, bPromote, bEnPassant);
      }
    }

    private void addPawnMoves(BoardSide side, Plane qpTo) {
      var qpPawn = side.Piece & Pawn;

      //
      // Pawn Advances:
      //
      var qpAdvance1 = shiftl(qpPawn, side.Parameter.ShiftRank) & ~RankPiece;
      var qpAdvance2 = shiftl(qpAdvance1 & side.Parameter.RankPass, side.Parameter.ShiftRank) & ~RankPiece;
      var qpAdv1From = shiftr(qpAdvance1 & qpTo, side.Parameter.ShiftRank);
      var qpAdv2From = shiftr(qpAdvance2 & qpTo, 2 * side.Parameter.ShiftRank);
#if TestPawnAdvances
      LogLine("Pawn Advance:\n");
      writeRect(qpAdvance1 | qpAdvance2);
      LogLine();
#endif
      while (qpAdv1From != 0) {
        var nFrom = RemoveLo(ref qpAdv1From);
        var nTo = nFrom + side.Parameter.ShiftRank;
        var qpMoveTo = BIT0 << nTo;
        var bAbove = (side.Parameter.Above & qpMoveTo) != 0;
        var bPromote = (side.Parameter.RankLast & qpMoveTo) != 0;
        addPawnMove(nFrom, nTo, bAbove, bPromote);
      }

      while (qpAdv2From != 0) {
        var nFrom = RemoveLo(ref qpAdv2From);
        var nTo = nFrom + 2 * side.Parameter.ShiftRank;
        addPawnMove(nFrom, nTo, false, false);
      }
    }

    // The following two methods are used by generateMaterialMoves()
    private void addPromotions(BoardSide side, Plane qpTo) {
      var qpPawn = side.Piece & Pawn;
      var qpAdvance1 = shiftl(qpPawn, side.Parameter.ShiftRank) & ~RankPiece;
      var qpAdv1From = shiftr(qpAdvance1 & qpTo & side.Parameter.RankLast, side.Parameter.ShiftRank);

      while (qpAdv1From != 0) {
        var nFrom = RemoveLo(ref qpAdv1From);
        var nTo = nFrom + side.Parameter.ShiftRank;
        addPawnMove(nFrom, nTo, true, true);
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
        var move = PieceCapture | (Move)(nTo << nToBit) | moveFrom;
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
        var move = (Move)(nTo << nToBit) | moveFrom;
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
      var move = KingMask;              // Each side has one King
      var moveFrom = move | (Move)(vKingPos << nFromBit);
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
        qpMoveTo &= ~attacks(foe, qpMoveTo);
        setRayState(vKingPos);
      }
      else
#endif
        qpMoveTo &= ~attacks(foe, qpMoveTo);

      addPieceCaptures(PseudoKingCapture, PseudoKingCapture, moveFrom, qpMoveTo);
      addPieceMoves(PseudoKingMove, PseudoKingMove, moveFrom, qpMoveTo);
    }
#if UnshadowRay2
    private void addKingCaptures(Plane qpTo, Byte vKingPos, Boolean bRayCheck) {
#else
    private void addKingCaptures(Plane qpTo, Byte vKingPos) {
#endif
      var move = KingMask;              // Each side has one King
      var moveFrom = move | (Move)(vKingPos << nFromBit);
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
        qpMoveTo &= ~attacks(foe, qpMoveTo);
        setRayState(vKingPos);
      }
      else
#endif
        qpMoveTo &= ~attacks(foe, qpMoveTo);

      addPieceCaptures(PseudoKingCapture, PseudoKingCapture, moveFrom, qpMoveTo);
    }

    private void addKnightCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      var move = KnightMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpMoveTo = KnightAtx[nFrom] & qpTo;
        addPieceCaptures(PseudoKnightCapture, PseudoKnightCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoKnightMove, PseudoKnightMove, moveFrom, qpMoveTo);
      }
    }

    private void addKnightCaptures(Plane qpTo, Plane qpPiece) {
      var move = KnightMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpMoveTo = KnightAtx[nFrom] & qpTo;
        addPieceCaptures(PseudoKnightCapture, PseudoKnightCapture, moveFrom, qpMoveTo);
      }
    }

    private void addBishopCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      var move = BishopMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpMoveTo = diagAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoDiagAboveMove, PseudoDiagBelowMove, moveFrom, qpMoveTo);
      }
    }

    private void addBishopCaptures(Plane qpTo, Plane qpPiece) {
      var move = BishopMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpMoveTo = diagAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpMoveTo);
      }
    }

    private void addRookCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      var move = RookMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpMoveTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpMoveTo);
        addPieceMoves(PseudoRectAboveMove, PseudoRectBelowMove, moveFrom, qpMoveTo);
      }
    }

    private void addRookCaptures(Plane qpTo, Plane qpPiece) {
      var move = RookMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpMoveTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpMoveTo);
      }
    }

    private void addQueenCapturesAndMoves(Plane qpTo, Plane qpPiece) {
      var move = QueenMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
        var qpDiagTo = diagAtx(nFrom) & qpTo;
        var qpRectTo = rectAtx(nFrom) & qpTo;
        addPieceCaptures(PseudoDiagAboveCapture, PseudoDiagBelowCapture, moveFrom, qpDiagTo);
        addPieceCaptures(PseudoRectAboveCapture, PseudoRectBelowCapture, moveFrom, qpRectTo);
        addPieceMoves(PseudoDiagAboveMove, PseudoDiagBelowMove, moveFrom, qpDiagTo);
        addPieceMoves(PseudoRectAboveMove, PseudoRectBelowMove, moveFrom, qpRectTo);
      }
    }

    private void addQueenCaptures(Plane qpTo, Plane qpPiece) {
      var move = QueenMask;
      while (qpPiece != 0) {
        var nFrom = RemoveLo(ref qpPiece);
        var moveFrom = move | (Move)(nFrom << nFromBit);
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
