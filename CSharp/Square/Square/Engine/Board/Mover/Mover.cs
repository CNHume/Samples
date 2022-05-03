//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-09-16 CNHume]Split Mover into the Board base class
//
// Conditionals:
//
#define VerifyGamePlyColor
//#define VerifyPieceColor
//#define VerifyPromotion
#define RecursiveNullMade
#define TracePosition
//#define CountCapturedPiece
#define SaveCapture

namespace Engine {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute

  using static CastleRule;
  using static Position;

  //
  // Type Aliases:
  //
  using Plane = UInt64;
  using Ply = UInt16;

  partial class Board {
    #region Constants
    public const Byte HalfMoveClockMax = 100;   // 50-move (100-ply) rule
    #endregion

    #region Side Methods
    protected CastleRuleParameter getRule(Boolean bWTM) {
      var rule = State.Rule.RuleParameter;
      return bWTM ?
        rule[White] :
        rule[Black];
    }

    protected (CastleRuleParameter friendRule, CastleRuleParameter foeRule) getRules(Boolean bWTM) {
      var rule = State.Rule.RuleParameter;
      return bWTM ?
        (rule[White], rule[Black]) :
        (rule[Black], rule[White]);
    }

    protected BoardSide getSide(Boolean bWTM) {
      return bWTM ?
        Side[White] :
        Side[Black];
    }

    protected (BoardSide friend, BoardSide foe) getSides(Boolean bWTM) {
      return bWTM ?
        (Side[White], Side[Black]) :
        (Side[Black], Side[White]);
    }

    protected Byte getKingPos(Boolean bWTM) {
      var side = getSide(bWTM);
      return side.KingPos.Value;
    }
    #endregion

    #region Piece Mover
    //
    //[Chess 960]Certain castling configurations require that both the King and the Rook
    // be removed before either is added back in their new positions.  Orthodox Castling
    // does not require this; but one or even both of the From squares may coincide with
    // the castling partner To square in Chess 960.
    //
    private void rookCastles(BoardSide side, CastleRuleParameter rule, Int32 nTo) {
      if (nTo == rule.KingOOTo) {
        raisePiece(side, rule, vR6, rule.RookOOFrom.Value);
        lowerPiece(side, vR6, rule.RookOOTo);
      }
      else if (nTo == rule.KingOOOTo) {
        raisePiece(side, rule, vR6, rule.RookOOOFrom.Value);
        lowerPiece(side, vR6, rule.RookOOOTo);
      }
    }

    //
    // Lazy Capture avoids calling getPieceIndex() until it becomes necessary.
    // The purpose of the following method is to update the move and to avoid
    // calling getPieceIndex() again.
    //
    // SaveCapture is required to show captures in AppendAN() if bExpandFrom.
    //
    protected Byte captureIndex(Int32 nTo, ref Move move, out Boolean bEnPassant) {
      bEnPassant = false;
      var vCapture = vPieceNull;
      var uCapture = captured(move);
      var capture = (Piece)uCapture;

      if (capture == Piece.Capture) {
#if CountCapturedPiece
        GameState.AtomicIncrement(ref State.CapturedPieceTotal);
#endif
        //
        // Between 2% and 20% of all Pseudo Moves require a call to getPieceIndex().
        // At the rate of 72 MHz when a Pawn is found; 68 MHz otherwise, the 1.5%
        // cost would be quite low even were it incurred for every generated move.
        //
        vCapture = getPieceIndex(nTo);

        Debug.Assert(vCapture != vPieceNull, "vCapture == vPieceNull",
                     $"There is no piece to capture on {(sq)nTo}.");
#if SaveCapture
        var captive = indexPiece(vCapture);
        move &= ~Move.CaptiveMask;
        move |= (Move)((UInt32)captive << nCaptiveBit);
#endif
      }
      else if (capture == Piece.EP) {
        bEnPassant = true;
        vCapture = vP6;
      }
      else if (capture == Piece.None) {
        Debug.Assert(capture != Piece.None, "Unexpected Non-Capture");
      }
      else
        vCapture = pieceIndex(uCapture);

      Debug.Assert(vCapture != vK6, "Unknown Captive",
                   $"No captive found for {(sq)nTo}.");

      return vCapture;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void resetEP() {
      if (IsPassed()) {
        //
        // Adjust the incremental Hash and reset EPFlags, except for WTM:
        //
        Hash ^= epHash();

        //[Note]Preserve WTM
        FlagsLo &= ~(LoFlags.Passed | LoFlags.EPFile);
      }
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void resetPawnAtx(BoardSide side) {
      var qpPawn = side.Piece & Pawn;
      side.PawnA1H8Atx = shiftl(qpPawn & ~side.Parameter.FileRight, side.Parameter.ShiftA1H8);
      side.PawnA8H1Atx = shiftl(qpPawn & ~side.Parameter.FileLeft, side.Parameter.ShiftA8H1);
    }

    protected void buildPawnAtx() {     // Reset Pawn[A1H8|A8H1]Atx
      foreach (var side in Side)
        resetPawnAtx(side);
    }

    protected Plane passed(BoardSide side, Int32 nPassed) {
      var qpFriend = BIT0 << nPassed;

      var qpFrom =
        shiftr(qpFriend & side.PawnA1H8Atx, side.Parameter.ShiftA1H8) |
        shiftr(qpFriend & side.PawnA8H1Atx, side.Parameter.ShiftA8H1);

      return qpFrom;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void setEPFile(Int32 nEP) {
      // Any Square on the EP File will do
      FlagsLo |= (LoFlags)((LoFlags)nEP & LoFlags.EPFile | LoFlags.Passed);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 ep(LoFlags flo) {
      var x = (Int32)(flo & LoFlags.EPFile);
      var bWTM = (flo & LoFlags.WTM) != 0;
      //[Note]EPFile identifies a Black pawn when it is White to move, and vice versa
      return (Int32)(bWTM ? sq.a6 : sq.a3) + x;
    }

    //
    // Set EP flags only if an EP would be legal
    //
    private void tryEP(
      BoardSide friend, CastleRuleParameter friendRule,
      BoardSide foe, CastleRuleParameter foeRule,
      Int32 nTo, Int32 nPassedTo) {
      var vKing = friend.KingPos.Value;
      var qpPassedFrom = passed(friend, nPassedTo);
      while (qpPassedFrom != 0) {
        var nFrom = RemoveLo(ref qpPassedFrom);

        //
        // Test for legality, were Friend to play EP:
        //
        // 1) Remove the Friend Pawn from its nFrom post;
        // 2) And place it on the nPassedTo square.
        // 3) Remove the Foe Pawn from the nTo square to which it moved.
        // 4) Note whether the resulting position would be legal.
        // 5) Restore the Foe Pawn to its nTo square.
        // 6) Remove the Friend Pawn placed on nPassedTo;
        // 7) And restore the Pawn to its nFrom post.
        // 8) If EP Legal break to setEPFile(nPassedTo).
        //
        raisePiece(friend, friendRule, vP6, nFrom);
        lowerPiece(friend, vP6, nPassedTo);
        raisePiece(foe, foeRule, vP6, nTo);     //[Speed]Remove Not Needed: Material balance restored below
                                                //[Note]buildPawnAtx() is not needed for this pin determination
        var bLegal =
          (foe.Piece & DiagPiece & diagAtx(vKing)) == 0 &&
          (foe.Piece & RectPiece & rectAtx(vKing)) == 0;

        lowerPiece(foe, vP6, nTo);              //[Speed]placePiece Not Needed
        raisePiece(friend, friendRule, vP6, nPassedTo);
        lowerPiece(friend, vP6, nFrom);

        if (bLegal) {
          setEPFile(nPassedTo);
          break;
        }
      }
    }

    // Capture: ~6.3 MHz, Simple: ~10.5 MHz, Pawn: ~9.5 MHz
    protected void movePiece(
      BoardSide friend, CastleRuleParameter friendRule,
      BoardSide foe, CastleRuleParameter foeRule,
      ref Move move) {
      unpack2(move, out Int32 nFrom, out Int32 nTo,
              out UInt32 uPiece, out UInt32 uPromotion,
              out Boolean bCastles, out Boolean bCapture);
      var vPiece = pieceIndex(uPiece);
      var bSupplied = uPromotion > 0;
#if VerifyPromotion                     //[PACN]
      var qpMoveTo = BIT0 << nTo;
      var bPromote = (friend.Parameter.RankLast & qpMoveTo) != 0;
      var bRequired = vPiece == vP6 && bPromote;
      Trace.Assert(bRequired == bSupplied, "Invalid Promotion");
#endif
      if (bSupplied)
        removePiece(friend, friendRule, vPiece, nFrom);
      else
        raisePiece(friend, friendRule, vPiece, nFrom);

      if (bCapture) {
        HalfMoveClock = 0;              // HalfMoveClock Reset due to Capture
        var vCapture = captureIndex(nTo, ref move, out Boolean bEnPassant);
        var nCaptureFrom = bEnPassant ? nTo + foe.Parameter.ShiftRank : nTo;
        removePiece(foe, foeRule, vCapture, nCaptureFrom);

        if (vCapture == vP6)
          resetPawnAtx(foe);
      }
      else if (bCastles)
        rookCastles(friend, friendRule, nTo);

      if (bSupplied)
        placePiece(friend, pieceIndex(uPromotion), nTo);
      else
        lowerPiece(friend, vPiece, nTo);

      if (vPiece == vP6) {
        if (nTo - nFrom == 2 * friend.Parameter.ShiftRank) {
          var nPassedTo = nTo - friend.Parameter.ShiftRank;
          tryEP(foe, foeRule, friend, friendRule, nTo, nPassedTo);
        }

        HalfMoveClock = 0;              // HalfMoveClock Reset due to Pawn Move
        resetPawnAtx(friend);
      }
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal void toggleWTM() {
      GamePly++;                        // GamePly should be even iff WTM
      Hash ^= ZobristTurn;
      FlagsLo ^= LoFlags.WTM;
#if VerifyGamePlyColor
      Trace.Assert(ColorParity(GamePly), "Incorrect GamePly Color (WTM != Even Ply)");
#endif
    }

    protected Boolean ColorParity(Ply wPly) {
      return WTM() == IsEven(wPly);
    }

    public static Boolean IsEven(UInt32 u) {
      return (u & 1) == 0;
    }

    public static Boolean IsOdd(UInt32 u) {
      return (u & 1) != 0;
    }

    //
    //[Note]Changes made here should be validated by running the perft tests
    //
    protected void move(ref Move move) {
      clrFence();

      if (HalfMoveClock < HalfMoveClockMax)   // Avoid Overflow
        HalfMoveClock++;

      // Record Castling Abilities prior to removePiece()
      var bWTM = WTM();
      (BoardSide friend, BoardSide foe) = getSides(bWTM);
      (CastleRuleParameter friendRule, CastleRuleParameter foeRule) = getRules(bWTM);

      var fhiCanCastleOld = friend.FlagsHi & HiFlags.CanCastleMask;

      movePiece(friend, friendRule, foe, foeRule, ref move);
      verifyPieceColors();              // Conditional

      toggleWTM();

      //
      // This En Passant State is not reset until addPseudoMoves()
      // has used it to add En Passant captures for the next ply:
      //
      if (IsPassed()) Hash ^= epHash();

      // Update Castling Abilities, if they changed
      var fhiCanCastleNew = friend.FlagsHi & HiFlags.CanCastleMask;
      if (fhiCanCastleNew != fhiCanCastleOld) {
        Hash ^= ZobristRights[(Int32)fhiCanCastleOld] ^
                ZobristRights[(Int32)fhiCanCastleNew];

        //
        // A new Transposition Group begins when Castling Rights change:
        //
        setFence();
      }
      else if (HalfMoveClock == 0) {
        //
        // A new Transposition Group begins when the 100-Ply Rule Clock is reset:
        //
        setFence();
      }
#if RecursiveNullMade
      //
      // The NullMade Flag is set when a Null Move is performed.  Subsequent Null Moves
      // are disallowed until the NullMade flag is cleared here: when this method makes
      // an actual move.
      //
      clrNullMade();
#endif
      tracePosition();                  // Conditional
    }

    protected void skipTurn() {
      clrFence();

      //
      //[Note]Null Moves are neutral wrt the 50 move rule:
      // HalfMoveClock is neither advanced nor reset.
      //
      toggleWTM();

      NullPly++;
      setNullMade();                    // Prevent two consecutive Null Moves

      tracePosition();                  // Conditional
    }
    #endregion

    #region Trace Positions
    // Called by move() and skipTurn()
    [Conditional("TracePosition")]
    protected void tracePosition() {
      clrTrace();

      //
      // From Johannessen vs Fischer #8
      // Child: Hash = 1C67654C9374A387; FEN = 4r1k1/p6p/5Qp1/2p5/P7/1b4PP/2r5/3K4 w - - 0 40
      //
      // From Caruana v Gustafsson 2012-07-17 #8
      // Child: Hashcode = FA627B7081C68EB4; FEN = 5rk1/5p1p/5Q2/1p3p2/8/2P4P/5P2/r2B1RK1 w - - 0 40
      //
      //setTrace(
      //  0xE12484DB802B6B2E,             // 1b6/1P6/2B5/8/7K/6Q1/4kq2/8 b - - 0 73
      //  0xD1CCD6D6AE6491E3);            // 1b6/1P6/2B5/8/7K/6q1/4k3/8 w - - 0 74
      //setTrace(
      //  0x1633CEFA9B3CDBCE,             // 2nq1nk1/5p1p/4p1pQ/pb1pP1NP/1p1PB1P1/1P4N1/P4P2/6K1 b - - 0 28
      //  0xFAC050CDA4A6DAA1);            // q6k/5b2/4p1p1/Q2pP3/1p1P2P1/1P6/P4P2/1B4K1 w - - 0 39
      //setTrace(0x2C5D9DE284682178);     // 4b1k1/8/4N1p1/pn1pP3/1p1P2P1/1P6/P4P2/1B4K1 w - - 0 38
      //setTrace(0x73C2AAA6889090C7);     // 4b1k1/8/4N1B1/pn1pP3/1p1P2P1/1P6/P4P2/6K1 b - - 0 38
      //setTrace(0x647465517DAB50F0);     // 8/3b2k1/3P2p1/p2p2P1/1p1P4/1P1B4/P4P2/6K1 b - - 0 40

      //setTrace(
      //  0xD1C45B256E94DC7D,             // 4b3/6k1/4p1p1/Q2pP3/1p1P2P1/1P3B2/P2q1P2/7K b - - 0 42
      //  0x1558E069FE4968DB,             // 4b3/6k1/4p1p1/Q2pP3/1p1P2P1/1P3B2/P4q2/7K w - - 0 43
      //  0xA3BBD95F394E880F);            // 4b3/6k1/4p1p1/3QP3/1p1P2P1/1P3B2/P4q2/7K b - - 0 43

      //setTrace(0x0F27859676101D10);     // rnb4r/pppp4/3Q3k/6p1/5B2/8/PPP3PP/RN5K b - - 0 21
      //setTrace(0xAE69BF6473F0C944);     // 4r2k/p3B2p/6p1/P1p5/7Q/1bb3PP/2r5/3K4 w - - 1 38
      //setTrace(0x98D97E33691E2FA3);     // 4r2k/p3B2p/6p1/2p5/P1r1Q3/1b4PP/8/b2K4 w - - 2 38
      //setTrace(0xB85FF60DF1132653);     // 8/k7/8/8/8/6b1/7p/7K w - - 0 9
      //setTrace(0xC430DFDDEBD9EE85);     // 8/8/k7/8/8/6b1/6Kp/8 w - - 0 10

      //
      // From Mavo Nice Mate1 #6
      // Parent: Hash = 0EA3665E88DC15B5; FEN = 1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PPK/8 w - - 0 4
      // Child1 after Ra6: Hash = F128B7F4E8794435; FEN = 1k5r/1r2q1pp/RPQ5/4p3/4B3/7P/5PPK/8 b - - 1 4
      // Child2 after Ra5: Hash = C8AAC46E31D79AF3; FEN = 1k5r/1r2q1pp/1PQ5/R3p3/4B3/7P/5PPK/8 b - - 1 4
      //
      //setTrace(
      //  //0x0EA3665E88DC15B5,             // Parent: 1k5r/1r2q1pp/1PQ5/4p3/R3B3/7P/5PPK/8 w - - 0 4
      //  0xF128B7F4E8794435,             // Child1 after Ra6: 1k5r/1r2q1pp/RPQ5/4p3/4B3/7P/5PPK/8 b - - 1 4
      //  0xC8AAC46E31D79AF3);            // Child2 after Ra5: 1k5r/1r2q1pp/1PQ5/R3p3/4B3/7P/5PPK/8 b - - 1 4

      //setTrace(
      //  0xB21BDAEEC22C9382,             // 2k5/8/K7/2q5/8/8/8/8 w - - 0 4
      //  0x976F4F844E694E44);            // 2k5/8/K7/1R6/8/8/8/2r5 w - - 0 3

      setTrace(
        0x62FE82784CA9DA62);            // 8/6Q1/8/7p/P3K2k/8/6P1/8 b - - 0 57
    }
    #endregion
  }
}
