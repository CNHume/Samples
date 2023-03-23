//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2012-09-16 CNHume]Split Mover into the Board base class
//
// Conditionals:
//
//#define CountCapturedPiece
#define RecursiveNullMade
#define SaveCapture
//#define TracePosition
//#define VerifyPieceColor
//#define VerifyPromotion

using System.Diagnostics;
using System.Runtime.CompilerServices;  // For MethodImplAttribute

namespace Engine {
  partial class Board {
    #region Constants
    private const Byte vHalfMoveClockMax = 100; // 50-move (100-ply) rule
    #endregion

    #region Methods
    //
    // Move Methods:
    //
    // MovePiece and its BoardSide methods:
    // LowerPiece which is called by PlacePiece
    // RaisePiece which is called by RemovePiece
    //
    #region Piece Mover
    //
    // Lazy Capture avoids calling GetPieceIndex() until it becomes necessary.
    // The purpose of the following method is to update the move and to avoid
    // calling GetPieceIndex() again.
    //
    // SaveCapture is required to show captures in AppendAN() if bExpandFrom.
    //
    protected Byte CaptureIndex(Int32 nTo, ref Move move, out Boolean bEnPassant) {
      bEnPassant = false;
      var vCapture = vPieceNull;
      var uCapture = Captured(move);
      var capture = (Piece)uCapture;

      if (capture == Piece.Capture) {
#if CountCapturedPiece
        GameState.AtomicIncrement(ref State.CapturedPieceTotal);
#endif
        //
        // Between 2% and 20% of all Pseudo Moves require a call to GetPieceIndex().
        // At the rate of 477.8 MHz [2023-02-19] the 1.5% cost would be quite low
        // even were it incurred for every generated move.
        //
        vCapture = GetPieceIndex(nTo);

        Debug.Assert(vCapture != vPieceNull, "vCapture == vPieceNull",
                     $"There is no piece to capture on {(Sq)nTo}.");
#if SaveCapture
        var captive = IndexPiece(vCapture);
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
        vCapture = PieceIndex(uCapture);

      Debug.Assert(vCapture != vK6, "Unknown Captive",
                   $"No captive found for {(Sq)nTo}.");

      return vCapture;
    }

    private void tryEP(Byte vEPTarget) {
      //
      // Set EPTarget whether or not En Passant is Legal, conforming
      // to the X-FEN Definition for Encoding En Passant.
      //
      // See https://en.wikipedia.org/wiki/X-FEN#Encoding_en-passant
      //
      EPTarget = vEPTarget;

      var nMovedTo = vEPTarget + Foe.Parameter.PawnStep;
      var vKing = Friend.GetKingPos();
      var qpCaptureFrom = Friend.EPGuard(vEPTarget);
      while (qpCaptureFrom != 0) {
        var nCaptureFrom = RemoveLo(ref qpCaptureFrom);

        //
        // Test whether pins prevent any En Passant capture at the EPTarget square:
        //
        // 1) Remove the Friend Pawn from its nCaptureFrom square;
        // 2) And place it on the EPTarget square.
        // 3) Remove the Foe Pawn from its nMovedTo square.
        // 4) Note whether the resulting position would be legal.
        // 5) Restore the Foe Pawn to its nMovedTo square.
        // 6) Remove the Friend Pawn placed on EPTarget;
        // 7) And restore the Pawn to its nCaptureFrom square.
        // 8) If EP was Legal setEPLegal() and break.
        //
        Friend.RaisePiece(vP6, nCaptureFrom);
        Friend.LowerPiece(vP6, vEPTarget);
        //[Speed]RemovePiece Not Needed, because material balance is restored below.
        Foe.RaisePiece(vP6, nMovedTo);

        //[Note]buildPawnAtx() is not needed to find Ray Checks
        var bLegal =
          (Foe.Piece & DiagPiece & RayDiag(vKing)) == 0 &&
          (Foe.Piece & OrthPiece & RayOrth(vKing)) == 0;

        //[Speed]PlacePiece Not Needed, because RemovePiece was not performed.
        Foe.LowerPiece(vP6, nMovedTo);
        Friend.RaisePiece(vP6, vEPTarget);
        Friend.LowerPiece(vP6, nCaptureFrom);

        if (bLegal) {
          setEPLegal();
          break;
        }
      }
    }

    //[2023-01-31]Capture: 21.6 MHz, Simple: 29.2 MHz, Pawn: 26.8 MHz, Passer: 27.7 MHz
    protected Byte? MovePiece(ref Move move) {
      Byte? vEPTarget = default;
      unpack2(move, out Int32 nFrom, out Int32 nTo,
              out UInt32 uPiece, out UInt32 uPromotion,
              out Boolean bCastles, out Boolean bCapture);
      var vPiece = PieceIndex(uPiece);
      var bSupplied = uPromotion > 0;
#if VerifyPromotion                     //[PACN]
      var bRequired = vPiece == vP6 && Friend.Parameter.IsPromotion(nTo);
      Trace.Assert(bRequired == bSupplied, "Invalid Promotion");
#endif
      if (bSupplied)
        Friend.RemovePiece(vPiece, nFrom);
      else
        Friend.RaisePiece(vPiece, nFrom);

      if (bCapture) {
        HalfMoveClock = 0;              // HalfMoveClock Reset due to Capture
        var vCapture = CaptureIndex(nTo, ref move, out Boolean bEnPassant);
        var nCaptureFrom = bEnPassant ? nTo + Foe.Parameter.PawnStep : nTo;
        Foe.RemovePiece(vCapture, nCaptureFrom);

        if (vCapture == vP6)
          Foe.ResetPawnAtx();
      }
      else if (bCastles)
        Friend.RookCastles(nTo);

      if (bSupplied)
        Friend.PlacePiece(PieceIndex(uPromotion), nTo);
      else
        Friend.LowerPiece(vPiece, nTo);

      if (vPiece == vP6) {
        if (nTo - nFrom == 2 * Friend.Parameter.PawnStep)
          vEPTarget = (Byte)(nTo - Friend.Parameter.PawnStep);

        HalfMoveClock = 0;              // HalfMoveClock Reset due to Pawn Move
        Friend.ResetPawnAtx();
      }

      verifyPieceColors();              //[Conditional]
      return vEPTarget;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void toggleWTM() {
      GamePly++;                        // GamePly should be even iff WTM
      Hash ^= zobristTurn;
      //[Note]Friend and Foe must always correspond to the value of WTM()
      (Friend, Foe) = GetSides(WTM());
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void updateRepetitionCycle() {
      //
      // A new Repetition Cycle begins whenever the 100-Ply Rule Clock is reset:
      //
      if (HalfMoveClock == 0)
        SetDraw0();
    }

    //
    //[Test]Validate any change made here by running Perft Tests!
    //
    protected void PlayMove(ref Move move) {
      clrDraw0();

      // Avoid Overflow
      if (HalfMoveClock < vHalfMoveClockMax)
        HalfMoveClock++;

      var vEPTarget = MovePiece(ref move);

      updateRepetitionCycle();

      //[Note]toggleWTM() inverts the sense of Friend and Foe.
      toggleWTM();

      #region Update En Passant
      // tryEP() will be assessed from the perspective
      // of Foe having become Friend after toggleWTM()
      if (vEPTarget.HasValue)
        tryEP(vEPTarget.Value);

      toggleEPHash();
      #endregion                        // Update En Passant
#if RecursiveNullMade
      //
      // The NullMade Flag is set when a Null Move is performed.  Subsequent Null Moves
      // are disallowed until the NullMade flag is cleared here: when this method makes
      // an actual move.
      //
      clrNullMade();
#endif
      tracePosition();                  //[Conditional]
    }

    protected void SkipTurn() {
      clrDraw0();

      //
      //[Note]Null Moves are neutral wrt the 50 move rule:
      // HalfMoveClock is neither advanced nor reset.
      //
      toggleWTM();

      setNullMade();                    // Prevent two consecutive Null Moves

      tracePosition();                  //[Conditional]
    }
    #endregion                          // Piece Mover

    #region Trace Positions
    // Called by PlayMove() and SkipTurn()
    [Conditional("TracePosition")]
    private void tracePosition() {
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

      //setTrace(
      //  0x62FE82784CA9DA62);            // 8/6Q1/8/7p/P3K2k/8/6P1/8 b - - 0 57

      setTrace(                         // 8/8/8/8/6b1/6k1/3b4/5K2 w - - 0 9
        0xF647B837F53828CE);
    }
    #endregion                          // Trace Positions
    #endregion                          // Methods
  }
}
