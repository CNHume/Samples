//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-02-21 CNHume]Split Hashcode processing into its own file
//
// Conditionals:
//
//#define DisplayHash
//#define CryptoServiceProvider           // Performance will vary owing to non-constant seed
//#define RNGStatistics
//#define TestZobrist

namespace Engine {
  using static Logging.Logger;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using static System.Math;
  using System.Runtime.CompilerServices;// for MethodImplAttribute
#if CryptoServiceProvider
  using System.Security.Cryptography;
#endif
  //
  // Type Aliases:
  //
  using Hashcode = UInt64;
  using Plane = UInt64;

  partial class Board {
    #region Plane Hash Methods
    protected static Hashcode nextZobrist() {
#if CryptoServiceProvider
      ZobristRNG.GetBytes(ZobristBuffer);
#else
      ZobristRandom.NextBytes(ZobristBuffer);
#endif
      var qHash = BitConverter.ToUInt64(ZobristBuffer, 0);
#if TestZobrist
      Zobrists.Add(qHash);
#endif
      return qHash;
    }

    protected static void newZobrist() {
#if TestZobrist
      Zobrists = new List<Hashcode>();
#endif
      ZobristDraw = new Hashcode[2];
      ZobristFile = new Hashcode[8];
      ZobristRights = new Hashcode[16];

      ZobristWhite = new Hashcode[nPieces][];
      ZobristBlack = new Hashcode[nPieces][];
      for (var nPiece = 0; nPiece < nPieces; nPiece++) {
        ZobristWhite[nPiece] = new Hashcode[nSquares];
        ZobristBlack[nPiece] = new Hashcode[nSquares];
      }

      ZobristExcludedFrom = new Hashcode[nSquares];
      ZobristExcludedTo = new Hashcode[nSquares];

      var nUnderPromotions = Promotions.Length - 1;
      if (nUnderPromotions > 0) {
        //Debug.Assert(Promotions[0] == Piece.Q, $"Promotion[0] should be {Piece.Q}");
        ZobristExcludedPromotion = new Hashcode[nUnderPromotions];
      }

      ZobristBuffer = new Byte[8];
#if CryptoServiceProvider
      ZobristRNG = new RNGCryptoServiceProvider();
#else
      ZobristRandom = new Random(0);    // Fixed, repeatable seed
#endif
    }

    protected static void loadZobrist() {
      ZobristTurn = nextZobrist();

      // For En Passant File:
      for (var n = 0; n < ZobristFile.Length; n++)
        ZobristFile[n] = nextZobrist();

      // For Castling Abilities:
      for (var n = 0; n < ZobristRights.Length; n++)
        ZobristRights[n] = nextZobrist();

      // For Pieces that can be held by each Square:
      for (var nPiece = 0; nPiece < nPieces; nPiece++) {
        var bPawn = nPiece == vP6;      //[Speed]Pawns cannot appear on their First or Last Rank:
        var nMin = (Int32)(bPawn ? sq.a2 : sq.a1);
        var nMax = (Int32)(bPawn ? sq.h7 : sq.h8);

        for (var n = nMin; n <= nMax; n++) {
          // White Piece:
          ZobristWhite[nPiece][n] = nextZobrist();
          // Black Piece:
          ZobristBlack[nPiece][n] = nextZobrist();
        }
      }

      // Distinguish actual Draws as well as Draw2:
      for (var n = 0; n < ZobristDraw.Length; n++)
        ZobristDraw[n] = nextZobrist();

      // For Excluded Moves
      for (var n = 0; n < nSquares; n++) {
        ZobristExcludedFrom[n] = nextZobrist();
        ZobristExcludedTo[n] = nextZobrist();
      }

      for (var n = 0; n < ZobristExcludedPromotion.Length; n++)
        ZobristExcludedPromotion[n] = nextZobrist();

      ZobristExcludedCastles = nextZobrist();
#if RNGStatistics
      var nCount = Zobrists.Count;
      var decSum = 0M;
      LogLine($"\nHashcodes = {Zobrists.Count}");
      foreach (var qHash in Zobrists) {
        decSum += qHash;
#if DumpZobrist
        LogLine(formatHash(qHash));
#endif
      }

      var dMean = (Double)decSum / nCount;
      var dRange = Pow(2, 64);
      var dMeanIdeal = dRange / 2;
      var dMeanError = dMean / dMeanIdeal - 1;
      LogLine($"Mean = {dMean:e} {100 * dMeanError:n2}%");

      if (nCount > 1) {
        var dSquareSum = 0.0;
        foreach (var qHash in Zobrists) {
          var dDelta = qHash - dMean;
          dSquareSum += dDelta * dDelta;
        }

        var dVariance = dSquareSum / (nCount - 1);
        var dDeviation = Sqrt(dVariance);
        var dDeviationIdeal = dRange / Sqrt(12.0);
        var dDeviationError = dDeviation / dDeviationIdeal - 1;
        LogLine($"s.d. = {dDeviation:e} {100 * dDeviationError:n2}%");
      }
#endif
#if TestZobrist
      Zobrists.Sort();
      var qLastCode = default(Hashcode);
      foreach (var qHash in Zobrists) {
        if (qHash == qLastCode) {
          Trace.Assert(qHash != qLastCode, "Duplicate Hashcode Found");
          break;
        }

        qLastCode = qHash;
      }
#endif
    }

    public Hashcode DynamicHash(Move moveExcluded = Move.Undefined) {
      //
      // Modify Hash when a Draw is detected because an equal/contempt() evaluation
      // will be different from the usual evaluation based on material balance, etc.
      //
      var qDynamic = Hash;

      //
      // Distinguish evaluations where the "best move" is excluded
      //
      if (isDefined(moveExcluded)) {
#if DebugMove
        unpack2(moveExcluded, out Int32 nFrom, out Int32 nTo,
                out UInt32 uPiece, out UInt32 uPromotion,
                out Boolean bCastles, out Boolean bCapture);
        var piece = (Piece)uPiece;
        var sqFrom = (sq)nFrom;
        var sqTo = (sq)nTo;
#else
        unpackShort(moveExcluded, out Int32 nFrom, out Int32 nTo,
                    out UInt32 uPromotion, out Boolean bCastles);
#endif
        qDynamic ^= ZobristExcludedFrom[nFrom];
        qDynamic ^= ZobristExcludedTo[nTo];

        if (uPromotion > 0) {
          var promotion = (Piece)uPromotion;
          var nIndex = Array.FindIndex(Promotions, p => p == promotion);
          if (nIndex > 0)               // Distinguish Excluded Promotions
            qDynamic ^= ZobristExcludedPromotion[nIndex - 1];
        }

        if (bCastles) {
          //
          //[Chess 960]Avoid potential ambiguity of ordinary King moves with castling
          //
#if HashExcludedChess960CastlesOnly
          if (State.IsChess960)
            qDynamic ^= ZobristExcludedCastles;
#else
          //
          // It seems simplest to Hash excluded orthodox castling moves
          // as well as Chess960 castling moves
          //
          qDynamic ^= ZobristExcludedCastles;
#endif
        }
      }

      if (IsDraw())
        qDynamic ^= ZobristDraw[0];
      else if (IsDraw2())
        qDynamic ^= ZobristDraw[1];

      return qDynamic;
    }

    protected Hashcode hashFlags(Boolean bWTM) {
      Hashcode qHash = 0;
      var nRights = 0;
      foreach (var side in Side) {
        nRights <<= nPerTwoBits;
        var fside = side.FlagsSide & SideFlags.CanCastleMask;
        nRights += (Int32)fside;
      }

      qHash ^= ZobristRights[nRights];
      if (IsPassed()) qHash ^= epHash();
      if (!bWTM) qHash ^= ZobristTurn;
      return qHash;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void hashPiece(BoardSide side, Byte vPiece, Int32 n) {
      if (nPieces <= vPiece) {
        Debug.Assert(vPiece < nPieces, "hashPiece(nPieces <= vPiece)");
      }

      if (n < 0) {
        Debug.Assert(n >= 0, "hashPiece(n < 0)");
      }
      else if (nSquares <= n) {
        Debug.Assert(n < nSquares, "hashPiece(nSquares <= n)");
      }

      var zobrist = side.Parameter.Zobrist;
      var qHash = zobrist[vPiece][n];
      if (vPiece == vP6) HashPawn ^= qHash;
      Hash ^= qHash;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Hashcode epHash() {
      return ZobristFile[(Int32)(FlagsLo & LoFlags.EPFile)];
    }

    private Hashcode hashPiece2(BoardSide side, Plane qpPiece, Byte vPiece) {
      var zobrist = side.Parameter.Zobrist;
      Hashcode qHash = 0;
      while (qpPiece != 0) {
        var n = RemoveLo(ref qpPiece);
        qHash ^= zobrist[vPiece][n];
      }
      return qHash;
    }

    private Hashcode hashPiece1(Plane qpPiece, Byte vPiece) {
      Hashcode qHash = 0;
      foreach (var side in Side)
        qHash ^= hashPiece2(side, qpPiece & side.Piece, vPiece);
      return qHash;
    }

    protected Hashcode hashPawn() {
      return hashPiece1(Pawn, vP6);
    }

    protected Hashcode hashPieces() {
      Hashcode qHash = 0;
      qHash ^= hashPiece1(Rook, vR6);
      qHash ^= hashPiece1(Knight, vN6);
      qHash ^= hashPiece1(Bishop, vB6);
      qHash ^= hashPiece1(Queen, vQ6);
      qHash ^= hashPiece1(King, vK6);
      qHash ^= hashFlags(WTM());
      return qHash;
    }

    [Conditional("TestHash")]
    protected void testHash() {
      var qHashPawn = hashPawn();
      var qHash = qHashPawn ^ hashPieces();
#if DisplayHash
      LogLine($" Hash = {formatHash(Hash)}");

      if (qHash != Hash)
        LogLine($"IHash = {formatHash(Hash)}");
#else
      if (qHashPawn != HashPawn) {
        //DisplayCurrent("testHash()");
        Trace.Assert(qHashPawn == HashPawn, "Full HashPawn differs from Incremental HashPawn");
      }

      if (qHash != Hash) {
        //DisplayCurrent("testHash()");
        Trace.Assert(qHash == Hash, "Full Hash differs from Incremental Hash");
      }
#endif                                  // DisplayHash
    }
    #endregion
  }
}
