//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2011-02-21 CNHume]Split Hashcode processing into its own file
//
// Conditionals:
//
//#define DisplayHash
#define HashCastlingRights
//#define RNGStatistics
//#define TestZobrist

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;  // For MethodImplAttribute0

using static System.Math;

namespace Engine;

using Exceptions;

using static Logging.Logger;

//
// Type Aliases:
//
using Hashcode = UInt64;
using Plane = UInt64;

partial class Board {
  #region Hash Methods
  private static Hashcode nextZobrist() {
    zobristRandom.NextBytes(zobristBuffer);
    var qHash = BitConverter.ToUInt64(zobristBuffer, 0);
#if TestZobrist
    zobrists.Add(qHash);
#endif
    return qHash;
  }

  [MemberNotNull(
    nameof(zobristRandom),
    nameof(zobristBuffer),
    nameof(zobristDraw),
    nameof(zobristFile),
    nameof(zobristRightsBlack),
    nameof(zobristRightsWhite),
    nameof(zobristBlack),
    nameof(zobristWhite),
    nameof(zobristExcludedFrom),
    nameof(zobristExcludedTo),
    nameof(zobristExcludedPromotion)
    )]
  private static void newZobrist() {
#if TestZobrist
    zobrists = new List<Hashcode>();
#endif
    zobristWhite = new Hashcode[nPieces][];
    zobristBlack = new Hashcode[nPieces][];
    for (var nPiece = 0; nPiece < nPieces; nPiece++) {
      zobristBlack[nPiece] = new Hashcode[nSquares];
      zobristWhite[nPiece] = new Hashcode[nSquares];
    }

    zobristRightsBlack = new Hashcode[4];
    zobristRightsWhite = new Hashcode[4];

    zobristFile = new Hashcode[8];
    zobristDraw = new Hashcode[2];

    zobristExcludedFrom = new Hashcode[nSquares];
    zobristExcludedTo = new Hashcode[nSquares];

    var nUnderPromotions = Promotions.Length - 1;
    if (nUnderPromotions > 0) {
      Debug.Assert(Promotions[0] == Piece.Q, $"Promotion[0] should be {Piece.Q}");
      zobristExcludedPromotion = new Hashcode[nUnderPromotions];
    }
    else
      throw new BoardException("No Underpromotions Defined");

    zobristBuffer = new Byte[8];
    zobristRandom = new Random(0);      // Fixed, repeatable seed
  }

  [MemberNotNull(
    nameof(zobristTurn),
    nameof(zobristExcludedCastles)
    )]
  internal static void loadZobrist() {
    zobristTurn = nextZobrist();

    // For Pieces that can be held by each Square:
    for (var nPiece = 0; nPiece < nPieces; nPiece++) {
      var bPawn = nPiece == vP6;        //[Speed]Pawns cannot appear on their First or Last Rank:
      var nMin = (Int32)(bPawn ? Sq.a2 : Sq.a1);
      var nMax = (Int32)(bPawn ? Sq.h7 : Sq.h8);

      for (var n = nMin; n <= nMax; n++) {
        // White Piece:
        zobristWhite[nPiece][n] = nextZobrist();
        // Black Piece:
        zobristBlack[nPiece][n] = nextZobrist();
      }
    }

    // For Castling Abilities:
    for (var n = 0; n < zobristRightsBlack.Length; n++)
      zobristRightsBlack[n] = nextZobrist();
    for (var n = 0; n < zobristRightsWhite.Length; n++)
      zobristRightsWhite[n] = nextZobrist();

    // For En Passant File:
    for (var n = 0; n < zobristFile.Length; n++)
      zobristFile[n] = nextZobrist();

    // Distinguish actual Draws as well as Draw2:
    for (var n = 0; n < zobristDraw.Length; n++)
      zobristDraw[n] = nextZobrist();

    // For Excluded Moves
    for (var n = 0; n < nSquares; n++) {
      zobristExcludedFrom[n] = nextZobrist();
      zobristExcludedTo[n] = nextZobrist();
    }

    for (var n = 0; n < zobristExcludedPromotion.Length; n++)
      zobristExcludedPromotion[n] = nextZobrist();

    zobristExcludedCastles = nextZobrist();
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
    zobrists.Sort();
    var qLastHash = default(Hashcode);
    foreach (var qHash in zobrists) {
      if (qHash == qLastHash) {
        Trace.Assert(qHash != qLastHash, "Duplicate Hashcode Found");
        break;
      }

      qLastHash = qHash;
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
    if (IsDefined(moveExcluded)) {
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
      qDynamic ^= zobristExcludedFrom[nFrom];
      qDynamic ^= zobristExcludedTo[nTo];

      if (uPromotion > 0) {
        var promotion = (Piece)uPromotion;
        var nIndex = Array.FindIndex(Promotions, p => p == promotion);
        if (nIndex > 0)                 // Distinguish Excluded Promotions
          qDynamic ^= zobristExcludedPromotion[nIndex - 1];
      }

      if (bCastles) {
        //
        //[Chess960]Avoid potential ambiguity of ordinary King moves with castling
        //
#if HashExcludedChess960CastlesOnly
        if (State.IsChess960)
          qDynamic ^= ZobristExcludedCastles;
#else
        //
        // It seems simplest to Hash excluded orthodox castling moves
        // as well as Chess960 castling moves
        //
        qDynamic ^= zobristExcludedCastles;
#endif
      }
    }

    if (IsDraw())
      qDynamic ^= zobristDraw[0];
    else if (IsDraw2())
      qDynamic ^= zobristDraw[1];

    return qDynamic;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private void hashFlags(Boolean bWTM, ref Hashcode qHash) {
    toggleEPHash(ref qHash);
    if (!bWTM) qHash ^= zobristTurn;
  }

  [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
  private Hashcode epHash() {
    return EPTarget.HasValue ? zobristFile[x(EPTarget.Value)] : 0;
  }

  private Hashcode hashPiece(Byte vPiece, Plane qp) {
    Hashcode qHash = 0;
    foreach (var side in Side)
      qHash ^= side.HashPiece(qp, vPiece);
    return qHash;
  }

  private Hashcode hashPawn() {
    return hashPiece(vP6, Pawn);
  }

  private Hashcode hashPieces() {
    Hashcode qHash = 0;
    qHash ^= hashPiece(vR6, Rook);
    qHash ^= hashPiece(vN6, Knight);
    qHash ^= hashPiece(vB6, Bishop);
    qHash ^= hashPiece(vQ6, Queen);
    qHash ^= hashPiece(vK6, King);
    hashFlags(WTM(), ref qHash);
#if HashCastlingRights
    foreach (var side in Side) {
      var fside = side.FlagsSide;
      qHash ^= side.CastlingRightsHash(fside & SideFlags.CanCastle);
    }
#endif
    return qHash;
  }

  protected Boolean TestHash() {
    Boolean bValid = true;
    var qHashPawn = hashPawn();
    var qHash = qHashPawn ^ hashPieces();
#if DisplayHash
    LogLine($" Hash = {formatHash(Hash)}");

    if (qHash != Hash)
      LogLine($"IHash = {formatHash(Hash)}");
#else
    if (qHashPawn != HashPawn) {
      Trace.Assert(qHashPawn == HashPawn, "Full HashPawn differs from Incremental HashPawn");
      bValid = false;
    }

    if (qHash != Hash) {
      Trace.Assert(qHash == Hash, "Full Hash differs from Incremental Hash");
      bValid = false;
    }
#endif                                  // DisplayHash
    return bValid;
  }
  #endregion                            // Hash Methods
}
