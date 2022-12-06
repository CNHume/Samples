//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-08-09 CNHume]Created File
//
// Conditionals:
//
//#define Magic                         //[Note]Magic is slightly slower than Rotation == !Magic
#define HalfMagic                       // Avoiding 64-Bit Division is faster on 3 GHz Pentium 4
//#define TestMagic
//#define TestDiagLo
#define TestRotation
#define DebugDiagIndexers

namespace Engine {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;

  using static Logging.Logger;          // For TestMagic

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Board {
    #region Constants
    private const Int32 nDiagonals = 15;        // nFiles + nRanks - 1
    private const Int32 nStates = 1 << 6;
    private const UInt32 uStateMask = nStates - 1;
#if Magic
    private const Plane qpFileMask = 0x0101010101010101UL;
    private const Plane qpA1H8Mask = 0x8040201008040201UL;
    private const Plane qpA8H1Mask = 0x0102040810204080UL;

    //
    // See http://www.dcs.bbk.ac.uk/~mark/download/bitboard_sliding_icga_final.pdf
    //
    // "Move Generation with Perfect Hash Functions" by Trevor Fenner and Mark Levine,
    // 2008, School of CS and IT, Birkbeck College, University of London
    //
    private const UInt16 wFileModulus = 256 + 2;// (1 << nFiles) + 2
    private const UInt16 wA1H8Modulus = 512 + 2;// (1 << nA1H8) + 2
    private const UInt16 wA8H1Modulus = 256 + 1;// (1 << nA8H1 + 1) + 1

    private const UInt32 uOffset = 42;  // ((1 << (N / 2 * 2)) - 1) * 2 / 3, where N = 6
    private const UInt32 uOffset2 = 21; // Obtained empirically, paper may be in error
#endif
    private static readonly ValueTuple<Int32, Int32>[] KingDeltas =
      { ( 1, 0), ( 1, 1), ( 0, 1), (-1, 1),
        (-1, 0), (-1,-1), ( 0,-1), ( 1,-1) };
    private static readonly ValueTuple<Int32, Int32>[] KnightDeltas =
      { ( 2, 1), ( 1, 2), (-1, 2), (-2, 1),
        (-2,-1), (-1,-2), ( 1,-2), ( 2,-1) };

    //[Dark|Lite]Square is used to determine square color of the Bishops
    internal static void colorSquares() {
      var qp = BIT0;
      for (var y = 0U; y < nRanks; y++)
        for (var x = 0U; x < nFiles; x++, qp <<= 1)
          if (IsOdd(x + y)) SquareLite |= qp;

      SquareDark = ~SquareLite;
    }
    #endregion

    #region Piece Atx
    private static void newKingAtx() {
      AtxKing = new Plane[nSquares];
    }

    private static void newKnightAtx() {
      AtxKnight = new Plane[nSquares];
    }

    private static Boolean inBounds(Int32 nX, Int32 nY) {
      return nX >= 0 && nX < nFiles &&
             nY >= 0 && nY < nRanks;
    }

    private static void loadPieceAtx(
      Plane[] qpAtx, ValueTuple<Int32, Int32>[] deltas) {
      foreach (var (ndx, ndy) in deltas) {
        var nFrom = 0;
        for (var y = 0; y < nRanks; y++)
          for (var x = 0; x < nFiles; x++, nFrom++) {
            var nX = x + ndx;
            var nY = y + ndy;

            if (inBounds(nX, nY)) {
              //[Optimized]var nFrom = sqr(x, y);
              var nTo = sqr(nX, nY);
              qpAtx[nFrom] |= bit(nTo);
            }
          }
      }
    }

    internal static void loadPieceAtx() {
      loadPieceAtx(AtxKing, KingDeltas);
      loadPieceAtx(AtxKnight, KnightDeltas);
    }
    #endregion

    #region Atx Lookup Table Initialization
    private static void newOrthAtx() {
      AtxFile = new Plane[nStates][];
      AtxRank = new Plane[nStates][];

      for (var nState = 0; nState < nStates; nState++) {
        AtxFile[nState] = new Plane[nSquares];
        AtxRank[nState] = new Plane[nSquares];
      }
    }

    private static void newDiagAtx() {
      AtxA1H8 = new Plane[nStates][];
      AtxA8H1 = new Plane[nStates][];

      for (var nState = 0; nState < nStates; nState++) {
        AtxA1H8[nState] = new Plane[nSquares];
        AtxA8H1[nState] = new Plane[nSquares];
      }
    }

    internal static void loadOrthAtx() {
      for (UInt32 uState = 0; uState < nStates; uState++) {
        //
        // Ray State values consist only of the medial 6 bits.
        // Add Hi and Lo limit bits here:
        //
        var uOrth = uBit(7) | uState << 1 | uBit(0);

        for (var y = 0; y < nRanks; y++) {
          var yInverse = InvertRank(y);

          for (var x = 0; x < nFiles; x++) {
            var nRankPos = sqr(x, y);
            var nFilePos = sqr(yInverse, x);

            var xUp = x + 1;
            var bLoop = xUp < nFiles;
            //
            // While building the Ray Atx Tables, bLoop remains true until uOrth indicates
            // that the piece sliding from the reference square has run into another piece.
            //
            for (var m = 1 << xUp; bLoop; bLoop = (uOrth & m) == 0, m <<= 1, xUp++) {
              AtxRank[uState][nRankPos] |= bit(sqr(xUp, y));
#if Magic
              AtxFile[MagicFile[uState]][nFilePos] |= bit(sqr(yInverse, xUp));
#else
              AtxFile[uState][nFilePos] |= bit(sqr(yInverse, xUp));
#endif
            }

            var xDn = x - 1;
            bLoop = x > 0;
            for (var m = 1 << xDn; bLoop; bLoop = (uOrth & m) == 0, m >>= 1, xDn--) {
              AtxRank[uState][nRankPos] |= bit(sqr(xDn, y));
#if Magic
              AtxFile[MagicFile[uState]][nFilePos] |= bit(sqr(yInverse, xDn));
#else
              AtxFile[uState][nFilePos] |= bit(sqr(yInverse, xDn));
#endif
            }
          }
        }
      }
    }

    //
    // The approach used here is based on "Rotated bitmaps, a new twist on an old idea" by Dr. Robert Hyatt
    // See http://www.craftychess.com/hyatt/bitmaps.html
    //
    internal static void loadDiagAtx() {
      for (UInt32 uState = 0; uState < nStates; uState++) {
        for (var d = 0; d < nDiagonals; d++) {
          var nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;
          var uDiagMask = uBit(nDiagLen) - 1;
          //
          // Ray State values consist only of the medial 6 bits.
          // Add Hi and Lo limit bits here:
          //
          var uDiag = (uBit(nDiagLen - 1) | uState << 1 | uBit(0)) & uDiagMask;

          for (Int32 x = d < nFiles ? 7 - d : 0,
                     y = d < nFiles ? 0 : d - 7,
                     w = 0; w < nDiagLen; w++, x++, y++) {
            var xInverse = InvertFile(x);
#if DebugDiagIndexers
            Debug.Assert(diagA1H8(sqr(x, y)) == d, "A1H8 mismatch");
            Debug.Assert(diagA8H1(sqr(xInverse, y)) == d, "A8H1 mismatch");
#endif
            var nA1H8Pos = sqr(x, y);
            var nA8H1Pos = sqr(xInverse, y);

            var xUp = x + 1;
            var yUp = y + 1;
            var bLoop = xUp < nFiles && yUp < nRanks;
            //
            // While building the Ray Atx Tables, bLoop remains true until uDiag indicates
            // that the piece sliding from the reference square has run into another piece.
            //
            for (var m = 1 << w + 1; bLoop; bLoop = (uDiag & m) == 0,
                                            m <<= 1, xUp++, yUp++) {
              var xUpInverse = InvertFile(xUp);
#if Magic
              AtxA1H8[MagicA1H8[uState]][nA1H8Pos] |= bit(sqr(xUp, yUp));
              AtxA8H1[MagicA8H1[uState]][nA8H1Pos] |= bit(sqr(xUpInverse, yUp));
#else
              AtxA1H8[uState][nA1H8Pos] |= bit(sqr(xUp, yUp));
              AtxA8H1[uState][nA8H1Pos] |= bit(sqr(xUpInverse, yUp));
#endif
            }

            var xDn = x - 1;
            var yDn = y - 1;
            bLoop = x > 0 && y > 0;
            for (var m = 1U << w - 1; bLoop; bLoop = (uDiag & m) == 0,
                                             m >>= 1, xDn--, yDn--) {
              var xDnInverse = InvertFile(xDn);
#if Magic
              AtxA1H8[MagicA1H8[uState]][nA1H8Pos] |= bit(sqr(xDn, yDn));
              AtxA8H1[MagicA8H1[uState]][nA8H1Pos] |= bit(sqr(xDnInverse, yDn));
#else
              AtxA1H8[uState][nA1H8Pos] |= bit(sqr(xDn, yDn));
              AtxA8H1[uState][nA8H1Pos] |= bit(sqr(xDnInverse, yDn));
#endif
            }
          }
        }
      }
    }
    #endregion

    #region Ray Atx
    private static void newRankOffset() {
      RankOffset = new Byte[nSquares];
    }

    internal static void loadRankOffset() {
      for (var n = 0; n < nSquares; n++)
        RankOffset[n] = (Byte)(nFiles * y(n) + 1);
    }
#if Magic
    private static void newDiagLo() {
      LoA1H8 = new Int32[nSquares];
      LoA8H1 = new Int32[nSquares];
    }

    internal static void newMagic() {
      newDiagLo();

      MagicFile = new Byte[nStates];
      MagicA1H8 = new Byte[nStates];
      MagicA8H1 = new Byte[nStates];
#if TestMagic
      StateFile = new Byte[nStates];
      StateA1H8 = new Byte[nStates];
      StateA8H1 = new Byte[nStates];
#endif
    }

    //
    // These Lo bit tables help quickly normalize Position State for diagonals
    // in the "magic" hash functions below, which return a Ray State index.
    //
    private static void loadDiagLo() {
      for (var d = 0; d < nDiagonals; d++) {
        Int32 xLo = d < nFiles ? 7 - d : 0,
              yLo = d < nFiles ? 0 : d - 7,
              nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;

        var xLoInverse = InvertFile(xLo);
        var nA1H8Lo = sqr(xLo, yLo);
        var nA8H1Lo = sqr(xLoInverse, yLo);
#if TestDiagLo
        LogLine($"d = {d,2}: A1H8Lo = {(sq)nA1H8Lo}, A8H1Lo = {(sq)nA8H1Lo}");
#endif
        for (Int32 x = xLo, y = yLo, w = 0; w < nDiagLen; w++, x++, y++) {
          var xInverse = InvertFile(x);

          LoA1H8[sqr(x, y)] = nA1H8Lo;
          LoA8H1[sqr(xInverse, y)] = nA8H1Lo;
        }
      }
#if TestDiagLo
      LogLine();
      printOffsets("A1H8Lo", A1H8Lo);
      printOffsets("A8H1Lo", A8H1Lo);
#endif
    }

    internal static void loadMagic() {
      loadDiagLo();                     // Build hash function lookup tables
#if TestMagic
      const Byte vUnused = Byte.MaxValue;
      for (var n = 0; n < nStates; n++) {
        StateFile[n] = vUnused;
        StateA1H8[n] = vUnused;
        StateA8H1[n] = vUnused;
      }

      var nUsed = 0;
#endif
      for (Byte vState = 0; vState < nStates; vState++) {
        var mState = 1U << 7 | (UInt32)vState << 1 | 1U;
        var m = 1U << 7;

        var qpFile = 0UL;
        var qpA8H1 = 0UL;
        var qpA1H8 = 0UL;

        for (var n = 0; n < nFiles; n++, m >>= 1) {
          qpFile <<= nFiles;
          qpA8H1 <<= nA8H1;
          qpA1H8 <<= nA1H8;

          if ((mState & m) != 0) {
            qpFile |= BIT0;
            qpA1H8 |= BIT0;
            qpA8H1 |= BIT0;
          }
        }

        //
        // The Plane values cover all permutations of piece placement corresponding to each State
        // by starting from a1 and proceeding along their respective Ray.  Thus, Plane values can
        // be hashed from the perspective of a1 with no further shifting.
        //
#if TestMagic || HalfMagic
        var vFileHalf = hashFileHalf(qpFile, 0);
        var vA1H8Half = hashA1H8Half(qpA1H8, 0);
        var vA8H1Half = hashA8H1Half(qpA8H1, 0);
#endif
#if TestMagic || !HalfMagic
        var vFileFull = hashFileFull(qpFile, 0);
        var vA1H8Full = hashA1H8Full(qpA1H8, 0);
        var vA8H1Full = hashA8H1Full(qpA8H1, 0);
#endif
#if HalfMagic
        var vFile = vFileHalf;
        var vA1H8 = vA1H8Half;
        var vA8H1 = vA8H1Half;
#else
        var vFile = vFileFull;
        var vA1H8 = vA1H8Full;
        var vA8H1 = vA8H1Full;
#endif
#if TestMagic
        Debug.Assert(vFileHalf == vFileFull, "FileHalf != FileFull");
        Debug.Assert(vA1H8Half == vA1H8Full, "A1H8Half != A1H8Full");
        Debug.Assert(vA8H1Half == vA8H1Full, "A8H1Half != A8H1Full");

        if (StateFile[vFileHalf] != vUnused) {
          LogLine($"{++nUsed,2}) StateFile[{vFileHalf,3}]");
          writeBinary("Old", StateFile[vFileHalf], 8);
          writeBinary("New", mState, 8);
        }

        if (StateA1H8[vA1H8Half] != vUnused) {
          LogLine($"{++nUsed,2}) StateA1H8[{vA1H8Half,3}]");
          writeBinary("Old", StateA1H8[vA1H8Half], 8);
          writeBinary("New", mState, 8);
        }

        if (StateA8H1[vA8H1Half] != vUnused) {
          LogLine($"{++nUsed,2}) StateA8H1[{vA8H1Half,3}]");
          writeBinary("Old", StateA8H1[vA8H1Half], 8);
          writeBinary("New", mState, 8);
        }

        StateFile[vFile] = vState;
        StateA1H8[vA1H8] = vState;
        StateA8H1[vA8H1] = vState;
#endif
        MagicFile[vState] = vFile;
        MagicA1H8[vState] = vA1H8;
        MagicA8H1[vState] = vA8H1;
      }
#if TestMagic
      var nFileUsed = 0;
      var nA1H8Used = 0;
      var nA8H1Used = 0;

      for (var n = 0; n < StateFile.Length; n++)
        if (StateFile[n] == vUnused)
          LogLine($"StateFile[{n,2}] unused");
        else
          nFileUsed++;

      for (var n = 0; n < StateA1H8.Length; n++)
        if (StateA1H8[n] == vUnused)
          LogLine($"StateA1H8[{n,2}] unused");
        else
          nA1H8Used++;

      for (var n = 0; n < StateA8H1.Length; n++)
        if (StateA8H1[n] == vUnused)
          LogLine($"StateA8H1[{n,2}] unused");
        else
          nA8H1Used++;

      LogLine($"FileUsed = {nFileUsed}");
      LogLine($"A1H8Used = {nA1H8Used}");
      LogLine($"A8H1Used = {nA8H1Used}");
#endif
    }
#else                                   //!Magic
    private static void newRotation() {
      OffsetDiag = new Byte[nDiagonals];
      OffsetFile = new Byte[nSquares];
      OffsetA1H8 = new Byte[nSquares];
      OffsetA8H1 = new Byte[nSquares];
    }

    internal static void loadRotation() {
      var nDiagLen = 0;                 //[Note]OffsetDiag increments by previous nDiagLen
      for (var d = 0; d < nDiagonals; d++) {
        OffsetDiag[d] = (Byte)(d > 0 ? OffsetDiag[d - 1] + nDiagLen : nDiagLen);
        nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;
      }

      for (var n = 0; n < nSquares; n++) {
        var xInverse = InvertFile(x(n));
        //[Note]One is added to each Offset because Ray State values consist only of the medial 6 bits:
        OffsetFile[n] = (Byte)(nFiles * xInverse + 1);

        OffsetA1H8[n] = (Byte)(OffsetDiag[diagA1H8(n)] + 1);
        OffsetA8H1[n] = (Byte)(OffsetDiag[diagA8H1(n)] + 1);
      }
    }

    private static void newOrthBit() {
      BitFile = new Plane[nSquares];
      BitRank = new Plane[nSquares];
    }

    private static void newDiagBit() {
      BitA1H8 = new Plane[nSquares];
      BitA8H1 = new Plane[nSquares];
    }

    internal static void loadOrthBit() {
      var qp = BIT0;
      for (var y = 0; y < nRanks; y++) {
        var yInverse = InvertRank(y);
        for (var x = 0; x < nFiles; x++, qp <<= 1)
          BitFile[sqr(yInverse, x)] = BitRank[sqr(x, y)] = qp;
      }
    }

    internal static void loadDiagBit() {
      var qp = BIT0;
      for (var d = 0; d < nDiagonals; d++) {
        var nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;

        for (Int32 x = d < nFiles ? 7 - d : 0,
                   y = d < nFiles ? 0 : d - 7,
                   w = 0; w < nDiagLen; w++, x++, y++, qp <<= 1) {
          var xInverse = InvertFile(x);
          BitA8H1[sqr(xInverse, y)] = BitA1H8[sqr(x, y)] = qp;
        }
      }
    }
#endif                                  //!Magic
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane RayA1H8(Int32 n) {
#if Magic && HalfMagic
      return AtxA1H8[hashA1H8Half(RankPiece, n)][n];
#elif Magic
      return AtxA1H8[hashA1H8Full(RankPiece, n)][n];
#else
      return AtxA1H8[rotateA1H8(n)][n];
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane RayA8H1(Int32 n) {
#if Magic && HalfMagic
      return AtxA8H1[hashA8H1Half(RankPiece, n)][n];
#elif Magic
      return AtxA8H1[hashA8H1Full(RankPiece, n)][n];
#else
      return AtxA8H1[rotateA8H1(n)][n];
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane RayFile(Int32 n) {
#if Magic && HalfMagic
      return AtxFile[hashFileHalf(RankPiece, n)][n];
#elif Magic
      return AtxFile[hashFileFull(RankPiece, n)][n];
#else
      return AtxFile[rotateFile(n)][n];
#endif
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane RayRank(Int32 n) {
      return AtxRank[rotateRank(n)][n];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane RayDiag(Int32 n) {
      return RayA1H8(n) | RayA8H1(n);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane RayOrth(Int32 n) {
      return RayRank(n) | RayFile(n);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected Plane Ray(Int32 n) {
      return RayDiag(n) | RayOrth(n);
    }
    #endregion

    #region Ray State Accessors
#if Magic
#if TestMagic || !HalfMagic
    internal static Byte hashFileFull(Plane qp, Int32 n) {
      var qFileState = qp >> x(n) + nFiles & (qpFileMask >> 2 * nFiles);
      qFileState += uOffset;
      return (Byte)(qFileState % wFileModulus);
    }

    internal static Byte hashA1H8Full(Plane qp, Int32 n) {
      var qA1H8State = qp >> LoA1H8[n] + nA1H8 & (qpA1H8Mask >> 2 * nA1H8);
      qA1H8State += uOffset;
      return (Byte)(qA1H8State % wA1H8Modulus);
    }

    internal static Byte hashA8H1Full(Plane qp, Int32 n) {
      //[Note]qpA8H1Mask Lo Bit is 1 << nA8H1
      var qA8H1State = qp >> LoA8H1[n] + nA8H1 & (qpA8H1Mask >> 3 * nA8H1);
      qA8H1State <<= 6 - 1;
      qA8H1State += uOffset2;
      return (Byte)(qA8H1State % wA8H1Modulus);
    }
#endif
#if TestMagic || HalfMagic
    //
    // Given a common modulus: if A1 is congruent to A2; and B1 is congruent to B2
    // then A1*B1 will be congruent to A2*B2
    //
    internal static Byte hashFileHalf(Plane qp, Int32 n) {
      const UInt16 wFileRem = (UInt16)(BIT32 % wFileModulus);   // 16
      var qFileState = qp >> x(n) + nFiles & (qpFileMask >> 2 * nFiles);
      qFileState += uOffset;
      var uHi = (UInt32)(qFileState >> 32);    // Avoiding 64-Bit Division
      var uLo = (UInt32)qFileState;
      var uFileState = wFileRem * uHi + uLo;
      return (Byte)(uFileState % wFileModulus);
    }

    internal static Byte hashA1H8Half(Plane qp, Int32 n) {
      const UInt16 wA1H8Rem = (UInt16)(BIT32 % wA1H8Modulus);   // 258
      var qA1H8State = qp >> LoA1H8[n] + nA1H8 & (qpA1H8Mask >> 2 * nA1H8);
      qA1H8State += uOffset;
      var uHi = (UInt32)(qA1H8State >> 32);    // Avoiding 64-Bit Division
      var uLo = (UInt32)qA1H8State;
      var uA1H8State = wA1H8Rem * uHi + uLo;
      return (Byte)(uA1H8State % wA1H8Modulus);
    }

    internal static Byte hashA8H1Half(Plane qp, Int32 n) {
      const UInt16 wA8H1Rem = (UInt16)(BIT32 % wA8H1Modulus);   // 1
      //[Note]qpA8H1Mask Lo Bit is 1 << nA8H1
      var qA8H1State = qp >> LoA8H1[n] + nA8H1 & (qpA8H1Mask >> 3 * nA8H1);
      qA8H1State <<= 6 - 1;
      qA8H1State += uOffset2;
      var uHi = (UInt32)(qA8H1State >> 32);    // Avoiding 64-Bit Division
      var uLo = (UInt32)qA8H1State;
      var uA8H1State = wA8H1Rem * uHi + uLo;
      return (Byte)(uA8H1State % wA8H1Modulus);
    }
#endif
#else                                   //!Magic
    private Byte rotateA1H8(Int32 n) {
      var uA1H8Rotate = (UInt32)(A1H8Piece >> OffsetA1H8[n]);
      return (Byte)(uA1H8Rotate & uStateMask);
    }

    private Byte rotateA8H1(Int32 n) {
      var uA8H1Rotate = (UInt32)(A8H1Piece >> OffsetA8H1[n]);
      return (Byte)(uA8H1Rotate & uStateMask);
    }

    private Byte rotateFile(Int32 n) {
#if NoFileOffset
      var nFileOffset = nFiles * invertFile(x(n)) + 1;
#endif
      var uFileRotate = (UInt32)(FilePiece >> OffsetFile[n]);
      return (Byte)(uFileRotate & uStateMask);
    }
#endif                                  //!Magic
    internal Byte rotateRank(Int32 n) {
#if NoRankOffset
      var nRankOffset = nFiles * y(n) + 1;
#endif
      var uRankRotate = (UInt32)(RankPiece >> RankOffset[n]);
      return (Byte)(uRankRotate & uStateMask);
    }
    #endregion

    #region Board Coordinate Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Boolean IsOrth(Int32 nFrom, Int32 nTo) {
      var n = nFrom ^ nTo;
      // Are either of the coordinates equal?
      return x(n) == 0 || y(n) == 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 diagA1H8(Int32 n) {
      return InvertFile(x(n)) + y(n);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 diagA8H1(Int32 n) {
      return x(n) + y(n);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 sqr(Int32 x, Int32 y) {
      return nFiles * y + x;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 x(Int32 n) {
      return n % nFiles;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 y(Int32 n) {
      return n / nFiles;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 InvertFile(Int32 x) {
      return nFiles - (x + 1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 InvertRank(Int32 y) {
      return nRanks - (y + 1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 InvertDiag(Int32 d) {
      return nDiagonals - (d + 1);
    }
    #endregion
  }
}
