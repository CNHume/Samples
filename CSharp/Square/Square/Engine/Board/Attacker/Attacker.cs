//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-08-09 CNHume]Created File
//
// Conditionals:
//
//#define UnrollAtx
//#define Magic                         //[Note]Magic is slightly slower than Rotation == !Magic
#define HalfMagic                       // Avoiding 64-Bit Division is faster on 3 GHz Pentium 4
//#define TestMagic
//#define TestDiagLo
#define TestRotation
#define DebugDiagIndexers

namespace Engine {
  using static Logging.Logger;          // For TestMagic

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Board {
    #region Constants
    protected const Int32 nDiagonals = 15;      // nFiles + nRanks - 1
    protected const Int32 nStates = 1 << 6;
    protected const UInt32 uStateMask = nStates - 1;
#if Magic
    protected const Plane qpFileMask = 0x0101010101010101UL;
    protected const Plane qpA1H8Mask = 0x8040201008040201UL;
    protected const Plane qpA8H1Mask = 0x0102040810204080UL;

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
    protected static void colorSquares() {
      var qp = BIT0;
      for (var y = 0U; y < nRanks; y++)
        for (var x = 0U; x < nFiles; x++, qp <<= 1)
          if (IsOdd(x + y)) LiteSquare |= qp;

      DarkSquare = ~LiteSquare;
    }
    #endregion

    #region Piece Atx
    private static void newKingAtx() {
      KingAtx = new Plane[nSquares];
    }

    private static void newKnightAtx() {
      KnightAtx = new Plane[nSquares];
    }

    private static Boolean inBounds(Int32 nX, Int32 nY) {
      return nX >= 0 && nX < nFiles &&
             nY >= 0 && nY < nRanks;
    }

    private static void loadPiecetAtx(
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
              qpAtx[nFrom] |= BIT0 << nTo;
            }
          }
      }
    }

    protected static void loadPieceAtx() {
      loadPiecetAtx(KingAtx, KingDeltas);
      loadPiecetAtx(KnightAtx, KnightDeltas);
    }
    #endregion

    #region Atx Lookup Table Initialization
    private static void newRectAtx() {
      FileAtx = new Plane[nStates][];
      RankAtx = new Plane[nStates][];

      for (var nState = 0; nState < nStates; nState++) {
        FileAtx[nState] = new Plane[nSquares];
        RankAtx[nState] = new Plane[nSquares];
      }
    }

    private static void newDiagAtx() {
      A1H8Atx = new Plane[nStates][];
      A8H1Atx = new Plane[nStates][];

      for (var nState = 0; nState < nStates; nState++) {
        A1H8Atx[nState] = new Plane[nSquares];
        A8H1Atx[nState] = new Plane[nSquares];
      }
    }

    //
    // When building the Ray Atx Tables, bLoop remains true until mState indicates
    // that the piece sliding from the reference square has run into another piece.
    //
    protected static void loadRectAtx() {
      for (Byte vState = 0; vState < nStates; vState++) {
        var mState = 1 << 7 | vState << 1 | 1;

        for (var y = 0; y < nRanks; y++) {
          var yInverse = invertRank(y);

          for (var x = 0; x < nFiles; x++) {
            var nRankPos = sqr(x, y);
            var nFilePos = sqr(yInverse, x);

            var xUp = x + 1;
            var bLoop = xUp < nFiles;
            for (var m = 1 << xUp; bLoop; bLoop = (mState & m) == 0, m <<= 1, xUp++) {
              RankAtx[vState][nRankPos] |= BIT0 << sqr(xUp, y);
#if Magic
              FileAtx[FileMagic[vState]][nFilePos] |= BIT0 << sqr(yInverse, xUp);
#else
              FileAtx[vState][nFilePos] |= BIT0 << sqr(yInverse, xUp);
#endif
            }

            var xDn = x - 1;
            bLoop = x >= 1;
            for (var m = 1 << xDn; bLoop; bLoop = (mState & m) == 0, m >>= 1, xDn--) {
              RankAtx[vState][nRankPos] |= BIT0 << sqr(xDn, y);
#if Magic
              FileAtx[FileMagic[vState]][nFilePos] |= BIT0 << sqr(yInverse, xDn);
#else
              FileAtx[vState][nFilePos] |= BIT0 << sqr(yInverse, xDn);
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
    protected static void loadDiagAtx() {
      for (Byte vState = 0; vState < nStates; vState++) {
        for (var d = 0; d < nDiagonals; d++) {
          var nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;
          var vDiagMask = (Byte)((1 << nDiagLen) - 1);
          var mDiagState = (1 << nDiagLen - 1 | vState << 1 | 1) & vDiagMask;

          for (Int32 x = d < nFiles ? 7 - d : 0,
                     y = d < nFiles ? 0 : d - 7,
                     w = 0; w < nDiagLen; w++, x++, y++) {
            var xInverse = invertFile(x);
#if DebugDiagIndexers
            Debug.Assert(diagA1H8(sqr(x, y)) == d, "A1H8 mismatch");
            Debug.Assert(diagA8H1(sqr(xInverse, y)) == d, "A8H1 mismatch");
#endif
            var nA1H8Pos = sqr(x, y);
            var nA8H1Pos = sqr(xInverse, y);

            var xUp = x + 1;
            var yUp = y + 1;
            var bLoop = xUp < nFiles && yUp < nRanks;
            for (var m = 1 << w + 1; bLoop; bLoop = (mDiagState & m) == 0,
                                            m <<= 1, xUp++, yUp++) {
              var xUpInverse = invertFile(xUp);
#if Magic
              A1H8Atx[A1H8Magic[vState]][nA1H8Pos] |= BIT0 << sqr(xUp, yUp);
              A8H1Atx[A8H1Magic[vState]][nA8H1Pos] |= BIT0 << sqr(xUpInverse, yUp);
#else
              A1H8Atx[vState][nA1H8Pos] |= BIT0 << sqr(xUp, yUp);
              A8H1Atx[vState][nA8H1Pos] |= BIT0 << sqr(xUpInverse, yUp);
#endif
            }

            var xDn = x - 1;
            var yDn = y - 1;
            bLoop = x >= 1 && y >= 1;
            for (var m = 1U << w - 1; bLoop; bLoop = (mDiagState & m) == 0,
                                             m >>= 1, xDn--, yDn--) {
              var xDnInverse = invertFile(xDn);
#if Magic
              A1H8Atx[A1H8Magic[vState]][nA1H8Pos] |= BIT0 << sqr(xDn, yDn);
              A8H1Atx[A8H1Magic[vState]][nA8H1Pos] |= BIT0 << sqr(xDnInverse, yDn);
#else
              A1H8Atx[vState][nA1H8Pos] |= BIT0 << sqr(xDn, yDn);
              A8H1Atx[vState][nA8H1Pos] |= BIT0 << sqr(xDnInverse, yDn);
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

    protected static void loadRankOffset() {
      for (var n = 0; n < nSquares; n++)
        RankOffset[n] = (Byte)(nFiles * y(n) + 1);
    }
#if Magic
    protected static void newDiagLo() {
      A1H8Lo = new Int32[nSquares];
      A8H1Lo = new Int32[nSquares];
    }

    protected static void newMagic() {
      newDiagLo();

      FileMagic = new Byte[nStates];
      A1H8Magic = new Byte[nStates];
      A8H1Magic = new Byte[nStates];
#if TestMagic
      FileState = new Byte[nStates];
      A1H8State = new Byte[nStates];
      A8H1State = new Byte[nStates];
#endif
    }

    //
    // These Lo bit tables help quickly normalize Position State for diagonals
    // in the "magic" hash functions below - which return a Ray State index.
    //
    protected static void loadDiagLo() {
      for (var d = 0; d < nDiagonals; d++) {
        Int32 xLo = d < nFiles ? 7 - d : 0,
              yLo = d < nFiles ? 0 : d - 7,
              nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;

        var xLoInverse = invertFile(xLo);
        var nA1H8Lo = sqr(xLo, yLo);
        var nA8H1Lo = sqr(xLoInverse, yLo);
#if TestDiagLo
        LogLine($"d = {d,2}: A1H8Lo = {(sq)nA1H8Lo}, A8H1Lo = {(sq)nA8H1Lo}");
#endif
        for (Int32 x = xLo, y = yLo, w = 0; w < nDiagLen; w++, x++, y++) {
          var xInverse = invertFile(x);

          A1H8Lo[sqr(x, y)] = nA1H8Lo;
          A8H1Lo[sqr(xInverse, y)] = nA8H1Lo;
        }
      }
#if TestDiagLo
      LogLine();
      printOffsets("A1H8Lo", A1H8Lo);
      printOffsets("A8H1Lo", A8H1Lo);
#endif
    }

    protected static void loadMagic() {
      loadDiagLo();                     // Build hash function lookup tables
#if TestMagic
      const Byte vUnused = Byte.MaxValue;
      for (var n = 0; n < nStates; n++) {
        FileState[n] = vUnused;
        A1H8State[n] = vUnused;
        A8H1State[n] = vUnused;
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

        if (FileState[vFileHalf] != vUnused) {
          LogLine($"{++nUsed,2}) FileState[{vFileHalf,3}]");
          writeBinary("Old", FileState[vFileHalf], 8);
          writeBinary("New", mState, 8);
        }

        if (A1H8State[vA1H8Half] != vUnused) {
          LogLine($"{++nUsed,2}) A1H8State[{vA1H8Half,3}]");
          writeBinary("Old", A1H8State[vA1H8Half], 8);
          writeBinary("New", mState, 8);
        }

        if (A8H1State[vA8H1Half] != vUnused) {
          LogLine($"{++nUsed,2}) A8H1State[{vA8H1Half,3}]");
          writeBinary("Old", A8H1State[vA8H1Half], 8);
          writeBinary("New", mState, 8);
        }

        FileState[vFile] = vState;
        A1H8State[vA1H8] = vState;
        A8H1State[vA8H1] = vState;
#endif
        FileMagic[vState] = vFile;
        A1H8Magic[vState] = vA1H8;
        A8H1Magic[vState] = vA8H1;
      }
#if TestMagic
      var nFileUsed = 0;
      var nA1H8Used = 0;
      var nA8H1Used = 0;

      for (var n = 0; n < FileState.Length; n++)
        if (FileState[n] == vUnused)
          LogLine($"FileState[{n,2}] unused");
        else
          nFileUsed++;

      for (var n = 0; n < A1H8State.Length; n++)
        if (A1H8State[n] == vUnused)
          LogLine($"A1H8State[{n,2}] unused");
        else
          nA1H8Used++;

      for (var n = 0; n < A8H1State.Length; n++)
        if (A8H1State[n] == vUnused)
          LogLine($"A8H1State[{n,2}] unused");
        else
          nA8H1Used++;

      LogLine($"FileUsed = {nFileUsed}");
      LogLine($"A1H8Used = {nA1H8Used}");
      LogLine($"A8H1Used = {nA8H1Used}");
#endif
    }
#else                                   //!Magic
    private static void newRotation() {
      DiagOffset = new Byte[nDiagonals];
      FileOffset = new Byte[nSquares];
      A1H8Offset = new Byte[nSquares];
      A8H1Offset = new Byte[nSquares];
    }

    protected static void loadRotation() {
      var nDiagLen = 0;                 //[Note]DiagOffset increments by previous nDiagLen
      for (var d = 0; d < nDiagonals; d++) {
        DiagOffset[d] = (Byte)(d > 0 ? DiagOffset[d - 1] + nDiagLen : nDiagLen);
        nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;
      }

      for (var n = 0; n < nSquares; n++) {
        var xInverse = invertFile(x(n));
        //[Note]One is added to each Offset because status values consist only of the medial 6 bits:
        FileOffset[n] = (Byte)(nFiles * xInverse + 1);

        A1H8Offset[n] = (Byte)(DiagOffset[diagA1H8(n)] + 1);
        A8H1Offset[n] = (Byte)(DiagOffset[diagA8H1(n)] + 1);
      }
    }

    private static void newRectBit() {
      FileBit = new Plane[nSquares];
      RankBit = new Plane[nSquares];
    }

    private static void newDiagBit() {
      A1H8Bit = new Plane[nSquares];
      A8H1Bit = new Plane[nSquares];
    }

    protected static void loadRectBit() {
      var qp = BIT0;
      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);
        for (var x = 0; x < nFiles; x++, qp <<= 1)
          FileBit[sqr(yInverse, x)] = RankBit[sqr(x, y)] = qp;
      }
    }

    protected static void loadDiagBit() {
      var qp = BIT0;
      for (var d = 0; d < nDiagonals; d++) {
        var nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;

        for (Int32 x = d < nFiles ? 7 - d : 0,
                   y = d < nFiles ? 0 : d - 7,
                   w = 0; w < nDiagLen; w++, x++, y++, qp <<= 1) {
          var xInverse = invertFile(x);
          A8H1Bit[sqr(xInverse, y)] = A1H8Bit[sqr(x, y)] = qp;
        }
      }
    }
#endif                                  //!Magic
    protected Plane a1h8Atx(Int32 n) {
#if Magic && HalfMagic
      return A1H8Atx[hashA1H8Half(RankPiece, n)][n];
#elif Magic
      return A1H8Atx[hashA1H8Full(RankPiece, n)][n];
#else
      return A1H8Atx[rotateA1H8(n)][n];
#endif
    }

    protected Plane a8h1Atx(Int32 n) {
#if Magic && HalfMagic
      return A8H1Atx[hashA8H1Half(RankPiece, n)][n];
#elif Magic
      return A8H1Atx[hashA8H1Full(RankPiece, n)][n];
#else
      return A8H1Atx[rotateA8H1(n)][n];
#endif
    }

    protected Plane fileAtx(Int32 n) {
#if Magic && HalfMagic
      return FileAtx[hashFileHalf(RankPiece, n)][n];
#elif Magic
      return FileAtx[hashFileFull(RankPiece, n)][n];
#else
      return FileAtx[rotateFile(n)][n];
#endif
    }

    protected Plane rankAtx(Int32 n) {
      return RankAtx[rotateRank(n)][n];
    }
#if UnrollAtx
    protected Plane diagAtx(Int32 n) {
#if Magic && HalfMagic
      return A1H8Atx[hashA1H8Half(RankPiece, n)][n] | A8H1Atx[hashA8H1Half(RankPiece, n)][n];
#elif Magic
      return A1H8Atx[hashA1H8Full(RankPiece, n)][n] | A8H1Atx[hashA8H1Full(RankPiece, n)][n];
#else
      return A1H8Atx[rotateA1H8(n)][n] | A8H1Atx[rotateA8H1(n)][n]; // 17.67 MHz
#endif
    }

    protected Plane rectAtx(Int32 n) {
#if Magic && HalfMagic
      return RankAtx[rotateRank(n)][n] | FileAtx[hashFileHalf(RankPiece, n)][n];
#elif Magic
      return RankAtx[rotateRank(n)][n] | FileAtx[hashFileFull(RankPiece, n)][n];
#else
      return RankAtx[rotateRank(n)][n] | FileAtx[rotateFile(n)][n]; // 19 MHz
#endif
    }
#else
    protected Plane diagAtx(Int32 n) {
      return a1h8Atx(n) | a8h1Atx(n);
    }

    protected Plane rectAtx(Int32 n) {
      return rankAtx(n) | fileAtx(n);
    }
#endif
    #endregion

    #region Ray State Accessors
#if Magic
#if TestMagic || !HalfMagic
    protected static Byte hashFileFull(Plane qp, Int32 n) {
      var qFileState = qp >> x(n) + nFiles & (qpFileMask >> 2 * nFiles);
      qFileState += uOffset;
      return (Byte)(qFileState % wFileModulus);
    }

    protected static Byte hashA1H8Full(Plane qp, Int32 n) {
      var qA1H8State = qp >> A1H8Lo[n] + nA1H8 & (qpA1H8Mask >> 2 * nA1H8);
      qA1H8State += uOffset;
      return (Byte)(qA1H8State % wA1H8Modulus);
    }

    protected static Byte hashA8H1Full(Plane qp, Int32 n) {
      //[Note]qpA8H1Mask Lo Bit is 1 << nA8H1
      var qA8H1State = qp >> A8H1Lo[n] + nA8H1 & (qpA8H1Mask >> 3 * nA8H1);
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
    protected static Byte hashFileHalf(Plane qp, Int32 n) {
      const UInt16 wFileRem = (UInt16)((BIT0 << 32) % wFileModulus);    // 16
      var qFileState = qp >> x(n) + nFiles & (qpFileMask >> 2 * nFiles);
      qFileState += uOffset;
      var uHi = (UInt32)(qFileState >> 32);    // Avoiding 64-Bit Division
      var uLo = (UInt32)qFileState;
      var uFileState = wFileRem * uHi + uLo;
      return (Byte)(uFileState % wFileModulus);
    }

    protected static Byte hashA1H8Half(Plane qp, Int32 n) {
      const UInt16 wA1H8Rem = (UInt16)((BIT0 << 32) % wA1H8Modulus);    // 258
      var qA1H8State = qp >> A1H8Lo[n] + nA1H8 & (qpA1H8Mask >> 2 * nA1H8);
      qA1H8State += uOffset;
      var uHi = (UInt32)(qA1H8State >> 32);    // Avoiding 64-Bit Division
      var uLo = (UInt32)qA1H8State;
      var uA1H8State = wA1H8Rem * uHi + uLo;
      return (Byte)(uA1H8State % wA1H8Modulus);
    }

    protected static Byte hashA8H1Half(Plane qp, Int32 n) {
      const UInt16 wA8H1Rem = (UInt16)((BIT0 << 32) % wA8H1Modulus);    // 1
      //[Note]qpA8H1Mask Lo Bit is 1 << nA8H1
      var qA8H1State = qp >> A8H1Lo[n] + nA8H1 & (qpA8H1Mask >> 3 * nA8H1);
      qA8H1State <<= 6 - 1;
      qA8H1State += uOffset2;
      var uHi = (UInt32)(qA8H1State >> 32);    // Avoiding 64-Bit Division
      var uLo = (UInt32)qA8H1State;
      var uA8H1State = wA8H1Rem * uHi + uLo;
      return (Byte)(uA8H1State % wA8H1Modulus);
    }
#endif
#else                                   //!Magic
    protected Byte rotateA1H8(Int32 n) {
      var uA1H8Rotate = (UInt32)(A1H8Piece >> A1H8Offset[n]);
      return (Byte)(uA1H8Rotate & uStateMask);
    }

    protected Byte rotateA8H1(Int32 n) {
      var uA8H1Rotate = (UInt32)(A8H1Piece >> A8H1Offset[n]);
      return (Byte)(uA8H1Rotate & uStateMask);
    }

    protected Byte rotateFile(Int32 n) {
#if NoFileOffset
      var nFileOffset = nFiles * invertFile(x(n)) + 1;
#endif
      var uFileRotate = (UInt32)(FilePiece >> FileOffset[n]);
      return (Byte)(uFileRotate & uStateMask);
    }
#endif                                  //!Magic
    protected Byte rotateRank(Int32 n) {
#if NoRankOffset
      var nRankOffset = nFiles * y(n) + 1;
#endif
      var uRankRotate = (UInt32)(RankPiece >> RankOffset[n]);
      return (Byte)(uRankRotate & uStateMask);
    }
    #endregion

    #region Board Coordinate Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Boolean isRect(Int32 nFrom, Int32 nTo) {
      var n = nFrom ^ nTo;
      // Are either of the coordinates equal?
      return x(n) == 0 | y(n) == 0;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Int32 diagA1H8(Int32 n) {
      return invertFile(x(n)) + y(n);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Int32 diagA8H1(Int32 n) {
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
    internal static Int32 invertFile(Int32 x) {
      return nFiles - (x + 1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Int32 invertRank(Int32 y) {
      return nRanks - (y + 1);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static Int32 invertDiag(Int32 d) {
      return nDiagonals - (d + 1);
    }
    #endregion
  }
}
