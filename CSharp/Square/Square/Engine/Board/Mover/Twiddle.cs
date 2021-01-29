//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-02-09 CNHume]Created File
//
// Conditionals:
//
//#define ImportTwiddle
#define InitDeBruijn
//#define TestDeBruijn
//
// Overall search performance was better for Half de Bruijn on the Dell i7-4702HQ at 2.2 GHz w 4-cores
// x86 was 9.83% faster
// x64 was 6.48% faster
//
// RemoveLo() 47% faster w Half de Bruijn: Avoiding 64-Bit Multiplication on a Compaq 3 GHz Pentium 4
//
#define HalfDeBruijn
//#define FindHi

namespace Engine {
  using static Logging.Logger;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute
  using System.Runtime.InteropServices; // for [DllImport]
  using static System.Math;
  using static System.String;

  //
  // Type Aliases:
  //
  using ExtensionCounter = System.UInt16;
  using PieceHashcode = System.UInt16;  // 10 bits

  partial class Board {
    #region Constants
    protected const Byte vDeBruijn = 0x1D;    //[CNH]0001 1101
#if !InitDeBruijn
    protected static readonly Byte[] deBruijnByte = { 0, 1, 6, 2, 7, 5, 4, 3 };
#endif
    //
    // The lowest bit in a word is isolated by performing an AND with the value in the word
    // and its two's complement: v & -v.  The bit position n within the m-bit word can then
    // be obtained after multiplying an m-bit de Bruijn constant by this isolated bit value
    // and extracting the unique bit pattern from the high order p-bit byte, where p is the
    // PBL of m.  A lookup table corresponding to the de Bruijn constant is then indexed by
    // the unique p-bit value to obtain the bit position n.
    //
    // Note that m-bit multiplies will suffice.  It is also easy to implement half-multiply,
    // which is faster in the case of a 64-bits, because the isolated bit is a power of two.
    //
    // See "Using de Bruijn Sequences to Index a 1 in a Computer Word"
    // Charles E. Leiserson, Harald Prokop, Keith H. Randall, 1998-07-07, MIT LCS
    //
#if HalfDeBruijn
    protected const UInt32 uDeBruijn = 0x077CB531U;
    // From the Paper: 0000 0111 0111 1100 1011 0101 0011 0001
#if !InitDeBruijn
    protected static readonly Byte[] deBruijnHalf =
    {  0,  1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
      31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9 };
#endif
#else
    protected const UInt64 qDeBruijn = 0x022FDD63CC95386DUL;
    // From http://www.chessprogramming.org/De_Bruijn_Sequence_Generator by Gerd Isenberg
#if !InitDeBruijn
    protected static readonly Byte[] deBruijnFull =
    {  0,  1,  2, 53,  3,  7, 54, 27,  4, 38, 41,  8, 34, 55, 48, 28,
      62,  5, 39, 46, 44, 42, 22,  9, 24, 35, 59, 56, 49, 18, 29, 11,
      63, 52,  6, 26, 37, 40, 33, 47, 61, 45, 43, 21, 23, 58, 17, 10,
      51, 25, 36, 32, 60, 20, 57, 16, 50, 31, 19, 15, 30, 14, 13, 12 };
#endif
#endif
    #endregion

    #region Bit Twiddles
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r, out Byte s) {
      s = (Byte)(r & (~r + 1));
      //[Debug]
      Debug.Assert(s != 0, "No Bit Found");
      if (s == 0) return -1;

      r ^= s;                           // r = r & (r - 1);
      var p = (Byte)(s * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r) { // 92 MHz
      var s = r & (~r + 1);
      //[Debug]
      Debug.Assert(s != 0, "No Bit Found");
      if (s == 0) return -1;

      r ^= (Byte)s;                     // r = r & (r - 1);
      var p = (Byte)(s * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }
#if HalfDeBruijn
#if ImportTwiddle
    [DllImport("twiddle.dll", EntryPoint = "?RemoveLo@Twiddle@CSquare@@SA?BHAA_K0@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 RemoveLo(ref UInt64 r, out UInt64 s);
#else
    // Referenced by hashPieces()
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate the lowest/first bit
      r ^= s;                           // r = r & (r - 1); clear from remaining bits

      var u = (UInt32)s;                // Half de Bruijn: Avoiding 64-Bit Multiplication
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        //[Debug]
        Debug.Assert(u != 0, "No Bit Found");
        if (u == 0) return -1;
        n = 32;
      }

      var p = u * uDeBruijn >> 32 - 5;
      return deBruijnHalf[p] + n;
    }
#endif
#if ImportTwiddle
    [DllImport("twiddle.dll", EntryPoint = "?RemoveLo@Twiddle@CSquare@@SA?BHAA_K@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 RemoveLo(ref UInt64 r);
#else
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) { // 76.23 MHz
      var s = r & (~r + 1);             // s = r & -r to isolate the lowest/first bit
      r ^= s;                           // r = r & (r - 1); clear from remaining bits

      var u = (UInt32)s;                // Half de Bruijn: Avoiding 64-Bit Multiplication
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        //[Debug]
        Debug.Assert(u != 0, "No Bit Found");
        if (u == 0) return -1;
        n = 32;
      }

      var p = u * uDeBruijn >> 32 - 5;
      return deBruijnHalf[p] + n;
    }
#endif
#else
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);
      //[Debug]
      Debug.Assert(s != 0, "No Bit Found");
      if (s == 0) return -1;
      r ^= s;                           // r = r & (r - 1);
      var p = s * qDeBruijn >> 64 - 6;
      return deBruijnFull[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) { // 34 MHz
      var s = r & (~r + 1);
      //[Debug]
      Debug.Assert(s != 0, "No Bit Found");
      if (s == 0) return -1;
      r ^= s;                           // r = r & (r - 1);
      var p = s * qDeBruijn >> 64 - 6;
      return deBruijnFull[p];
    }
#endif
#if ImportTwiddle
    [DllImport("twiddle.dll", EntryPoint = "?FindLo@Twiddle@CSquare@@SA?BH_K@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 FindLo(UInt64 r);
#else
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 FindLo(UInt64 r) {
      var r2 = r;
      return RemoveLo(ref r2);
    }
#endif
#if FindHi
    protected static sq sqHi(UInt64 r) {
      for (var n = 0; n < nSquares; n++, r <<= 1)
        if ((r & BIT63) != 0) return (sq)(63 - n);

      throw new ApplicationException("Empty Bit Mask");
    }
#endif
#if InitDeBruijn
    protected static Byte[] newDeBruijn(Int32 nLog) {
      Debug.Assert(nLog <= 8, "smog too large");
      var nLength = 1 << nLog;
      return new Byte[nLength];
    }
#endif
    [Conditional("InitDeBruijn")]
    protected static void loadDeBruijn(Byte[] deBruijnMap, Int32 nLog, UInt64 qDeBruijnNumber) {
      var nLength = 1 << nLog;
      Debug.Assert(deBruijnMap.Length == nLength, "Inconsistent Length");

      var vMask = (Byte)(nLength - 1);
      var m = qDeBruijnNumber;
      for (var n = 0; n < nLength; n++, m <<= 1) {
        var p = vMask & (m >> nLength - nLog);
        Debug.Assert((Int32)p < deBruijnMap.Length, "Index Out of Range",
                     "Index = {0}, Length = {1}", p, deBruijnMap.Length);
        deBruijnMap[p] = (Byte)n;
      }
#if TestDeBruijn
      var s = Empty;
      var sDelim = Empty;
      for (var n = 0; n < nLength; n++) {
        s += $"{sDelim}{deBruijnMap[n],2}";
        sDelim = ", ";
      }

      LogLine(s);
#endif
    }
    #endregion

    #region Math Support
    public UInt16 ISqrt(UInt16 w) {     // 1.4 GHz
      return (UInt16)Sqrt((Double)w);
    }

    public UInt64 USqrt(UInt16 q) {
      if (q == 0)
        return 0;

      //
      // The double precision floating point instruction is fast and accurate;
      // but rounding must be corrected in a few cases.  We seek the greatest
      // integer whose square is no greater than N.
      //
      var init = (UInt16)Sqrt((Double)q);
      var root = (UInt64)init;
      var mean = (root + (q / root)) / 2;

      var count = 0;
      while (root > mean) {
        root--;
        mean = (root + (q / root)) / 2;
        count++;
      }
#if DEBUG
      //
      // Once root reaches 4294967293, root == mean in all but three cases:
      //
      // usqrt: n = 18446744073709551615, root = 4294967295, count = 1
      // usqrt: n = 18446744065119617024, root = 4294967294, count = 1
      // usqrt: n = 18446744056529682435, root = 4294967293, count = 1
      //
      if (root < 4294967293 && count > 0)
        Console.WriteLine("usqrt: n = {0}, root = {1}, count = {2}", q, root, count);
#endif
      return root;
    }
    #endregion

    #region Counter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void setTwoBits(ref PieceHashcode wTwoBitMask, Int32 nIndex, UInt32 u) {
      var bOverflow = u != (u & vTwoBits);
      if (bOverflow) {
        Debug.Assert(!bOverflow, "TwoBits Overflow");
        u &= vTwoBits;
      }

      var nOffset = nIndex * nPerTwoBits;
      var wFieldMask = (PieceHashcode)(vTwoBits << nOffset);
      wTwoBitMask &= (PieceHashcode)~wFieldMask;
      wTwoBitMask |= (PieceHashcode)(u << nOffset);
    }

    //[UCI]Internal Method made available to the GameState class
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static void setNibble(ref ExtensionCounter wNibbleMask, Int32 nIndex, UInt32 u) {
      var bOverflow = u != (u & vNibble);
      if (bOverflow) {
        Debug.Assert(!bOverflow, "Nibble Overflow");
        u &= vNibble;
      }

      var nOffset = nIndex * nPerNibble;
      var wFieldMask = (ExtensionCounter)(vNibble << nOffset);
      wNibbleMask &= (ExtensionCounter)~wFieldMask;
      wNibbleMask |= (ExtensionCounter)(u << nOffset);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    internal static Byte getNibble(ExtensionCounter wNibbleMask, Int32 nIndex) {
      return (Byte)(wNibbleMask >> nIndex * nPerNibble & vNibble);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void incSideCount(BoardSide side, Byte vPiece) {
      side.Counts += 1U << vPiece * nPerNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void decSideCount(BoardSide side, Byte vPiece) {
      side.Counts -= 1U << vPiece * nPerNibble;
    }
    #endregion
  }
}
