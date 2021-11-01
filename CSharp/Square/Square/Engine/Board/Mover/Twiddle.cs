//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-02-09 CNHume]Created File
//
// Conditionals:
//
//#define FindHi
//#define ImportTwiddle
//
// RemoveLo() 47% faster w Half de Bruijn: Avoiding 64-Bit Multiplication on a Compaq 3 GHz Pentium 4
//
// Overall search performance was better for Half de Bruijn on the Dell i7-4702HQ at 2.2 GHz w 4-cores
// x86 was 9.83% faster
// x64 was 6.48% faster
//
// Using i7-9700K CPU at 3.60GHz w 8-cores
//
// Overall Search Rate "8/8/8/6N1/8/7R/1K2PRn1/3q2k1 w - - 0 1" [16-ply for ~30 minutes]
// -------------------
// RemoveLoMask  1,189.884 KHz  0.000%
// FullDeBruijn  1,212.314 KHz
// HalfDeBruijn  1,221.797 KHz
//
//#define FullDeBruijn
#define HalfDeBruijn
#define InitDeBruijn
//#define TestDeBruijn

namespace Engine {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;// for MethodImplAttribute
  using System.Runtime.InteropServices; // for [DllImport]

  using static Logging.Logger;

  using static System.Math;
  using static System.String;

  //
  // Type Aliases:
  //
  using ExtensionCounter = System.UInt16;
  using PieceHashcode = System.UInt16;  // 10 bits
  using Plane = System.UInt64;

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
#if FullDeBruijn
    protected const UInt64 qDeBruijn = 0x022FDD63CC95386DUL;
    // From http://www.chessprogramming.org/De_Bruijn_Sequence_Generator by Gerd Isenberg
#if !InitDeBruijn
    protected static readonly Byte[] deBruijnFull =
    {  0,  1,  2, 53,  3,  7, 54, 27,  4, 38, 41,  8, 34, 55, 48, 28,
      62,  5, 39, 46, 44, 42, 22,  9, 24, 35, 59, 56, 49, 18, 29, 11,
      63, 52,  6, 26, 37, 40, 33, 47, 61, 45, 43, 21, 23, 58, 17, 10,
      51, 25, 36, 32, 60, 20, 57, 16, 50, 31, 19, 15, 30, 14, 13, 12 };
#endif
#elif HalfDeBruijn
    protected const UInt32 uDeBruijn = 0x077CB531U;
    // From the Paper: 0000 0111 0111 1100 1011 0101 0011 0001
#if !InitDeBruijn
    protected static readonly Byte[] deBruijnHalf =
    {  0,  1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
      31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9 };
#endif
#endif
    #endregion

    #region Bit Twiddles
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Boolean IsOneOrNone(Plane qp) {
      return (qp - 1 & qp) == 0;
    }
#if FindHi
    protected static sq sqHi(UInt64 r) {
      for (var n = 0; n < nSquares; n++, r <<= 1)
        if ((r & BITHI) != 0) return (sq)(63 - n);

      throw new ApplicationException("Empty Bit Mask");
    }
#endif                                  // FindHi
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLoExp(ref Byte r, out Byte s) {
      s = (Byte)(r & (~r + 1));         // s = r & -r to isolate lowest/first bit
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return singleBSF8(s);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLoExp(ref Byte r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      r ^= (Byte)s;                     // r = r & (r - 1) to subtract s from r
      return singleBSF8(s);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 singleBSF8(Int32 n) {
      if (n == 0) {
        Debug.Assert(n != 0, "No Bit Found");
        return -1;
      }
      var p = (Byte)(n * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r, out Byte s) {
      s = (Byte)(r & (~r + 1));         // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return -1;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = (Byte)(s * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return -1;
      }
      r ^= (Byte)s;                     // r = r & (r - 1) to subtract s from r
      var p = (Byte)(s * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }
#if ImportTwiddle
    [DllImport("twiddle.dll", EntryPoint = "?FindLo@Twiddle@CSquare@@SA?BH_K@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 BSF64(UInt64 r);

    [DllImport("twiddle.dll", EntryPoint = "?RemoveLo@Twiddle@CSquare@@SA?BHAA_K0@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 RemoveLo(ref UInt64 r, out UInt64 s);

    [DllImport("twiddle.dll", EntryPoint = "?RemoveLo@Twiddle@CSquare@@SA?BHAA_K@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 RemoveLo(ref UInt64 r);
#else                                   //!ImportTwiddle
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    // Bit Scan Forward, formerly known as FindLo()
    public static Int32 BSF64(UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      return singleBSF64(s);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLoExp(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return singleBSF64(s);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLoExp(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return singleBSF64(s);
    }
#if FullDeBruijn
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 singleBSF64(UInt64 q) {
      if (q == 0) {
        Debug.Assert(q != 0, "No Bit Found");
        return -1;
      }
      var p = q * qDeBruijn >> 64 - 6;
      return deBruijnFull[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return -1;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = s * qDeBruijn >> 64 - 6;
      return deBruijnFull[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return -1;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = s * qDeBruijn >> 64 - 6;
      return deBruijnFull[p];
    }
#elif HalfDeBruijn
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 singleBSF64(UInt64 q) {
      var u = (UInt32)q;                // Half de Bruijn: Avoiding 64-Bit Multiplication
      var n = 0;
      if (u == 0) {
        u = (UInt32)(q >> 32);
        n = 32;
      }
      return singleBSF32(u) + n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static int singleBSF32(UInt32 u) {
      if (u == 0) {
        Debug.Assert(u != 0, "No Bit Found");
        return -1;
      }
      var p = u * uDeBruijn >> 32 - 5;
      return deBruijnHalf[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      var u = (UInt32)s;                // Half de Bruijn: Avoiding 64-Bit Multiplication
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return -1;
        }
        n = 32;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = u * uDeBruijn >> 32 - 5;
      return deBruijnHalf[p] + n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      var u = (UInt32)s;                // Half de Bruijn: Avoiding 64-Bit Multiplication
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return -1;
        }
        n = 32;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = u * uDeBruijn >> 32 - 5;
      return deBruijnHalf[p] + n;
    }
#else                                   //!(FullDeBruijn || HalfDeBruijn)
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 singleBSF64(UInt64 q) {
      if (q == 0) {
        Debug.Assert(q != 0, "No Bit Found");
        return -1;
      }
      var n = 0;
      if ((q & 0xAAAAAAAAAAAAAAAA) != 0) n |= 1 << 0;
      if ((q & 0xCCCCCCCCCCCCCCCC) != 0) n |= 1 << 1;
      if ((q & 0xF0F0F0F0F0F0F0F0) != 0) n |= 1 << 2;
      if ((q & 0xFF00FF00FF00FF00) != 0) n |= 1 << 3;
      if ((q & 0xFFFF0000FFFF0000) != 0) n |= 1 << 4;
      if ((q & 0xFFFFFFFF00000000) != 0) n |= 1 << 5;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return -1;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var n = 0;
      if ((s & 0xAAAAAAAAAAAAAAAA) != 0) n |= 1 << 0;
      if ((s & 0xCCCCCCCCCCCCCCCC) != 0) n |= 1 << 1;
      if ((s & 0xF0F0F0F0F0F0F0F0) != 0) n |= 1 << 2;
      if ((s & 0xFF00FF00FF00FF00) != 0) n |= 1 << 3;
      if ((s & 0xFFFF0000FFFF0000) != 0) n |= 1 << 4;
      if ((s & 0xFFFFFFFF00000000) != 0) n |= 1 << 5;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return -1;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var n = 0;
      if ((s & 0xAAAAAAAAAAAAAAAA) != 0) n |= 1 << 0;
      if ((s & 0xCCCCCCCCCCCCCCCC) != 0) n |= 1 << 1;
      if ((s & 0xF0F0F0F0F0F0F0F0) != 0) n |= 1 << 2;
      if ((s & 0xFF00FF00FF00FF00) != 0) n |= 1 << 3;
      if ((s & 0xFFFF0000FFFF0000) != 0) n |= 1 << 4;
      if ((s & 0xFFFFFFFF00000000) != 0) n |= 1 << 5;
      return n;
    }
#endif                                  //!(FullDeBruijn || HalfDeBruijn)
#endif                                  //!ImportTwiddle
#if InitDeBruijn
    protected static Byte[] newDeBruijn(Int32 nLog) {
      Debug.Assert(nLog <= 8, $"nLog = {nLog} too large");
      var nLength = 1 << nLog;
      return new Byte[nLength];
    }
#endif                                  // InitDeBruijn
    [Conditional("InitDeBruijn")]
    protected static void loadDeBruijn(Byte[] deBruijnMap, Int32 nLog, UInt64 qDeBruijnNumber) {
      var nLength = 1 << nLog;
      Debug.Assert(
        deBruijnMap.Length == nLength,
        "Inconsistent Length",
        $"Length = {deBruijnMap.Length} != 1 << {nLog}");

      var vMask = (Byte)(nLength - 1);
      var m = qDeBruijnNumber;
      for (var n = 0; n < nLength; n++, m <<= 1) {
        var p = vMask & (m >> nLength - nLog);
        Debug.Assert(
          (Int32)p < deBruijnMap.Length,
          "Index Out of Range",
          $"Index = {p}, Length = {deBruijnMap.Length}");
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
#endif                                  // TestDeBruijn
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
        Console.WriteLine($"usqrt: n = {q}, root = {root}, count = {count}");
#endif
      return root;
    }
    #endregion

    #region Counter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected static void setTwoBits(ref PieceHashcode wTwoBitMask, Int32 nIndex, UInt32 u) {
      var bOverflow = u != twoBits(u);
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
      var bOverflow = u != nibble(u);
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
      return (Byte)nibble(wNibbleMask >> nIndex * nPerNibble);
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

    #region Nibble & TwoBits Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 nibble(Int32 input) {
      return input & vNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static UInt32 nibble(UInt32 input) {
      return input & vNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 twoBits(Int32 input) {
      return input & vTwoBits;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static UInt32 twoBits(UInt32 input) {
      return input & vTwoBits;
    }
    #endregion

    #region Shift Methods
    //
    //[C#]The << and >> operators treat negative exponents
    // as unsigned p-bit values, where p is the PBL of the
    // data type size.  The shift overloads implement more
    // intuitive semantics of additive, signed exponents:
    //
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Plane shiftl(Plane qp, Int32 n) {
      return n < 0 ? qp >> -n : qp << n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Plane shiftr(Plane qp, Int32 n) {
      return shiftl(qp, -n);
    }
    #endregion
  }
}
