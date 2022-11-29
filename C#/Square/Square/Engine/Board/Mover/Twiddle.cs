//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-02-09 CNHume]Created File
//
// Conditionals:
//
//#define FindHi
//#define ImportTwiddle
//
// RemoveLo() 47% faster w Half de Bruijn: Avoiding 64-Bit Multiply on a Compaq 3 GHz Pentium 4
//
// Overall search performance was better for Half de Bruijn on the Dell i7-4702HQ at 2.2 GHz w 4-cores
// x86 was 9.83% faster
// x64 was 6.48% faster
//
// Using i7-9700K CPU at 3.60GHz w 8-cores
//
// Overall Search Rate "8/8/8/6N1/8/7R/1K2PRn1/3q2k1 w - - 0 1" [16-ply]
// -------------------
// FullMask     [47:32.79] 764.538 KHz
// HalfDeBruijn [46:12.32] 786.73  KHz +2.9%
//
//[2022-04-22]
// Lazy Numerics Release
//
// StartPos
// Searched a total of 130,363,835 nodes in 41.536 sec, 3138.575 KHz +3.8352%
//
// Perft2
// Searched a total of 205,629,265 nodes in 54.275 sec, 3788.655 KHz +4.2266%
//
// Perft3
// Searched a total of 218,839,927 nodes in 91.324 sec, 2396.302 KHz +7.3387%
//
// Perft4
// Searched a total of 767,280,050 nodes in 237.195 sec, 3234.807 KHz +4.812%
//
// ===========================================================================
// HalfMask Release
//
// StartPos
// Searched a total of 130,363,835 nodes in 43.129 sec, 3022.649 KHz
//
// Perft2
// Searched a total of 205,629,265 nodes in 56.569 sec, 3635.017 KHz
//
// Perft3
// Searched a total of 218,839,927 nodes in 98.026 sec, 2232.468 KHz
//
// Perft4
// Searched a total of 767,280,050 nodes in 248.608 sec, 3086.305 KHz
//
//#define TestDeBruijn
//#define InitDeBruijn
//#define ByteDeBruijn
//#define DeBruijn                        // DeBruijn vs Mask
//#define FullData                        // Full vs Half
#define BitOperations

namespace Engine {
  using System;
  using System.Diagnostics;
  using System.Numerics;                // for BitOperations
  using System.Runtime.CompilerServices;// for MethodImplAttribute
#if ImportTwiddle
  using System.Runtime.InteropServices; // for [DllImport]
#endif
  using static System.Math;
#if TestDeBruijn
  using System.Text;

  using SortTest.Extensions;            // For AppendDelim()

  using static System.String;
#endif
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using ExtensionCounter = UInt16;
  using PieceHashcode = UInt16;         // 10 bits
  using Plane = UInt64;

  partial class Board {
    #region Constants
    private const Int32 nBit0 = 1;
    private const Int32 nBit1 = nBit0 << 1;
    private const Int32 nBit2 = nBit1 << 1;
    private const Int32 nBit3 = nBit2 << 1;
    private const Int32 nBit4 = nBit3 << 1;
    private const Int32 nBit5 = nBit4 << 1;
    private const Int32 nBit6 = nBit5 << 1;

    private const Byte vDeBruijn = 0x1D;    //[CNH]0001 1101
#if ByteDeBruijn && !InitDeBruijn
    private static readonly Byte[] deBruijnByte = { 0, 1, 6, 2, 7, 5, 4, 3 };
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
#if DeBruijn
#if FullData
    private const UInt64 qDeBruijn = 0x022FDD63CC95386DUL;
    // From http://www.chessprogramming.org/De_Bruijn_Sequence_Generator by Gerd Isenberg
#if !InitDeBruijn
    private static readonly Byte[] deBruijnFull =
    {  0,  1,  2, 53,  3,  7, 54, 27,  4, 38, 41,  8, 34, 55, 48, 28,
      62,  5, 39, 46, 44, 42, 22,  9, 24, 35, 59, 56, 49, 18, 29, 11,
      63, 52,  6, 26, 37, 40, 33, 47, 61, 45, 43, 21, 23, 58, 17, 10,
      51, 25, 36, 32, 60, 20, 57, 16, 50, 31, 19, 15, 30, 14, 13, 12 };
#endif
#else                                   //!FullData
    private const UInt32 uDeBruijn = 0x077CB531U;
    // From the Paper: 0000 0111 0111 1100 1011 0101 0011 0001
#if !InitDeBruijn
    private static readonly Byte[] deBruijnHalf =
    {  0,  1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
      31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9 };
#endif
#endif                                   // FullData
#endif                                   // DeBruijn
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
#if BitOperations
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    // Trailing Zero Count (TZC), formerly known as FindLo()
    private static Int32 TZC8(Byte r) {
      Debug.Assert(r != 0, "No Bit Found");
      return BitOperations.TrailingZeroCount(r);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r, out Byte s) {
      s = (Byte)(r & (~r + 1));         // s = r & -r to isolate lowest/first bit
      Debug.Assert(s != 0, "No Bit Found");
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return BitOperations.TrailingZeroCount(s);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r) {
      var s = (Byte)(r & (~r + 1));     // s = r & -r to isolate lowest/first bit
      Debug.Assert(s != 0, "No Bit Found");
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return BitOperations.TrailingZeroCount(s);
    }
#else
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    // Trailing Zero Count (TZC), formerly known as FindLo()
    private static Int32 TZC8(Byte r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      return TZC8Single((Byte)s);
    }
#if ByteDeBruijn                        // ByteDeBruijn
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 TZC8Single(Int32 n) {
      if (n == 0) {
        Debug.Assert(n != 0, "No Bit Found");
        return nBit5;
      }
      var p = (Byte)(n * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r, out Byte s) {
      s = (Byte)(r & (~r + 1));         // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit5;
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
        return nBit5;
      }
      r ^= (Byte)s;                     // r = r & (r - 1) to subtract s from r
      var p = (Byte)(s * vDeBruijn) >> 8 - 3;
      return deBruijnByte[p];
    }
#else                                   //!ByteDeBruijn
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 TZC8Single(UInt32 u) {
      if (u == 0) {
        Debug.Assert(u != 0, "No Bit Found");
        return nBit5;
      }
      var n = 0;
      if ((u & 0xAA) != 0) n |= nBit0;
      if ((u & 0xCC) != 0) n |= nBit1;
      if ((u & 0xF0) != 0) n |= nBit2;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r, out Byte s) {
      s = (Byte)(r & (~r + 1));         // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit5;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var n = 0;
      if ((s & 0xAA) != 0) n |= nBit0;
      if ((s & 0xCC) != 0) n |= nBit1;
      if ((s & 0xF0) != 0) n |= nBit2;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref Byte r) {
      var s = (Byte)(r & (~r + 1));     // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit5;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var n = 0;
      if ((s & 0xAA) != 0) n |= nBit0;
      if ((s & 0xCC) != 0) n |= nBit1;
      if ((s & 0xF0) != 0) n |= nBit2;
      return n;
    }
#endif                                  //!ByteDeBruijn
#endif                                  // BitOperations
#if ImportTwiddle
    [DllImport("twiddle.dll", EntryPoint = "?FindLo@Twiddle@CSquare@@SA?BH_K@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 TZC64(UInt64 r);

    [DllImport("twiddle.dll", EntryPoint = "?RemoveLo@Twiddle@CSquare@@SA?BHAA_K0@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 RemoveLo(ref UInt64 r, out UInt64 s);

    [DllImport("twiddle.dll", EntryPoint = "?RemoveLo@Twiddle@CSquare@@SA?BHAA_K@Z",
                CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Auto)]
    extern public static Int32 RemoveLo(ref UInt64 r);
#elif BitOperations
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    // Trailing Zero Count (TZC), formerly known as FindLo()
    public static Int32 TZC64(UInt64 r) {
      Debug.Assert(r != 0, "No Bit Found");
      return BitOperations.TrailingZeroCount(r);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      Debug.Assert(s != 0, "No Bit Found");
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return BitOperations.TrailingZeroCount(s);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      Debug.Assert(s != 0, "No Bit Found");
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      return BitOperations.TrailingZeroCount(s);
    }
#else                                   //!(ImportTwiddle || BitOperations)
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    // Trailing Zero Count (TZC), formerly known as FindLo()
    public static Int32 TZC64(UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      return TZC64Single(s);
    }
#if DeBruijn
#if FullData
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 TZC64Single(UInt64 q) {
      if (q == 0) {
        Debug.Assert(q != 0, "No Bit Found");
        return nBit6;
      }
      var p = q * qDeBruijn >> nBit6 - 6;
      return deBruijnFull[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit6;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = s * qDeBruijn >> nBit6 - 6;
      return deBruijnFull[p];
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit6;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = s * qDeBruijn >> nBit6 - 6;
      return deBruijnFull[p];
    }
#else                                   //!FullData
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 TZC64Single(UInt64 q) {
      var u = (UInt32)q;                // Half de Bruijn: Avoiding 64-Bit Multiply
      var n = 0;
      if (u == 0) {
        u = (UInt32)(q >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return nBit6;
        }
        n = nBit5;
      }
      var p = u * uDeBruijn >> nBit5 - 5;
      return deBruijnHalf[p] | n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      var u = (UInt32)s;                // Half de Bruijn: Avoiding 64-Bit Multiply
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return nBit6;
        }
        n = nBit5;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = u * uDeBruijn >> nBit5 - 5;
      return deBruijnHalf[p] | n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      var u = (UInt32)s;                // Half de Bruijn: Avoiding 64-Bit Multiply
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return nBit6;
        }
        n = nBit5;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var p = u * uDeBruijn >> nBit5 - 5;
      return deBruijnHalf[p] | n;
    }
#endif                                  // FullData
#else                                   //!DeBruijn
#if FullData
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 TZC64Single(UInt64 q) {
      if (q == 0) {
        Debug.Assert(q != 0, "No Bit Found");
        return nBit6;
      }
      var n = 0;
      if ((q & 0xAAAAAAAAAAAAAAAA) != 0) n |= nBit0;
      if ((q & 0xCCCCCCCCCCCCCCCC) != 0) n |= nBit1;
      if ((q & 0xF0F0F0F0F0F0F0F0) != 0) n |= nBit2;
      if ((q & 0xFF00FF00FF00FF00) != 0) n |= nBit3;
      if ((q & 0xFFFF0000FFFF0000) != 0) n |= nBit4;
      if ((q & 0xFFFFFFFF00000000) != 0) n |= nBit5;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit6;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var n = 0;
      if ((s & 0xAAAAAAAAAAAAAAAA) != 0) n |= nBit0;
      if ((s & 0xCCCCCCCCCCCCCCCC) != 0) n |= nBit1;
      if ((s & 0xF0F0F0F0F0F0F0F0) != 0) n |= nBit2;
      if ((s & 0xFF00FF00FF00FF00) != 0) n |= nBit3;
      if ((s & 0xFFFF0000FFFF0000) != 0) n |= nBit4;
      if ((s & 0xFFFFFFFF00000000) != 0) n |= nBit5;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      if (s == 0) {
        Debug.Assert(s != 0, "No Bit Found");
        return nBit6;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      var n = 0;
      if ((s & 0xAAAAAAAAAAAAAAAA) != 0) n |= nBit0;
      if ((s & 0xCCCCCCCCCCCCCCCC) != 0) n |= nBit1;
      if ((s & 0xF0F0F0F0F0F0F0F0) != 0) n |= nBit2;
      if ((s & 0xFF00FF00FF00FF00) != 0) n |= nBit3;
      if ((s & 0xFFFF0000FFFF0000) != 0) n |= nBit4;
      if ((s & 0xFFFFFFFF00000000) != 0) n |= nBit5;
      return n;
    }
#else                                   //!FullData
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Int32 TZC64Single(UInt64 q) {
      var u = (UInt32)q;                // Half Data: Avoiding 64-Bit Masks
      var n = 0;
      if (u == 0) {
        u = (UInt32)(q >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return nBit6;
        }
        n = nBit5;
      }
      if ((u & 0xAAAAAAAA) != 0) n |= nBit0;
      if ((u & 0xCCCCCCCC) != 0) n |= nBit1;
      if ((u & 0xF0F0F0F0) != 0) n |= nBit2;
      if ((u & 0xFF00FF00) != 0) n |= nBit3;
      if ((u & 0xFFFF0000) != 0) n |= nBit4;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r, out UInt64 s) {
      s = r & (~r + 1);                 // s = r & -r to isolate lowest/first bit
      var u = (UInt32)s;                // Half Data: Avoiding 64-Bit Masks
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return nBit6;
        }
        n = nBit5;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      if ((u & 0xAAAAAAAA) != 0) n |= nBit0;
      if ((u & 0xCCCCCCCC) != 0) n |= nBit1;
      if ((u & 0xF0F0F0F0) != 0) n |= nBit2;
      if ((u & 0xFF00FF00) != 0) n |= nBit3;
      if ((u & 0xFFFF0000) != 0) n |= nBit4;
      return n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 RemoveLo(ref UInt64 r) {
      var s = r & (~r + 1);             // s = r & -r to isolate lowest/first bit
      var u = (UInt32)s;                // Half Data: Avoiding 64-Bit Masks
      var n = 0;
      if (u == 0) {
        u = (UInt32)(s >> 32);
        if (u == 0) {
          Debug.Assert(u != 0, "No Bit Found");
          return nBit6;
        }
        n = nBit5;
      }
      r ^= s;                           // r = r & (r - 1) to subtract s from r
      if ((u & 0xAAAAAAAA) != 0) n |= nBit0;
      if ((u & 0xCCCCCCCC) != 0) n |= nBit1;
      if ((u & 0xF0F0F0F0) != 0) n |= nBit2;
      if ((u & 0xFF00FF00) != 0) n |= nBit3;
      if ((u & 0xFFFF0000) != 0) n |= nBit4;
      return n;
    }
#endif                                  // FullData
#endif                                  // DeBruijn
#endif                                  //!ImportTwiddle
#if InitDeBruijn
    private static Byte[] newDeBruijn(Int32 nLog) {
      Debug.Assert(nLog <= 8, $"nLog = {nLog} too large");
      var nLength = 1 << nLog;
      return new Byte[nLength];
    }
#endif                                  // InitDeBruijn
    [Conditional("InitDeBruijn")]
    private static void loadDeBruijn(Byte[] deBruijnMap, Int32 nLog, UInt64 qDeBruijnNumber) {
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
      var methodName = nameof(loadDeBruijn);
      var sb = new StringBuilder();
      for (var n = 0; n < nLength; n++) {
        sb.AppendDelim($"{deBruijnMap[n],2}");
      }
      LogLine($"{methodName}({nLog}): {sb}");
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
        LogLine($"usqrt: n = {q}, root = {root}, count = {count}");
#endif
      return root;
    }
    #endregion

    #region Counter Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static void setTwoBits(ref PieceHashcode wTwoBitMask, Int32 nIndex, UInt32 u) {
      var bOverflow = u != TwoBits(u);
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
    public static void SetNibble(ref ExtensionCounter wNibbleMask, Int32 nIndex, UInt32 u) {
      var bOverflow = u != Nibble(u);
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
    public static Byte GetNibble(ExtensionCounter wNibbleMask, Int32 nIndex) {
      return (Byte)Nibble(wNibbleMask >> nIndex * nPerNibble);
    }
    #endregion

    #region Nibble & TwoBits Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 Nibble(Int32 input) {
      return input & vNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static UInt32 Nibble(UInt32 input) {
      return input & vNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static Int32 TwoBits(Int32 input) {
      return input & vTwoBits;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public static UInt32 TwoBits(UInt32 input) {
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
    private static Plane shiftl(Plane qp, Int32 n) {
      return n < 0 ? qp >> -n : qp << n;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private static Plane shiftr(Plane qp, Int32 n) {
      return shiftl(qp, -n);
    }
    #endregion
  }
}
