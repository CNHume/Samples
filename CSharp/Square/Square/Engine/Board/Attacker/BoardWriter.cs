//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split Diagnostics into their own file
//
// Conditionals:
//
//#define Magic
#define TestRotation
//#define DisplayFEN
#define DisplayFlags
//#define DisplayCounts
//#define DisplayPieceHash

namespace Engine {
  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  using static Logging.Logger;
  using static System.String;

  //
  // Type Aliases:
  //
  using Hashcode = System.UInt64;
  using Plane = System.UInt64;

  partial class Board {
    #region Constants
    public const char cSpace = ' ';
    public const char cFileMin = 'a';
    public const char cRankMin = '1';
    public const char cFileMax = (char)(cFileMin + nFiles - 1);
    public const char cRankMax = (char)(cRankMin + nRanks - 1);

    internal const Int16 mWrapLength = 144;
    internal const String sSpace = " ";
    internal const String sSpace2 = sSpace + sSpace;
    internal const String sSpace3 = sSpace + sSpace2;
    internal const Char cVacant = '-';
    internal const Char cOccupied = 'X';
    internal const Char cNewline = '\n';

    internal static LoFlags[] loFlags =
      { LoFlags.Final, LoFlags.InCheck, LoFlags.Illegal, LoFlags.WTM };

    internal static HiFlags[] hiFlags =
      { HiFlags.Dark, HiFlags.Lite, HiFlags.CanOOO, HiFlags.CanOO };

    internal static EGFlags[] egFlags =
      { EGFlags.KQvKP, EGFlags.KBNvK, EGFlags.OutsideSquare, EGFlags.BlackAlone, EGFlags.WhiteAlone };

    internal static DrawFlags[] drFlags =
      { DrawFlags.DrawIM, DrawFlags.Draw50, DrawFlags.Draw3, DrawFlags.Draw2, DrawFlags.Fence };

    internal static ModeFlags[] mdFlags =
      { ModeFlags.Trace, ModeFlags.Reduced, ModeFlags.ZWS, ModeFlags.NullMade };
    #endregion

    #region Flag Diagnostics
    public static String FormatFlags(ModeFlags fmd) {
      var en = mdFlags.Where(f => (f & fmd) != 0);
      return Join(sSpace, en);
    }

    public static String FormatFlags(DrawFlags fdr) {
      var en = drFlags.Where(f => (f & fdr) != 0);
      return Join(sSpace, en);
    }

    public static String FormatFlags(EGFlags feg) {
      var en = egFlags.Where(f => (f & feg) != 0);
      return Join(sSpace, en);
    }

    public static String FormatFlags(HiFlags fhi) {
      var en = hiFlags.Where(f => (f & fhi) != 0);
      return Join(sSpace, en);
    }

    public static String FormatFlags(LoFlags flo) {
      var en = loFlags.Where(f => (f & flo) != 0);
      var s = Join(sSpace, en);

      if ((flo & LoFlags.Passed) != 0) {
        var sPrefix = IsNullOrEmpty(s) ? Empty : sSpace;
        s += sPrefix + (sq)ep(flo) + sSpace + LoFlags.Passed;
      }

      return s;
    }

    public static String FormatFlags(ModeFlags fmd, DrawFlags fdr, EGFlags feg, HiFlags fBlackHi, HiFlags fWhiteHi, LoFlags flo) {
      if (fmd == 0 && fdr == 0 && feg == 0 && fBlackHi == 0 && fWhiteHi == 0 && flo == 0)
        return "None";

      var sBlackHi = FormatFlags(fBlackHi);
      var sWhiteHi = FormatFlags(fWhiteHi);
      var sBlackHiLabelled = IsNullOrEmpty(sBlackHi) ? Empty : $"Black[{sBlackHi}]";
      var sWhiteHiLabelled = IsNullOrEmpty(sWhiteHi) ? Empty : $"White[{sWhiteHi}]";

      const Int32 nCapacity = 4;
      var sFlags = new List<String>(nCapacity)
        .AddNotEmpty(FormatFlags(fmd))
        .AddNotEmpty(FormatFlags(fdr))
        .AddNotEmpty(FormatFlags(feg))
        .AddNotEmpty(sBlackHiLabelled)
        .AddNotEmpty(sWhiteHiLabelled)
        .AddNotEmpty(FormatFlags(flo));

      return Join(sSpace, sFlags);
    }
    #endregion

    #region Radix Conversion
    protected static String formatHash(Hashcode qHash) {
      return Convert.ToString((Int64)qHash, 16).ToUpper().PadLeft(16, '0');
    }

    private static String formatBinary(UInt32 u, Int32 nWidth) {
      return Convert.ToString(u, 2).PadLeft(nWidth, '0');
    }

    private static void writeBinary(String sValue, UInt32 u, Int32 nWidth) {
      LogLine($"{sValue} = {formatBinary(u, nWidth)}");
    }
    #endregion

    #region ToString() Methods
    protected void appendPositionPieces(StringBuilder sb) {
      var nSkip = 0;

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);

        if (y > 0)
          sb.Append("/");

        var qp = BIT0 << sqr(0, yInverse);
        for (var x = 0; x < nFiles; x++, qp <<= 1) {
          var vPiece = getPiece(sqr(x, yInverse));

          if (vPiece > vK6)
            nSkip++;
          else {
            if (nSkip > 0) {
              sb.Append(nSkip);
              nSkip = 0;
            }

            var piece = (Piece)(vPiece + vFirst);
            var sPiece = piece.ToString();
            sb.Append((qp & Side[White].Piece) == 0 ? sPiece.ToLower() : sPiece.ToUpper());
          }
        }

        if (nSkip > 0) {
          sb.Append(nSkip);
          nSkip = 0;
        }
      }
    }

    protected void appendPosition(StringBuilder sb) {
      appendPositionPieces(sb);

      var sSideToMove = WTM() ? " w " : " b ";
      sb.Append(sSideToMove).AppendCastleRights(Side[Black].FlagsHi, Side[White].FlagsHi, State.Rule);

      if (!IsPassed())
        sb.Append(" -");
      else {
        sb.Append(sSpace);
        sb.Append((sq)ep(FlagsLo));     // EP
      }
    }

    public String ToString(PositionType type = PositionType.FEN) {
      var sb = new StringBuilder();
      appendPosition(sb);

      switch (type) {
      case PositionType.Prefix:
        break;
      case PositionType.FEN:
        var nMove = GamePly / 2 + 1;
        sb.Append(sSpace)
          .Append(HalfMoveClock)        // Half-Move Clock for the 50-Move Rule
          .Append(sSpace)
          .Append(nMove);               // Full Move Number
        break;
      case PositionType.EPD:
        if (Operations is null) newOperations();
        sb.AppendOperations(Operations);
        break;
      }

      return sb.ToString();
    }
    #endregion

    #region Board Display
    public static String PieceSymbol(Byte vPiece) {
      var piece = (Piece)(vPiece + vFirst);
      var sPiece = IsNullOrEmpty(PieceSymbols) ?
        piece.ToString() : PieceSymbols[vPiece].ToString();
      return sPiece;
    }

    private void appendPiece1(StringBuilder sb, Int32 n, Plane qp) {
      const String sLite = "-";
      const String sDark = "*";
      var vPiece = getPiece(n);
      if (vPiece > vK6)
        sb.Append((LiteSquare & qp) != 0 ? sLite : sDark);
      else
        sb.Append(PieceSymbol(vPiece));
    }

    private void append960(StringBuilder sb, Boolean bFlip = false) {
      const Int32 rank = 0;
      var qp = BIT0 << sqr(0, rank);
      for (var x = 0; x < nFiles; x++, qp <<= 1) {
        var file = bFlip ? invertFile(x) : x;
        appendPiece1(sb, sqr(file, rank), qp);
      }
    }

    public String PositionSetup(Boolean bFlip = false) {
      var sb = new StringBuilder();
      append960(sb, bFlip);
      return sb.ToString();
    }

    private void appendPiece2(StringBuilder sb, Int32 n, Plane qp) {
      const String sLite = "--";
      const String sDark = "**";
      var vPiece = getPiece(n);
      if (vPiece > vK6)
        sb.Append((LiteSquare & qp) != 0 ? sLite : sDark);
      else {
        sb.Append((Side[White].Piece & qp) != 0 ?
          Parameter[White].Symbol : Parameter[Black].Symbol);
        sb.Append(PieceSymbol(vPiece));
      }
    }

    private void appendRank(StringBuilder sb, Int32 rank, Boolean bFlip = false) {
      var bRightRuler = bFlip;
      var qp = BIT0 << sqr(0, rank);
      for (var x = 0; x < nFiles; x++, qp <<= 1) {
        var file = bFlip ? invertFile(x) : x;
        if (!bRightRuler)               // Left Pad
          sb.Append(sSpace);
        appendPiece2(sb, sqr(file, rank), qp);
        if (bRightRuler)                // Right Pad
          sb.Append(sSpace);
      }
    }

    protected void appendBoard(StringBuilder sb, Boolean bFlip = false) {
      var bTopRuler = bFlip;
      var bRightRuler = bFlip;
      if (bTopRuler)                    // File Ruler at Top
        sb.Append(sSpace)
          .AppendFiles(sSpace2, bFlip)
          .Append(cNewline);
      for (var y = 0; y < nRanks; y++) {
        var rank = bFlip ? y : invertRank(y);
        var c = (Char)(cRankMin + rank);
        if (!bRightRuler)               // Rank Ruler at Left
          sb.Append(c);
        appendRank(sb, rank, bFlip);
        if (bRightRuler)                // Rank Ruler at Right
          sb.Append(c);
        sb.Append(cNewline);
      }
      if (!bTopRuler)                   // File Ruler at Bottom
        sb.AppendFiles(sSpace2, bFlip)
          .Append(cNewline);
    }

    protected void appendProperties(StringBuilder sb) {
      sb.Append("Hashcode = ").Append(formatHash(Hash))
#if DisplayFEN
        .Append("; FEN = ").AppendLine(ToString(PositionType.FEN))
#else
        .Append("; EPD = ").AppendLine(ToString(PositionType.EPD))
#endif
#if DisplayFlags
        .Append("Flags: ").AppendLine(FormatFlags(FlagsMode, FlagsDraw, FlagsEG, Side[Black].FlagsHi, Side[White].FlagsHi, FlagsLo))
#else
        .AppendLine($"{getSide(WTM()).SideName} to Move")
#endif
#if DisplayCounts
        .AppendLine()
        .AppendPieceCounts(Side[White].Counts, Side[Black].Counts)
#endif
#if DisplayPieceHash
        .AppendLine()
        .AppendPieceHash(Side[White].PieceHash, Side[Black].PieceHash)
#endif
        .AppendLine();
    }

    public StringBuilder Display(StringBuilder sb = default) {
      if (sb is null) sb = new StringBuilder();
      appendProperties(sb);
      appendBoard(sb, State.IsFlip);
      return sb.FlushLine();
    }

    public StringBuilder Display(String sLabel) {
      var sb = new StringBuilder();
      if (!IsNullOrEmpty(sLabel))
        sb.AppendLine(sLabel);
      return Display(sb);
    }
    #endregion

    #region Board Diagnostics
    protected static void printMapping(String sLabel, Func<Int32, Int32> mapper) {
      var sb = new StringBuilder();
      sb.Append(sLabel);
      sb.Append(cNewline);
      sb.Append(cNewline);

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);
        var s = Empty;
        for (var x = 0; x < nFiles; x++)
          sb.Append($"{mapper(sqr(x, yInverse)),3}");
        sb.Append(cNewline);
      }
      sb.FlushLine();
    }

    protected static void printSquares(String sLabel, Byte[] map) {
      printMapping(sLabel, n => map[n]);
    }

    protected static void writeRect(Plane qp, Boolean bRotateBoard = false, Boolean bFlip = false) {
      var sb = new StringBuilder();
      var bTopRuler = bFlip;
      var bRightRuler = bFlip;
      if (bTopRuler)                    // File Ruler at Top
        sb.AppendRuler(bRotateBoard, bFlip);
      for (var y = 0; y < nRanks; y++) {
        var rank = bFlip ? y : invertRank(y);
        var c = (Char)(bRotateBoard ? cFileMax - rank : cRankMin + rank);
        if (!bRightRuler)               // Rank Ruler at Left
          sb.Append(c);
        var uRect = (UInt32)(qp >> sqr(0, rank) & Byte.MaxValue);
        sb.AppendRect(uRect, bFlip);
        if (bRightRuler)                // Rank Ruler at Right
          sb.Append(c);
        sb.Append(cNewline);
      }
      if (!bTopRuler)                   // File Ruler at Bottom
        sb.AppendRuler(bRotateBoard, bFlip);
      sb.FlushLine();
    }

    protected static void testRect(String sLabel, Plane qp, Boolean bRotateBoard = false, Boolean bFlip = false) {
      LogLine($"{sLabel}");
      LogLine();
      writeRect(qp, bRotateBoard, bFlip);
    }

    protected static void writeDiagIndexes(Func<Int32, Int32> idxr) {
      var sb = new StringBuilder();
      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);
        for (var x = 0; x < nFiles; x++)
          sb.Append($" {idxr(sqr(x, yInverse)),2}");
        sb.Append(cNewline);
      }
      sb.FlushLine();
    }

    protected static void testDiagIndexes(String sLabel, Func<Int32, Int32> idxr) {
      LogLine(sLabel);
      LogLine();
      writeDiagIndexes(idxr);
    }

    public static Int32 invertFile(Int32 x) {
      return nFiles - (x + 1);
    }

    public static Int32 invertRank(Int32 y) {
      return nRanks - (y + 1);
    }
    #endregion

    #region Rotation Diagnostics
    protected static Int32 invertDiag(Int32 d) {
      return nDiagonals - (d + 1);
    }

    private static char ruler(Int32 d, Boolean bRotateBoard) {
      var dInverse = invertDiag(d);
      var c = cSpace;
      if (dInverse < nFiles - 1)
        c = (Char)(bRotateBoard ? cFileMin + dInverse + 1 : cFileMax - (dInverse + 1));
      else if (dInverse > nFiles - 1)
        c = (Char)(cRankMin + dInverse - nFiles);
      return c;
    }
#if TestRotation && !Magic
    public static void writeDiag(Plane qp, Boolean bRotateBoard = false) {
      var sb = new StringBuilder();
      sb.AppendIndent(8)
        .Append(ruler(-1, bRotateBoard))
        .Append(cNewline);
      for (var d = 0; d < nDiagonals; d++) {
        var dInverse = invertDiag(d);
        var nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
        var vDiagMask = (Byte)((1 << nDiagLen) - 1);
        var uDiag = (UInt32)(qp >> DiagOffset[dInverse] & vDiagMask);
        sb.AppendIndent(8 - nDiagLen)
          .Append(ruler(d, bRotateBoard))
          .AppendDiag(nDiagLen, uDiag)
          .Append(cNewline);
      }
      sb.AppendIndent(8)
        .Append(ruler(nDiagonals, bRotateBoard))
        .Append(cNewline)
        .FlushLine();
    }

    protected static void testDiag(String sLabel, Plane qp, bool bRotateBoard = false) {
      LogLine($"{sLabel}");
      LogLine();
      writeDiag(qp, bRotateBoard);
    }

    protected static void writeRectRotations(String sLabel, Plane[] qpRect) {
      var sb = new StringBuilder();
      sb.Append(sLabel);
      sb.Append(cNewline);
      sb.Append(cNewline);

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);
        sb.AppendRectRotations(qpRect, BIT0 << sqr(0, yInverse)).Append(cNewline);
      }
      sb.FlushLine();
    }

    protected static void writeDiagRotations(String sLabel, Plane[] qpDiag) {
      var sb = new StringBuilder();
      sb.Append(sLabel);
      sb.Append(cNewline);
      sb.Append(cNewline);

      for (var d = 0; d < nDiagonals; d++) {
        var dInverse = invertDiag(d);
        var nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
        sb.AppendIndent(8 - nDiagLen).AppendDiagRotations(nDiagLen, qpDiag, BIT0 << DiagOffset[dInverse]).Append(cNewline);
      }
      sb.FlushLine();
    }
#endif
    #endregion
  }
}
