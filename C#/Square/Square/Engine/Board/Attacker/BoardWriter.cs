﻿//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  using static Logging.Logger;
  using static System.String;

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;
  using Plane = UInt64;

  partial class Board {
    #region Constants
    public const Char cSpace = ' ';
    public const Char cFileMin = 'a';
    public const Char cRankMin = '1';
    public const Char cFileMax = (Char)(cFileMin + nFiles - 1);
    public const Char cRankMax = (Char)(cRankMin + nRanks - 1);

    internal const Int16 mWrapLength = 144;

    internal const Char cVacant = '-';
    internal const Char cOccupied = 'X';
    internal const Char cNewline = '\n';

    internal const String sSpace = " ";
    internal const String sSpace2 = sSpace + sSpace;
    internal const String sSpace3 = sSpace + sSpace2;

    internal static TurnFlags[] turnFlags =
      { TurnFlags.Final, TurnFlags.InCheck, TurnFlags.Illegal, TurnFlags.WTM };

    internal static SideFlags[] sideFlags =
      { SideFlags.Insufficient, SideFlags.Dark, SideFlags.Lite, SideFlags.CanOOO, SideFlags.CanOO };

    internal static GameFlags[] gameFlags =
      { GameFlags.KQvKP, GameFlags.KBNvK, GameFlags.OutsideSquare, GameFlags.BlackAlone, GameFlags.WhiteAlone };

    internal static DrawFlags[] drawFlags =
      { DrawFlags.DrawIM, DrawFlags.Draw50, DrawFlags.Draw3, DrawFlags.Draw2, DrawFlags.Draw0 };

    internal static ModeFlags[] modeFlags =
      { ModeFlags.Trace, ModeFlags.Reduced, ModeFlags.ZWS, ModeFlags.NullMade };
    #endregion

    #region Flag Diagnostics
    public static String FormatFlags(ModeFlags fmode) {
      var en = modeFlags.Where(f => fmode.Has(f));
      return Join(sSpace, en);
    }

    public static String FormatFlags(DrawFlags fdraw) {
      var en = drawFlags.Where(f => fdraw.Has(f));
      return Join(sSpace, en);
    }

    public static String FormatFlags(GameFlags fgame) {
      var en = gameFlags.Where(f => fgame.Has(f));
      return Join(sSpace, en);
    }

    public static String FormatFlags(SideFlags fside) {
      var en = sideFlags.Where(f => fside.Has(f));
      return Join(sSpace, en);
    }

    public static String FormatFlags(TurnFlags fturn) {
      var en = turnFlags.Where(f => fturn.Has(f));
      var s = Join(sSpace, en);

      if (fturn.Has(TurnFlags.Passed)) {
        var sPrefix = IsNullOrEmpty(s) ? Empty : sSpace;
        s += sPrefix + (sq)ep(fturn) + sSpace + TurnFlags.Passed;
      }

      return s;
    }

    public static String FormatFlags(
      ModeFlags fmode, DrawFlags fdraw, GameFlags fgame, SideFlags fBlackSide, SideFlags fWhiteSide, TurnFlags fturn) {
      if (fmode == 0 && fdraw == 0 && fgame == 0 && fBlackSide == 0 && fWhiteSide == 0 && fturn == 0)
        return "None";

      var sBlackSide = FormatFlags(fBlackSide);
      var sWhiteSide = FormatFlags(fWhiteSide);
      var sBlackSideLabelled = IsNullOrEmpty(sBlackSide) ? Empty : $"Black[{sBlackSide}]";
      var sWhiteSideLabelled = IsNullOrEmpty(sWhiteSide) ? Empty : $"White[{sWhiteSide}]";

      const Int32 nCapacity = 6;
      var sFlags = new List<String>(nCapacity)
        .AddNotEmpty(FormatFlags(fmode))
        .AddNotEmpty(FormatFlags(fdraw))
        .AddNotEmpty(FormatFlags(fgame))
        .AddNotEmpty(sBlackSideLabelled)
        .AddNotEmpty(sWhiteSideLabelled)
        .AddNotEmpty(FormatFlags(fturn));

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
    private static string coloredPiece(Boolean bWhite, Byte vPiece) {
      var sPiece = indexPiece(vPiece).ToString();
      return bWhite ? sPiece.ToUpper() : sPiece.ToLower();
    }

    protected void appendPositionPieces(StringBuilder sb) {
      var nSkip = 0;

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);

        if (y > 0)
          sb.Append("/");

        var qp = BIT0 << sqr(0, yInverse);
        for (var x = 0; x < nFiles; x++, qp <<= 1) {
          var vPiece = getPieceIndex(sqr(x, yInverse));

          if (vPiece > vK6)
            nSkip++;
          else {
            if (nSkip > 0) {
              sb.Append(nSkip);
              nSkip = 0;
            }

            var bWhite = (qp & Side[White].Piece) != 0;
            sb.Append(coloredPiece(bWhite, vPiece));
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
      sb.Append(sSideToMove).AppendCastleRights(Side, State.IsChess960);

      if (!IsPassed())
        sb.Append(" -");
      else {
        sb.Append(sSpace);
        sb.Append((sq)ep(FlagsTurn));   // EP
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
      var sSymbol = IsNullOrEmpty(PieceSymbols) ?
        indexPiece(vPiece).ToString() : PieceSymbols[vPiece].ToString();
      return sSymbol;
    }

    private void appendPiece1(StringBuilder sb, Int32 n, Plane qp) {
      const String sLite = "-";
      const String sDark = "*";
      var vPiece = getPieceIndex(n);
      if (vPiece > vK6)
        sb.Append((qp & LiteSquare) != 0 ? sLite : sDark);
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

      var vPiece = getPieceIndex(n);
      if (vPiece > vK6)
        sb.Append((qp & LiteSquare) != 0 ? sLite : sDark);
      else {
        var sBlack = Parameter[Black].Symbol;
        var sWhite = Parameter[White].Symbol;
        var bWhite = (qp & Side[White].Piece) != 0;
        sb.Append(bWhite ? sWhite : sBlack);
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
      var fBlackSide = Side[Black].FlagsSide;
      var fWhiteSide = Side[White].FlagsSide;

      sb.Append("Hashcode = ").Append(formatHash(Hash))
#if DisplayFEN
        .Append("; FEN = ").AppendLine(ToString(PositionType.FEN))
#else
        .Append("; EPD = ").AppendLine(ToString(PositionType.EPD))
#endif
#if DisplayFlags
        .Append("Flags: ")
        .AppendLine(FormatFlags(FlagsMode, FlagsDraw, FlagsGame, fBlackSide, fWhiteSide, FlagsTurn))
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

    public StringBuilder Display(StringBuilder? sb = default) {
      if (sb is null) sb = new StringBuilder();
      appendProperties(sb);
      appendBoard(sb, State.IsFlip);
      return sb.FlushLine();
    }

    public StringBuilder Display(String? sLabel) {
      var sb = new StringBuilder();
      if (!IsNullOrEmpty(sLabel)) sb.AppendLine(sLabel);
      return Display(sb);
    }
    #endregion

    #region Board Diagnostics
    protected static void printMapping(String sLabel, Func<Int32, Int32> mapper) {
      var sb = new StringBuilder();
      sb.Append(sLabel)
        .Append(cNewline)
        .Append(cNewline);

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
    #endregion

    #region Rotation Diagnostics
    private static Char ruler(Int32 d, Boolean bRotateBoard) {
      var c = cSpace;
      var dInverse = invertDiag(d);
      if (dInverse < nFiles - 1)
        c = (Char)(bRotateBoard ?
                   cFileMin + dInverse + 1 :
                   cFileMax - (dInverse + 1));
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
      sb.Append(sLabel)
        .Append(cNewline)
        .Append(cNewline);

      for (var y = 0; y < nRanks; y++) {
        var yInverse = invertRank(y);
        sb.AppendRectRotations(qpRect, BIT0 << sqr(0, yInverse))
          .Append(cNewline);
      }

      sb.FlushLine();
    }

    protected static void writeDiagRotations(String sLabel, Plane[] qpDiag) {
      var sb = new StringBuilder();
      sb.Append(sLabel)
        .Append(cNewline)
        .Append(cNewline);

      for (var d = 0; d < nDiagonals; d++) {
        var dInverse = invertDiag(d);
        var nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
        sb.AppendIndent(8 - nDiagLen)
          .AppendDiagRotations(nDiagLen, qpDiag, BIT0 << DiagOffset[dInverse])
          .Append(cNewline);
      }

      sb.FlushLine();
    }
#endif
    #endregion
  }
}