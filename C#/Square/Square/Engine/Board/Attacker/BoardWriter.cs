//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
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

using System.Text;

namespace Engine;

using Exceptions;

using static System.String;
using static Logging.Logger;

//
// Type Aliases:
//
using Hashcode = UInt64;
using Plane = UInt64;

partial class Board {
  #region Constants
  internal const Int16 mWrapLength = 144;

  internal const Char cSemi = ';';
  internal const Char cSpace = ' ';
  internal const Char cMinus = '-';

  internal const Char cVacant = '-';
  internal const Char cOccupied = 'X';
  internal const Char cNewline = '\n';

  internal const Char cFileMin = 'a';
  internal const Char cRankMin = '1';

  private const Char cFileMax = (Char)(cFileMin + nFiles - 1);
  private const Char cRankMax = (Char)(cRankMin + nRanks - 1);

  private const Char cSlash = '/';

  private const Char cLite = '-';
  private const Char cDark = '*';

  private const String sLite = "--";
  private const String sDark = "**";

  internal const String sSpace = " ";
  internal const String sSpace2 = sSpace + sSpace;
  internal const String sSpace3 = sSpace + sSpace2;

  internal static TurnFlags[] turnFlags =
    { TurnFlags.Final, TurnFlags.InCheck, TurnFlags.Illegal, TurnFlags.EPLegal };

  internal static SideFlags[] sideFlags = {
      SideFlags.Insufficient, SideFlags.Alone,
      SideFlags.Lite, SideFlags.Dark,
      SideFlags.CanOOO, SideFlags.CanOO };

  internal static EvalFlags[] gameFlags =
    { EvalFlags.KQvKP, EvalFlags.KBNvK, EvalFlags.OutsideSquare };

  internal static DrawFlags[] drawFlags = {
      DrawFlags.DrawIM, DrawFlags.Draw50,
      DrawFlags.Draw3, DrawFlags.Draw2, DrawFlags.Draw0 };

  internal static ModeFlags[] modeFlags =
    { ModeFlags.Trace, ModeFlags.Reduced, ModeFlags.ZWS, ModeFlags.NullMade };
  #endregion

  #region Flag Diagnostics
  private static String formatFlags(ModeFlags fmode) {
    var en = modeFlags.Where(f => fmode.Has(f));
    return Join(sSpace, en);
  }

  private static String formatFlags(DrawFlags fdraw) {
    var en = drawFlags.Where(f => fdraw.Has(f));
    return Join(sSpace, en);
  }

  private static String formatFlags(EvalFlags feval) {
    var en = gameFlags.Where(f => feval.Has(f));
    return Join(sSpace, en);
  }

  private static String formatFlags(SideFlags fside) {
    var en = sideFlags.Where(f => fside.Has(f));
    return Join(sSpace, en);
  }

  private static String formatFlags(TurnFlags fturn) {
    var en = turnFlags.Where(f => fturn.Has(f));
    return Join(sSpace, en);
  }

  private static String formatState(
    ModeFlags fmode,
    DrawFlags fdraw,
    EvalFlags feval,
    SideFlags fBlackSide,
    SideFlags fWhiteSide,
    TurnFlags fturn,
    Boolean bWTM) {
    if (fmode == 0 &&
        fdraw == 0 &&
        feval == 0 &&
        fBlackSide == 0 &&
        fWhiteSide == 0 &&
        fturn == 0)
      return "None";

    var sBlackSide = formatFlags(fBlackSide);
    var sWhiteSide = formatFlags(fWhiteSide);
    var sBlackSideLabelled = IsNullOrEmpty(sBlackSide) ? Empty : $"Black[{sBlackSide}]";
    var sWhiteSideLabelled = IsNullOrEmpty(sWhiteSide) ? Empty : $"White[{sWhiteSide}]";

    const Int32 nCapacity = 8;
    var sFlags = new List<String>(nCapacity)
      .AddNotEmpty(formatFlags(fmode))
      .AddNotEmpty(formatFlags(fdraw))
      .AddNotEmpty(formatFlags(feval))
      .AddNotEmpty(sBlackSideLabelled)
      .AddNotEmpty(sWhiteSideLabelled)
      .AddNotEmpty(formatFlags(fturn));

    if (bWTM) sFlags.Add("WTM");

    return Join(sSpace, sFlags);
  }
  #endregion

  #region Radix Conversion
  private static String formatHash(Hashcode qHash) {
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
    var sPiece = IndexPiece(vPiece).ToString();
    return bWhite ? sPiece.ToUpper() : sPiece.ToLower();
  }

  private void appendPositionPieces(StringBuilder sb) {
    var whiteSide = Side[White];
    var nSkip = 0;
    for (var y = 0; y < nRanks; y++) {
      var yInverse = InvertRank(y);

      if (y > 0)
        sb.Append(cSlash);

      var qp = bit(sqr(0, yInverse));
      for (var x = 0; x < nFiles; x++, qp <<= 1) {
        var vPiece = GetPieceIndex(sqr(x, yInverse));

        if (vPiece > vK6)
          nSkip++;
        else {
          if (nSkip > 0) {
            sb.Append(nSkip);
            nSkip = 0;
          }

          var bWhite = (qp & whiteSide.Piece) != 0;
          sb.Append(coloredPiece(bWhite, vPiece));
        }
      }

      if (nSkip > 0) {
        sb.Append(nSkip);
        nSkip = 0;
      }
    }
  }

  private void appendPosition(StringBuilder sb) {
    appendPositionPieces(sb);

    sb.Append(cSpace)
      .Append(WTM() ? 'w' : 'b')
      .Append(cSpace)
      .AppendCastlingRights(Side, State.IsChess960)
      .Append(cSpace);

    //
    // If EPTarget.HasValue serialize its corresponding Sq into the FEN or EPD:
    //
    var sq = (Sq?)EPTarget;
    if (sq.HasValue)
      sb.Append(sq.Value);
    else
      sb.Append(cMinus);
  }

  public String ToString(PositionType type = PositionType.FEN) {
    var sb = new StringBuilder();
    appendPosition(sb);

    switch (type) {
    case PositionType.Prefix:
      break;
    case PositionType.FEN:
      sb.Append(cSpace)
        .Append(HalfMoveClock)          // Half-Move Clock for the 50-Move Rule
        .Append(cSpace)
        .Append(MoveNumber(GamePly));   // Full Move Number
      break;
    case PositionType.EPD:
      //[Note]Position may have resulted from ParseFEN() rather than ParseEPD()
      ensureOperations();
      sb.AppendOperations(Operations);
      break;
    }

    return sb.ToString();
  }

  protected Boolean VerifyFEN(String sFEN) {
    // Output FEN:
    var nFENLength = sFEN.Length;
    var sFEN2 = ToString(PositionType.FEN);
    // Input FEN abbreviations should match Output FEN Prefix:
    var sFEN2Prefix = nFENLength < sFEN2.Length ?
      sFEN2.Substring(0, nFENLength) : sFEN2;

    return sFEN == sFEN2Prefix;
  }
  #endregion

  #region Board Display
  public static String PieceSymbol(Byte vPiece) {
    var sSymbol = IsNullOrEmpty(PieceSymbols) ?
      IndexPiece(vPiece).ToString() : PieceSymbols[vPiece].ToString();
    return sSymbol;
  }

  private Position.PositionSide findSide(Plane qp) {
    foreach (var side in Side) {
      if ((qp & side.Piece) != 0)
        return side;
    }

    var sb = new StringBuilder("Side not found for")
      .AppendSquares(qp);
    throw new ColorException(sb.ToString());
  }

  private void appendPiece2(StringBuilder sb, Int32 n, Plane qp) {
    var vPiece = GetPieceIndex(n);
    if (vPiece > vK6) {
      var sColor = (qp & SquareLite) != 0 ? sLite : sDark;
      sb.Append(sColor);
    }
    else {
      var side = findSide(qp);
      sb.Append(side.Parameter.Symbol)
        .Append(PieceSymbol(vPiece));
    }
  }

  private void appendPiece1(StringBuilder sb, Int32 n, Plane qp) {
    var vPiece = GetPieceIndex(n);
    if (vPiece > vK6) {
      var cColor = (qp & SquareLite) != 0 ? cLite : cDark;
      sb.Append(cColor);
    }
    else
      sb.Append(PieceSymbol(vPiece));
  }

  private void append960(StringBuilder sb, Boolean bReflect = false) {
    const Int32 rank = 0;
    var qp = bit(sqr(0, rank));
    for (var x = 0; x < nFiles; x++, qp <<= 1) {
      var file = bReflect ? InvertFile(x) : x;
      appendPiece1(sb, sqr(file, rank), qp);
    }
  }

  public String PositionSetup(Boolean bReflect = false) {
    var sb = new StringBuilder();
    append960(sb, bReflect);
    return sb.ToString();
  }

  private void appendRank(StringBuilder sb, Int32 rank, Boolean bFlip = false) {
    var bRightRuler = bFlip;
    var qp = bit(sqr(0, rank));
    for (var x = 0; x < nFiles; x++, qp <<= 1) {
      var file = bFlip ? InvertFile(x) : x;
      if (!bRightRuler)                 // Left Pad
        sb.Append(cSpace);
      appendPiece2(sb, sqr(file, rank), qp);
      if (bRightRuler)                  // Right Pad
        sb.Append(cSpace);
    }
  }

  private void appendBoard(StringBuilder sb, Boolean bFlip = false) {
    var bTopRuler = bFlip;
    var bRightRuler = bFlip;
    if (bTopRuler)                      // File Ruler at Top
      sb.Append(cSpace)
        .AppendFiles(sSpace2, bFlip)
        .Append(cNewline);

    for (var y = 0; y < nRanks; y++) {
      var rank = bFlip ? y : InvertRank(y);
      var c = (Char)(cRankMin + rank);
      if (!bRightRuler)                 // Rank Ruler at Left
        sb.Append(c);
      appendRank(sb, rank, bFlip);
      if (bRightRuler)                  // Rank Ruler at Right
        sb.Append(c);
      sb.Append(cNewline);
    }

    if (!bTopRuler)                     // File Ruler at Bottom
      sb.AppendFiles(sSpace2, bFlip)
        .Append(cNewline);
  }

  private void appendProperties(StringBuilder sb) {
    var (blackSide, whiteSide) = Side.GetBothSides();

    sb.Append("Hashcode = ").Append(formatHash(Hash))
#if DisplayFEN
      .Append("; FEN = ").AppendLine(ToString(PositionType.FEN))
#else
      .Append("; EPD = ").AppendLine(ToString(PositionType.EPD))
#endif
#if DisplayFlags
      .Append("Flags: ")
      .AppendLine(
        formatState(
          FlagsMode,
          FlagsDraw,
          FlagsEval,
          blackSide.FlagsSide,
          whiteSide.FlagsSide,
          FlagsTurn,
          WTM()))
#else
      .AppendLine($"{getSide(WTM()).SideName} to Move")
#endif
#if DisplayCounts
      .AppendLine()
      .AppendPieceCounts(blackSide, whiteSide, blackSide.Counts, whiteSide.Counts)
#endif
#if DisplayPieceHash
      .AppendLine()
      .AppendPieceHash(blackSide, whiteSide)
#endif
      .AppendLine();
  }

  public StringBuilder Display(StringBuilder? sb = default) {
    if (sb == null) sb = new StringBuilder();
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
  private static void printMapping(String sLabel, Func<Int32, Int32> mapper) {
    var sb = new StringBuilder();
    sb.Append(sLabel)
      .Append(cNewline)
      .Append(cNewline);

    for (var y = 0; y < nRanks; y++) {
      var yInverse = InvertRank(y);
      var s = Empty;
      for (var x = 0; x < nFiles; x++)
        sb.Append($"{mapper(sqr(x, yInverse)),3}");
      sb.Append(cNewline);
    }

    sb.FlushLine();
  }

  private static void printSquares(String sLabel, Byte[] map) {
    printMapping(sLabel, n => map[n]);
  }

  protected static void WriteOrth(
    Plane qp, Boolean bRotateBoard = false, Boolean bFlip = false) {
    var sb = new StringBuilder();
    var bTopRuler = bFlip;
    var bRightRuler = bFlip;
    if (bTopRuler)                      // File Ruler at Top
      sb.AppendRuler(bRotateBoard, bFlip);

    for (var y = 0; y < nRanks; y++) {
      var rank = bFlip ? y : InvertRank(y);
      var c = (Char)(bRotateBoard ? cFileMax - rank : cRankMin + rank);
      if (!bRightRuler)                 // Rank Ruler at Left
        sb.Append(c);
      var uOrth = (UInt32)(qp >> sqr(0, rank) & MASK8);
      sb.AppendOrth(uOrth, bFlip);
      if (bRightRuler)                  // Rank Ruler at Right
        sb.Append(c);
      sb.Append(cNewline);
    }

    if (!bTopRuler)                   // File Ruler at Bottom
      sb.AppendRuler(bRotateBoard, bFlip);
    sb.FlushLine();
  }

  internal static void testOrth(
    String sLabel, Plane qp, Boolean bRotateBoard = false, Boolean bFlip = false) {
    LogLine(sLabel);
    LogLine();
    WriteOrth(qp, bRotateBoard, bFlip);
  }

  private static void writeDiagIndexes(Func<Int32, Int32> idxr) {
    var sb = new StringBuilder();
    for (var y = 0; y < nRanks; y++) {
      var yInverse = InvertRank(y);
      for (var x = 0; x < nFiles; x++)
        sb.Append($" {idxr(sqr(x, yInverse)),2}");
      sb.Append(cNewline);
    }

    sb.FlushLine();
  }

  private static void testDiagIndexes(String sLabel, Func<Int32, Int32> idxr) {
    LogLine(sLabel);
    LogLine();
    writeDiagIndexes(idxr);
  }
  #endregion

  #region Rotation Diagnostics
  private static Char ruler(Int32 d, Boolean bRotateBoard) {
    var c = cSpace;
    var dInverse = InvertDiag(d);
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
      var dInverse = InvertDiag(d);
      var nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
      var uDiagMask = uBit(nDiagLen) - 1;
      var uDiag = (UInt32)(qp >> OffsetDiag[dInverse] & uDiagMask);
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

  private static void testDiag(
    String sLabel, Plane qp, bool bRotateBoard = false) {
    LogLine(sLabel);
    LogLine();
    writeDiag(qp, bRotateBoard);
  }

  private static void writeOrthRotations(String sLabel, Plane[] qpOrth) {
    var sb = new StringBuilder();
    sb.Append(sLabel)
      .Append(cNewline)
      .Append(cNewline);

    for (var y = 0; y < nRanks; y++) {
      var yInverse = InvertRank(y);
      sb.AppendOrthRotations(qpOrth, bit(sqr(0, yInverse)))
        .Append(cNewline);
    }

    sb.FlushLine();
  }

  private static void writeDiagRotations(String sLabel, Plane[] qpDiag) {
    var sb = new StringBuilder();
    sb.Append(sLabel)
      .Append(cNewline)
      .Append(cNewline);

    for (var d = 0; d < nDiagonals; d++) {
      var dInverse = InvertDiag(d);
      var nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
      sb.AppendIndent(8 - nDiagLen)
        .AppendDiagRotations(nDiagLen, qpDiag, bit(OffsetDiag[dInverse]))
        .Append(cNewline);
    }

    sb.FlushLine();
  }
#endif
  #endregion
}
