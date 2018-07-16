/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 */
//
// Rank
//   a  b  c  d  e  f  g  h
// 1 A1 B1 C1 D1 E1 F1 G1 H1
// 2 A2 B2 C2 D2 E2 F2 G2 H2
// 3 A3 B3 C3 D3 E3 F3 G3 H3
// 4 A4 B4 C4 D4 E4 F4 G4 H4
// 5 A5 B5 C5 D5 E5 F5 G5 H5
// 6 A6 B6 C6 D6 E6 F6 G6 H6
// 7 A7 B7 C7 D7 E7 F7 G7 H7
// 8 A8 B8 C8 D8 E8 F8 G8 H8
//
// File
//   1  2  3  4  5  6  7  8
// h H1 H2 H3 H4 H5 H6 H7 H8
// g G1 G2 G3 G4 G5 G6 G7 G8
// f F1 F2 F3 F4 F5 F6 F7 F8
// e E1 E2 E3 E4 E5 E6 E7 E8
// d D1 D2 D3 D4 D5 D6 D7 D8
// c C1 C2 C3 C4 C5 C6 C7 C8
// b B1 B2 B3 B4 B5 B6 B7 B8
// a A1 A2 A3 A4 A5 A6 A7 A8
//
// A1H8
//               h
//             g  H1
//           f  G1  H2
//         e  F1  G2  H3
//       d  E1  F2  G3  H4
//     c  D1  E2  F3  G4  H5
//   b  C1  D2  E3  F4  G5  H6
// a  B1  C2  D3  E4  F5  G6  H7
//  A1  B2  C3  D4  E5  F6  G7  H8
// 1  A2  B3  C4  D5  E6  F7  G8
//   2  A3  B4  C5  D6  E7  F8
//     3  A4  B5  C6  D7  E8
//       4  A5  B6  C7  D8
//         5  A6  B7  C8
//           6  A7  B8
//             7  A8
//               8
// A8H1
//               a
//             b  A1
//           c  B1  A2
//         d  C1  B2  A3
//       e  D1  C2  B3  A4
//     f  E1  D2  C3  B4  A5
//   g  F1  E2  D3  C4  B5  A6
// h  G1  F2  E3  D4  C5  B6  A7
//  H1  G2  F3  E4  D5  C6  B7  A8
// 1  H2  G3  F4  E5  D6  C7  B8
//   2  H3  G4  F5  E6  D7  C8
//     3  H4  G5  F6  E7  D8
//       4  H5  G6  F7  E8
//         5  H6  G7  F8
//           6  H7  G8
//             7  H8
//               8
package othello;
import static othello.Board.BIT0;
import static othello.Board.cBlackDisc;
import static othello.Board.cWhiteDisc;
import static othello.Board.cEmpty;
import static othello.Board.cSpace;
import static othello.Board.cFileMin;
import static othello.Board.cFileMax;
import static othello.Board.cRankMin;
import static othello.Board.cRankMax;
import static othello.Board.cNewline;
import static othello.Board.DiagOffset;
import static othello.Board.FFMASK;
import static othello.Board.nDiagonals;
import static othello.Board.nFiles;
import static othello.Board.nRanks;
import static othello.Board.nSquares;
import static othello.Board.sqr;
import static othello.Board.squareName;

public class Writer {
  final static char cVacant = '-';
  final static char cOccupied = 'X';
  final static String sSpace = " ";
  final static String sSpace2 = sSpace + sSpace;
  final static String sSpace3 = sSpace + sSpace2;
  final static String sSpace4 = sSpace + sSpace3;
  final static String squareUndefined = "??";

  public static StringBuilder appendRanks(StringBuilder sb, String s) {
    sb.append(sSpace);
    for (int x = 0; x < nRanks; x++)
      sb.append(s)
	.append(x + 1);
    return sb;
  }

  public static StringBuilder appendFiles(StringBuilder sb, String s) {
    sb.append(sSpace);
    char c = cFileMin;
    for (int x = 0; x < nFiles; x++, c += 1)
      sb.append(s)
	.append(c);
    return sb;
  }

  protected static void printSquares(String sLabel, byte[] squares) {
    StringBuilder sb = new StringBuilder();
    sb.append(sLabel)
      .append(cNewline)
      .append(cNewline);
    appendFiles(sb, sSpace2)
      .append(cNewline);		// Show File Letters
    for (int y = 0; y < nRanks; y++) {
      //int yInverse = invertRank(y);
      sb.append(y + 1);			// Show Rank Numbers
      for (int x = 0; x < nFiles; x++)
	sb.append(String.format("%3d", squares[sqr(x, y)]));
      sb.append(cNewline);
    }
    Logger.flushLine(sb);
  }

  protected static void printTranscript(String sLabel, byte[] squares) {
    final String sEmpty = String.format("%4s", cEmpty);
    StringBuilder sb = new StringBuilder();
    sb.append(sLabel)
      .append(cNewline)
      .append(cNewline);
    appendFiles(sb, sSpace3)
      .append(cNewline);		// Show File Letters
    for (int y = 0; y < nRanks; y++) {
      //int yInverse = invertRank(y);
      sb.append(y + 1);			// Show Rank Numbers
      for (int x = 0; x < nFiles; x++) {
	String s = sEmpty;
	byte coloredIndex = squares[sqr(x, y)];
	if (coloredIndex != 0) {
	  char color = coloredIndex > 0 ? cWhiteDisc : cBlackDisc;
	  byte index = (byte)Math.abs(coloredIndex);
	  s = String.format("%3d%s", index < nSquares ? index : 0, color);
	}
	sb.append(s);
      }
      sb.append(cNewline);
    }
    Logger.flushLine(sb);
  }

  // Find square holding bit based on Rotation Map:
  static String squareUsingBit(long[] qpMask, long qp) {
    for (int n = 0; n < nSquares; n++)
      if (qpMask[n] == qp) return squareName(n);
    return squareUndefined;
  }

  private static StringBuilder appendRectRotations(StringBuilder sb, long[] qpRect, long qp) {
    for (int x = 0; x < nFiles; x++, qp <<= 1)
      sb.append(sSpace)
	.append(squareUsingBit(qpRect, qp));
    return sb;
  }

  protected static void writeRectRotations(String sLabel, long[] qpRect) {
    StringBuilder sb = new StringBuilder();
    sb.append(sLabel)
      .append(cNewline)
      .append(cNewline);
    for (int y = 0; y < nRanks; y++) {
      //int yInverse = invertRank(y);
      appendRectRotations(sb, qpRect, BIT0 << sqr(0, y))
	.append(cNewline);
    }
    Logger.flushLine(sb);
  }

  private static StringBuilder appendDiagRotations(StringBuilder sb, int nDiagLen, long[] qpDiag, long qp) {
    for (int z = 0; z < nDiagLen; z++, qp <<= 1)
      sb.append(sSpace2)
	.append(squareUsingBit(qpDiag, qp));
    return sb;
  }

  protected static void writeDiagRotations(String sLabel, long[] qpDiag) {
    StringBuilder sb = new StringBuilder();
    sb.append(sLabel)
      .append(cNewline)
      .append(cNewline);
    for (int d = 0; d < nDiagonals; d++) {
      int dInverse = invertDiag(d);
      int nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
      appendIndent(sb, 8 - nDiagLen);
      appendDiagRotations(sb, nDiagLen, qpDiag, BIT0 << (int)DiagOffset[d])
	.append(cNewline);
    }
    Logger.flushLine(sb);
  }

  private static StringBuilder appendRect(StringBuilder sb, int nRect) {
    for (int x = 0; x < nFiles; x++, nRect >>= 1) {
      char c = (nRect & BIT0) == 0 ? cVacant : cOccupied;
      sb.append(sSpace)
	.append(c);
    }
    return sb;
  }

  //
  //    Rect	Rotate
  //	Type	Legend
  //	----	------
  //    Rank	false
  //    File	true
  //
  public static void writeRect(long qp, boolean bRotateLegend) {
    StringBuilder sb = new StringBuilder();
    if (bRotateLegend)
      appendRanks(sb, sSpace);		// Show Rank Numbers
    else
      appendFiles(sb, sSpace);		// Show File Letters
    sb.append(cNewline);
    for (int y = 0; y < nRanks; y++) {
      //int yInverse = invertRank(y);
      if (bRotateLegend) {
	char c = (char)(cFileMax - y);
	sb.append(c);			// Show File Letters
      }
      else
	sb.append(y + 1);		// Show Rank Numbers
      int nRect = (int)(qp >> sqr(0, y) & FFMASK);
      appendRect(sb, nRect)
	.append(cNewline);
    }
    Logger.flushLine(sb);
  }

  public static void testRect(String sLabel, long qp, boolean bRotateLegend) {
    Logger.logLine(String.format("%s", sLabel));
    Logger.logLine();
    writeRect(qp, bRotateLegend);
  }

  public static StringBuilder appendIndent(StringBuilder sb, int nDent) {
    for (int index = 0; index < nDent; index++)
      sb.append(sSpace2);
    return sb;
  }

  public static StringBuilder appendDiag(StringBuilder sb, int nDiagLen, int uDiag) {
    for (int z = 0; z < nDiagLen; z++, uDiag >>= 1) {
      char c = (uDiag & BIT0) == 0 ? cVacant : cOccupied;
      sb.append(sSpace3)
	.append(c);
    }
    return sb;
  }

  protected static int invertRank(int y) {
    return nRanks - (y + 1);
  }

  protected static int invertDiag(int d) {
    return nDiagonals - (d + 1);
  }

  private static char legend(int d, boolean bRotateLegend) {
    //int dInverse = invertDiag(d);
    char c = cSpace;
    if (d < nFiles - 1)
      c = (char)(bRotateLegend ? cFileMin + d + 1 : cFileMax - (d + 1));
    else if (d > nFiles - 1)
      c = (char)(cRankMin + d - nFiles);
    return c;
  }

  //
  //    Diag	Rotate
  //	Type	Legend
  //	----	------
  //    A1H8	false
  //    A8H1	true
  //
  public static void writeDiag(long qp, boolean bRotateLegend) {
    StringBuilder sb = new StringBuilder();
    appendIndent(sb, 8)
      .append(legend(-1, bRotateLegend))
      .append(cNewline);
    for (int d = 0; d < nDiagonals; d++) {
      int dInverse = invertDiag(d);
      int nDiagLen = d < nFiles ? d + 1 : dInverse + 1;
      int nDiagMask = (1 << nDiagLen) - 1;
      int uDiag = (int)(qp >> DiagOffset[d] & nDiagMask);
      appendIndent(sb, 8 - nDiagLen)
	.append(legend(d, bRotateLegend));
      appendDiag(sb, nDiagLen, uDiag)
	.append(cNewline);
    }
    appendIndent(sb, 8)
      .append(legend(nDiagonals, bRotateLegend))
      .append(cNewline);
    Logger.flushLine(sb);
  }

  public static void testDiag(String sLabel, long qp, boolean bRotateLegend) {
    Logger.logLine(String.format("%s", sLabel));
    Logger.logLine();
    writeDiag(qp, bRotateLegend);
  }
}
