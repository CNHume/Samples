/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 */
package othello;
import static othello.Twiddle.deBruijnHalf;
import static othello.Twiddle.loadDeBruijn;
import static othello.Twiddle.nDeBruijn;
import static othello.Twiddle.newDeBruijn;
import static othello.Writer.invertRank;
import static othello.Writer.printSquares;
import static othello.Writer.writeDiagRotations;
import static othello.Writer.writeRectRotations;

public class BoardData {
  final static int nA8H1 = 7;
  final static int nA1H8 = 9;

  final static int nFiles = 8;
  final static int nRanks = nFiles;
  final static int nSquares = nRanks * nFiles;

  final static int nDiagonals = 15;     // nFiles + nRanks - 1
  final static int nStates = 6561;	// 3**8 FriendFoeStates
  final static int FFMASK = (1 << nFiles) - 1;

  final static char cSpace = ' ';
  final static char cFileMin = 'a';
  final static char cRankMin = '1';
  final static char cFileMax = cFileMin + nFiles - 1;
  final static char cRankMax = cRankMin + nRanks - 1;

  final static char cNewline = '\n';
  final static char cEmpty = '-';
  final static char cBlackDisc = 'B';
  final static char cWhiteDisc = 'W';

  final static long BIT0 = 1L;

  protected final long qpFileA = 0x0101010101010101L;
  protected final long qpFileB = qpFileA << 1;
  protected final long qpFileC = qpFileB << 1;
  protected final long qpFileD = qpFileC << 1;
  protected final long qpFileE = qpFileD << 1;
  protected final long qpFileF = qpFileE << 1;
  protected final long qpFileG = qpFileF << 1;
  protected final long qpFileH = qpFileG << 1;

  protected final long qpRank1 = 0xFFL;
  protected final long qpRank2 = qpRank1 << nFiles;
  protected final long qpRank3 = qpRank2 << nFiles;
  protected final long qpRank4 = qpRank3 << nFiles;
  protected final long qpRank5 = qpRank4 << nFiles;
  protected final long qpRank6 = qpRank5 << nFiles;
  protected final long qpRank7 = qpRank6 << nFiles;
  protected final long qpRank8 = qpRank7 << nFiles;

  //
  // There are 3**8 = 6561 FriendFoeStates along an 8-square Rank, File or Diagonal.
  // [Major diagonals contain 8 squares.  Minor diagonals contain fewer squares.]
  //
  // Each of the four Flips tables is doubly indexed by a FriendFoeState value
  // and a value corresponding to each square on the 64-square board.
  //
  // These tables provide 64-bit BitBoard values which identify the squares flipped when
  // a disc is placed on the corresponding square in the given FriendFoeState condition.
  //
  // Where this value is Zero, no squares would be flipped.  Moving to such a square
  // under the corresponding FriendFoeState is therefore not allowed.
  //
  // The Rotated BitBoard approach used here is based on
  // "Rotated bitmaps, a new twist on an old idea" by Dr. Robert Hyatt
  // See http://www.craftychess.com/hyatt/bitmaps.html
  //
  // Instrumentation Results:
  //
  // Size of RankFlips is 26,264 bytes
  // Size of RankFlips[0] is 528 bytes
  // Size of RankFlips[0][0] is 24 bytes
  // Per-state subtotal 528 + 64 * 24 = 1,536 bytes
  // Total = 26264 + 6561 * 1536 = 10,103,960 bytes per Flips table
  // Grand total of 40,415,840 bytes for all four Flips tables
  //
  // The size of the Flips tables can be optimized to only one third of their present size
  // by ensuring the origin square is empty.  3**7 = 2,187 FriendFoeStates would be needed.
  //
  protected static long[][] RankFlips;
  protected static long[][] FileFlips;
  protected static long[][] A1H8Flips;
  protected static long[][] A8H1Flips;

  protected static byte[] RankOffset;
  protected static byte[] FileOffset;
  protected static byte[] DiagOffset;	// Helps build A1H8Offset and A8H1Offset
  protected static byte[] A1H8Offset;
  protected static byte[] A8H1Offset;

  protected static long[] RankBit;
  protected static long[] FileBit;
  protected static long[] A1H8Bit;
  protected static long[] A8H1Bit;

  static {
    init();
  }

  public static void init() {
    newRankOffset();
    loadRankOffset();

    newRotation();
    loadRotation();

    newRectBit();
    newDiagBit();

    loadRectBit();
    loadDiagBit();

    newRectFlips();
    newDiagFlips();

    loadRectFlips();
    loadDiagFlips();

    deBruijnHalf = newDeBruijn(5);	// 32 == 1 << 5
    loadDeBruijn(deBruijnHalf, 5, nDeBruijn);

    //[Test]
    testBoard();
  }

  private static void testBoard() {
    //[Test]testOffsets();
    //[Test]testRotations();

    // FriendFoe Tests
    //FriendFoe ffTest = new FriendFoe();
    //ffTest.encode(0x00, 0xFF);
    //ffTest.decode(6560); //[Reverse]
    //ffTest.encode(0x80, 0x16);
    //ffTest.decode(2373); //[Forward]
    //ffTest.decode(1999); //[Reverse]
    //ffTest.encode(0xA8, 0x55);
    //ffTest.decode(4097); //[Forward]
    //ffTest.decode(5011); //[Reverse]
    //[Test]Logger.logLine("FriendFoe(0x%X, 0x%X) = %d%length", ffTest.friend, ffTest.foe, ffTest.state);
  }

  private static void testOffsets() {
    printSquares("RankOffset", RankOffset);
    printSquares("FileOffset", FileOffset);
    printSquares("A1H8Offset", A1H8Offset);
    printSquares("A8H1Offset", A8H1Offset);
  }

  private static void testRotations() {
    writeRectRotations("RankBit", RankBit);
    writeRectRotations("FileBit", FileBit);
    writeDiagRotations("A1H8Bit", A1H8Bit);
    writeDiagRotations("A8H1Bit", A8H1Bit);
  }

  private static void newRectFlips() {
    FileFlips = new long[nStates][];
    RankFlips = new long[nStates][];

    for (int nState = 0; nState < nStates; nState++) {
      FileFlips[nState] = new long[nSquares];
      RankFlips[nState] = new long[nSquares];
    }
  }

  private static void newDiagFlips() {
    A1H8Flips = new long[nStates][];
    A8H1Flips = new long[nStates][];

    for (int nState = 0; nState < nStates; nState++) {
      A1H8Flips[nState] = new long[nSquares];
      A8H1Flips[nState] = new long[nSquares];
    }
  }

  protected static void loadRectFlips() {
    FriendFoe ffLoad = new FriendFoe();
    for (int nState = 0; nState < nStates; nState++) {
      ffLoad.decode(nState);
      int foe = ffLoad.foe;
      int friend = ffLoad.friend;

      for (int y = 0; y < nRanks; y++) {
	int yInverse = invertRank(y);

	int bitOrigin = 1;
	for (int x = 0; x < nFiles; bitOrigin <<= 1, x++) {
	  boolean bEmpty = (friend & bitOrigin) == 0
			   && (foe & bitOrigin) == 0;
	  if (!bEmpty) continue;

	  int nRankPos = sqr(x, y);
	  int nFilePos = sqr(yInverse, x);

	  int xUp = x + 1;
	  if (xUp < nFiles) {
	    long qpRank = 0;
	    long qpFile = 0;
	    boolean bFoe = false;
	    int bit = 1 << xUp;
	    for (; (foe & bit) != 0; bit <<= 1, xUp++) {
	      bFoe = true;
	      qpRank |= BIT0 << sqr(xUp, y);
	      qpFile |= BIT0 << sqr(yInverse, xUp);
	    }

	    boolean bFriend = (friend & bit) != 0;
	    if (bFriend && bFoe) {
	      RankFlips[nState][nRankPos] |= qpRank;
	      FileFlips[nState][nFilePos] |= qpFile;
	    }
	  }

	  if (x >= 1) {
	    int xDn = x - 1;
	    long qpRank = 0;
	    long qpFile = 0;
	    boolean bFoe = false;
	    int bit = 1 << xDn;
	    for (; (foe & bit) != 0; bit >>= 1, xDn--) {
	      bFoe = true;
	      qpRank |= BIT0 << sqr(xDn, y);
	      qpFile |= BIT0 << sqr(yInverse, xDn);
	    }

	    boolean bFriend = (friend & bit) != 0;
	    if (bFriend && bFoe) {
	      RankFlips[nState][nRankPos] |= qpRank;
	      FileFlips[nState][nFilePos] |= qpFile;
	    }
	  }
	}
      }
    }
  }

  protected static void loadDiagFlips() {
    FriendFoe ffLoad = new FriendFoe();
    for (int d = 0; d < nDiagonals; d++) {
      int nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;
      int nDiagMask = (1 << nDiagLen) - 1;

      for (int nState = 0; nState < nStates; nState++) {
	ffLoad.decode(nState);
	int foe = ffLoad.foe & nDiagMask;
	int friend = ffLoad.friend & nDiagMask;
	int bitOrigin = 1;
	for (int x = d < nFiles ? 7 - d : 0,
	  y = d < nFiles ? 0 : d - 7,
	  w = 0; w < nDiagLen; bitOrigin <<= 1, w++, x++, y++) {
	  boolean bEmpty = (friend & bitOrigin) == 0
			   && (foe & bitOrigin) == 0;
	  if (!bEmpty) continue;

	  int xInverse = nFiles - (x + 1);
	  int nA1H8Pos = sqr(x, y);
	  int nA8H1Pos = sqr(xInverse, y);

	  //[Debug DiagIndexers]
	  assert d == diagA1H8(nA1H8Pos);	// A1H8 mismatch
	  assert d == diagA8H1(nA8H1Pos);	// A8H1 mismatch

	  int xUp = x + 1;
	  int yUp = y + 1;
	  if (xUp < nFiles && yUp < nRanks) {
	    long qpA1H8 = 0;
	    long qpA8H1 = 0;
	    boolean bFoe = false;
	    int bit = 1 << w + 1;
	    for (; (foe & bit) != 0; bit <<= 1, xUp++, yUp++) {
	      bFoe = true;
	      int xUpInverse = nFiles - (xUp + 1);
	      qpA1H8 |= BIT0 << sqr(xUp, yUp);
	      qpA8H1 |= BIT0 << sqr(xUpInverse, yUp);
	    }

	    boolean bFriend = (friend & bit) != 0;
	    if (bFriend && bFoe) {
	      A1H8Flips[nState][nA1H8Pos] |= qpA1H8;
	      A8H1Flips[nState][nA8H1Pos] |= qpA8H1;
	    }
	  }

	  int xDn = x - 1;
	  int yDn = y - 1;
	  if (x >= 1 && y >= 1) {
	    long qpA1H8 = 0;
	    long qpA8H1 = 0;
	    boolean bFoe = false;
	    int bit = 1 << w - 1;
	    for (; (foe & bit) != 0; bit >>= 1, xDn--, yDn--) {
	      bFoe = true;
	      int xDnInverse = nFiles - (xDn + 1);
	      qpA1H8 |= BIT0 << sqr(xDn, yDn);
	      qpA8H1 |= BIT0 << sqr(xDnInverse, yDn);
	    }

	    boolean bFriend = (friend & bit) != 0;
	    if (bFriend && bFoe) {
	      A1H8Flips[nState][nA1H8Pos] |= qpA1H8;
	      A8H1Flips[nState][nA8H1Pos] |= qpA8H1;
	    }
	  }
	}
      }
    }
  }

  private static void newRankOffset() {
    RankOffset = new byte[nSquares];
  }

  protected static void loadRankOffset() {
    for (int n = 0; n < nSquares; n++) {
      int y = n / nFiles;
      RankOffset[n] = (byte)(nFiles * y);	//[+1]
    }
  }

  private static void newRotation() {
    DiagOffset = new byte[nDiagonals];
    FileOffset = new byte[nSquares];
    A1H8Offset = new byte[nSquares];
    A8H1Offset = new byte[nSquares];
  }

  protected static void loadRotation() {
    int nDiagLen = 0;                 //[Note]DiagOffset increments by previous nDiagLen
    for (int d = 0; d < nDiagonals; d++) {
      DiagOffset[d] = (byte)(d > 0 ? DiagOffset[d - 1] + nDiagLen : nDiagLen);
      nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;
    }

    for (int n = 0; n < nSquares; n++) {
      int x = n % nFiles;
      int xInverse = nFiles - (x + 1);
      FileOffset[n] = (byte)(nFiles * xInverse);	//[+1]
      A1H8Offset[n] = (byte)(DiagOffset[diagA1H8(n)]);	//[+1]
      A8H1Offset[n] = (byte)(DiagOffset[diagA8H1(n)]);	//[+1]
    }
  }

  protected static int diagA1H8(int n) {
    int x = n % nFiles;
    int y = n / nFiles;
    int xInverse = nFiles - (x + 1);
    return y + xInverse;
  }

  protected static int diagA8H1(int n) {
    int x = n % nFiles;
    int y = n / nFiles;
    return x + y;
  }

  private static void newRectBit() {
    FileBit = new long[nSquares];
    RankBit = new long[nSquares];
  }

  private static void newDiagBit() {
    A1H8Bit = new long[nSquares];
    A8H1Bit = new long[nSquares];
  }

  protected static void loadRectBit() {
    long qp = BIT0;
    for (int y = 0; y < nRanks; y++) {
      int yInverse = invertRank(y);
      for (int x = 0; x < nFiles; x++, qp <<= 1)
	FileBit[sqr(yInverse, x)] = RankBit[sqr(x, y)] = qp;
    }
  }

  protected static void loadDiagBit() {
    long qp = BIT0;
    for (int d = 0; d < nDiagonals; d++) {
      int nDiagLen = d < nFiles ? d + 1 : nDiagonals - d;

      for (int x = d < nFiles ? 7 - d : 0,
	y = d < nFiles ? 0 : d - 7,
	w = 0; w < nDiagLen; w++, x++, y++, qp <<= 1) {
	int xInverse = nFiles - (x + 1);
	A8H1Bit[sqr(xInverse, y)] = A1H8Bit[sqr(x, y)] = qp;
      }
    }
  }

  public static int parseSquare(String square) throws MoveException {
    if (square.length() != 2)
      throw new MoveException(String.format("Invalid Square: %s", square));

    char cFile = square.charAt(0);
    if (cFile < cFileMin || cFileMax < cFile)
      throw new MoveException(String.format("Invalid File: %c", cFile));

    char cRank = square.charAt(1);
    if (cRank < cRankMin || cRankMax < cRank)
      throw new MoveException(String.format("Invalid Rank: %c", cRank));

    int x = cFile - cFileMin;
    int y = cRank - cRankMin;
    return sqr(x, y);
  }

  protected static int sqr(int x, int y) {
    return nFiles * y + x;
  }

  protected static String squareName(int n) {
    int x = n % nFiles;
    int y = n / nFiles;

    char cFile = (char)(cFileMin + x);
    char cRank = (char)(cRankMin + y);

    String sFile = String.valueOf(cFile);
    String square = sFile + cRank;
    return square;
  }
}
