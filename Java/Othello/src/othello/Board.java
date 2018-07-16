/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 */
package othello;
import java.util.ArrayList;
import static java.util.Arrays.fill;
import java.util.List;
import static othello.Writer.appendFiles;
import static othello.Writer.printTranscript;
import static othello.Writer.sSpace;
import static othello.Writer.testDiag;
import static othello.Writer.testRect;

public class Board extends BoardData {
  final static byte MOVEUNDEFINED = -1;
  final static byte MOVEWTM = 1 << 6;
  final static byte MOVEMASK = MOVEWTM - 1;

  final static String sBlack = "Black";
  final static String sWhite = "White";

  // Virtual Fields
  protected Game game;			// Initialized by Game.Push()
  protected Board parent;
  protected Byte currentMove;

  public ArrayList<Byte> setupMoves;
  public byte[] transcript;		// Game Transcript

  // Board state
  protected long WhiteRankDisc;
  protected long BlackRankDisc;
  protected long WhiteFileDisc;		// Rotations
  protected long BlackFileDisc;
  protected long WhiteA1H8Disc;
  protected long BlackA1H8Disc;
  protected long WhiteA8H1Disc;
  protected long BlackA8H1Disc;

  public boolean WTM;
  public boolean MoreMoves;

  // Thread Specific state
  private final FriendFoe ffState;	// Shared by the four game methods
  private final Twiddle tw;		// Shared Twiddle instance

  // inner static class for Factory<T>
  public static class BoardFactory<T> implements Factory<Board> {
    @Override
    public Board create() {
      return new Board();
    }
  }

  protected Board() {
    ffState = new FriendFoe();
    tw = new Twiddle();

    newSetupMoves();
    newTranscript();
  }

  public void copyTo(Board board) {
    board.WhiteRankDisc = WhiteRankDisc;
    board.BlackRankDisc = BlackRankDisc;
    board.WhiteFileDisc = WhiteFileDisc;
    board.BlackFileDisc = BlackFileDisc;
    board.WhiteA1H8Disc = WhiteA1H8Disc;
    board.BlackA1H8Disc = BlackA1H8Disc;
    board.WhiteA8H1Disc = WhiteA8H1Disc;
    board.BlackA8H1Disc = BlackA8H1Disc;

    board.WTM = WTM;
    board.MoreMoves = MoreMoves;
  }

  public void clear() {
    clearSetup();

    WTM = true;
    MoreMoves = false;
  }

  private void clearSetup() {
    WhiteRankDisc = WhiteFileDisc = WhiteA1H8Disc = WhiteA8H1Disc
      = BlackRankDisc = BlackFileDisc = BlackA1H8Disc = BlackA8H1Disc = 0;

    setupMoves.clear();
  }

  private void clearTranscript() {
    fill(transcript, (byte)0);
  }

  public void setup() {
    try {
      WTM = true;			// Setup White
      setSquare("d4");
      setSquare("e5");

      WTM = false;			// Setup Black
      setSquare("e4");
      setSquare("d5");

      MoreMoves = more();		// Advance WTM

      //[Test]
      testSetup();
    }
    catch (MoveException ex) {
      Logger.logLine(ex.getMessage());
    }
  }

  public void setSquare(String square) throws MoveException {
    int sq = parseSquare(square);
    if (WTM)
      setWhite(sq);
    else
      setBlack(sq);
    addSetupMove(sq);
  }

  private void addSetupMove(int n) {
    byte move = (byte)n;
    if (WTM) move |= MOVEWTM;
    setupMoves.add(move);
  }

  private void testSetup() throws MoveException {
    // Twiddle Tests
//    long input = BIT0 << 63;
//    int length = tw.removeLo(input);
//    Logger.logLine("removeLo(0x%X) = %d, r = 0x%X, s = 0x%X%length", input, length, tw.r, tw.s);

    // Setup additional discs to exercise the following test:
    //[Test White]testDiscs(true);
    //[Test Black]testDiscs(false);
  }

  private void testDiscs(boolean bWTM) {
    long qpRank = bWTM ? WhiteRankDisc : BlackRankDisc;
    long qpFile = bWTM ? WhiteFileDisc : BlackFileDisc;
    long qpA1H8 = bWTM ? WhiteA1H8Disc : BlackA1H8Disc;
    long qpA8H1 = bWTM ? WhiteA8H1Disc : BlackA8H1Disc;

    testRect("Rank", qpRank, false);
    testRect("File", qpFile, true);
    testDiag("A1H8", qpA1H8, false);
    testDiag("A8H1", qpA8H1, true);
  }

  // Wrapper Methods for game.push() and game.pop()
  public Board push() {
    return game.push(this);
  }

  public void pop(Board child) {
    child.game.pop(child);
  }

  public Board parseMoves(String moves) throws MoveException {
    Board board = this;

    int length = moves.length();
    if ((length & 1) != 0)
      throw new MoveException(String.format("%s has odd length of %d", moves, length));

    boolean annotate = length == 2;
    for (int begin = 0; begin < length; begin += 2) {
      int end = begin + 2;
      String square = moves.substring(begin, end);

      Board child = board.push();	// See unmove()
      try {
	int move = parseSquare(square);
	child.resetMove();
	child.move(move, annotate);
	board = child;
      }
      catch (Exception ex) {
	// Reclaim *last* child if move() throws an Exception
	pop(child);
	throw ex;
      }
    }

    return board;
  }

  public void move(int n, boolean annotate) throws MoveException {
    String square = squareName(n);
    String side = WTM ? sWhite : sBlack;

    //[Safe]Ensure square is not occupied:
    long qp = BIT0 << n;
    long qpOccupied = WhiteRankDisc | BlackRankDisc;
    if ((qp & qpOccupied) != 0) {
      String occupier = (qp & WhiteRankDisc) != 0 ? sWhite : sBlack;
      throw new MoveException(String.format("%s is occupied by %s", square, occupier));
    }

    // Othello rules require that some discs be flipped:
    long qpFlips = flips(n);
    if (qpFlips == 0)
      throw new MoveException(String.format("%s cannot move to %s", side, square));

    if (annotate) {
      int nFlips = count(qpFlips);
      String s = nFlips == 1 ? "" : "s";
      Logger.logLine("%s moves to %s with %d flip%s", side, square, nFlips, s);
    }

    // Record currentMove
    byte move = (byte)n;
    if (WTM) move |= MOVEWTM;
    currentMove = move;

    if (WTM) {
      setWhite(n);
      flipWhite(qpFlips);
    }
    else {
      setBlack(n);
      flipBlack(qpFlips);
    }

    MoreMoves = more();			// Advance WTM
  }

  protected void resetMove() throws MoveException {
    if (parent == null)
      throw new MoveException("resetMove() called at the Root Board");
    parent.copyTo(this);
  }

  // Determine whether either side has a move
  protected boolean more() {
    WTM = !WTM;
    if (anyMoves()) return true;
    WTM = !WTM;
    return anyMoves();
  }

  public void showMoves() {
    long qpMoves = findMoves();
    String side = WTM ? sWhite : sBlack;
    String sLabel = String.format("%s moves:", side);
    testRect(sLabel, qpMoves, false);
  }

  public void showBoard() {
    StringBuilder sb = new StringBuilder();
    // Display board before prompting for input
    display(sb);
    if (MoreMoves)
      sb.append(WTM ? sWhite : sBlack)
	.append(" to move");
    else
      sb.append("Game over");
    Logger.flushLine(sb);
  }

  public void showScore() {
    StringBuilder sb = new StringBuilder();
    int nWhite = count(WhiteRankDisc);
    int nBlack = count(BlackRankDisc);
    sb.append(nWhite)
      .append(" White Discs")
      .append(cNewline)
      .append(nBlack)
      .append(" Black Discs");

    if (!MoreMoves) {			// Declare game result
      String result = "draw";
      if (nBlack < nWhite)
	result = "White wins";
      else if (nWhite < nBlack)
	result = "Black wins";
      sb.append(cNewline)
	.append(result);
    }

    Logger.flushLine(sb);
  }

  public int count(long qp) {
    int count = 0;
    while (qp != 0) {
      tw.removeLo(qp);
      qp = tw.r;
      count++;
    }
    return count;
  }

  public ArrayList<Byte> movesFromParent(Board parent) {
    ArrayList<Byte> moves = new ArrayList<Byte>();
    for (Board board = this; board != null && board != parent; board = board.parent)
      moves.add(0, board.currentMove);
    return moves;
  }

  private static String colorMove(byte move) {
    String square = squareName(move & MOVEMASK);
    return (move & MOVEWTM) != 0 ? square.toUpperCase() : square.toLowerCase();
  }

  // List Game Moves
  public static void listMoves(List<Byte> moves) {
    StringBuilder sb = new StringBuilder();
    moves.stream().forEach((move) -> sb.append(colorMove(move)));
    Logger.logLine(sb.toString());
  }

  // Show Game Transcript
  public void showTranscript(List<Byte> moves) {
    clearTranscript();
    for (int move : setupMoves) {
      byte coloredIndex = (byte)((move & MOVEWTM) != 0 ? nSquares : -nSquares);
      transcript[move & MOVEMASK] = coloredIndex;
    }

    byte index = 0;
    for (int move : moves) {
      index++;
      byte coloredIndex = (byte)((move & MOVEWTM) != 0 ? index : -index);
      transcript[move & MOVEMASK] = coloredIndex;
    }
    printTranscript("Transcript", transcript);
  }

  private boolean squareEmpty(int n) {
    long qp = BIT0 << n;
    long unionDisc = WhiteRankDisc | BlackRankDisc;
    return (qp & unionDisc) == 0;
  }

  private boolean squareWhite(int n) {
    long qp = BIT0 << n;
    return (qp & WhiteRankDisc) != 0;
  }

  private void appendDisc(StringBuilder sb, int n) {
    char color = squareEmpty(n) ? cEmpty : squareWhite(n) ? cWhiteDisc : cBlackDisc;
    sb.append(color);
  }

  private void appendRank(StringBuilder sb, int y) {
    sb.append(y + 1);			// Show Rank Numbers
    for (int x = 0; x < nFiles; x++) {
      sb.append(sSpace);
      appendDisc(sb, sqr(x, y));
    }
    sb.append(cNewline);
  }

  private void display(StringBuilder sb) {
    appendFiles(sb, sSpace) // Show File Letters
      .append(cNewline);
    for (int y = 0; y < nRanks; y++) {
      //int yInverse = invertRank(y);
      appendRank(sb, y);
    }
  }

  private void newSetupMoves() {
    setupMoves = new ArrayList<Byte>();
  }

  private void newTranscript() {
    transcript = new byte[nSquares];
  }

  protected long flips(int n) {
    return rankFlips(n) | fileFlips(n) | a1h8Flips(n) | a8h1Flips(n);
  }

  protected long rankFlips(int n) {
    return RankFlips[stateRank(n)][n];
  }

  protected long fileFlips(int n) {
    return FileFlips[stateFile(n)][n];
  }

  protected long a1h8Flips(int n) {
    return A1H8Flips[stateA1H8(n)][n];
  }

  protected long a8h1Flips(int n) {
    return A8H1Flips[stateA8H1(n)][n];
  }

  protected int stateRank(int n) {
    int nWhiteState = (int)(WhiteRankDisc >> RankOffset[n]) & FFMASK;
    int nBlackState = (int)(BlackRankDisc >> RankOffset[n]) & FFMASK;
    if (WTM)
      ffState.encode(nWhiteState, nBlackState);
    else
      ffState.encode(nBlackState, nWhiteState);
    return ffState.state;
  }

  protected int stateFile(int n) {
    int nWhiteState = (int)(WhiteFileDisc >> FileOffset[n]) & FFMASK;
    int nBlackState = (int)(BlackFileDisc >> FileOffset[n]) & FFMASK;
    if (WTM)
      ffState.encode(nWhiteState, nBlackState);
    else
      ffState.encode(nBlackState, nWhiteState);
    return ffState.state;
  }

  protected int stateA1H8(int n) {
    int nWhiteState = (int)(WhiteA1H8Disc >> A1H8Offset[n]) & FFMASK;
    int nBlackState = (int)(BlackA1H8Disc >> A1H8Offset[n]) & FFMASK;
    if (WTM)
      ffState.encode(nWhiteState, nBlackState);
    else
      ffState.encode(nBlackState, nWhiteState);
    return ffState.state;
  }

  protected int stateA8H1(int n) {
    int nWhiteState = (int)(WhiteA8H1Disc >> A8H1Offset[n]) & FFMASK;
    int nBlackState = (int)(BlackA8H1Disc >> A8H1Offset[n]) & FFMASK;
    if (WTM)
      ffState.encode(nWhiteState, nBlackState);
    else
      ffState.encode(nBlackState, nWhiteState);
    return ffState.state;
  }

  protected boolean anyMoves() {
    long qpEmpty = ~(WhiteRankDisc | BlackRankDisc);
    long qpFoe = WTM ? BlackRankDisc : WhiteRankDisc;

    long qpRankUp = (qpFoe & ~qpFileH) << 1;
    long qpRankDn = (qpFoe & ~qpFileA) >> 1;
    long qpRank = (qpRankUp | qpRankDn) & qpEmpty;
    if (anyRank(qpRank)) return true;

    long qpFileUp = qpFoe << nFiles;
    long qpFileDn = qpFoe >> nFiles;
    long qpFile = (qpFileUp | qpFileDn) & qpEmpty;
    if (anyFile(qpFile)) return true;

    long qpA1H8Up = (qpFoe & ~qpFileH) << nA1H8;
    long qpA1H8Dn = (qpFoe & ~qpFileA) >> nA1H8;
    long qpA1H8 = (qpA1H8Up | qpA1H8Dn) & qpEmpty;
    if (anyA1H8(qpA1H8)) return true;

    long qpA8H1Up = (qpFoe & ~qpFileA) << nA8H1;
    long qpA8H1Dn = (qpFoe & ~qpFileH) >> nA8H1;
    long qpA8H1 = (qpA8H1Up | qpA8H1Dn) & qpEmpty;
    if (anyA8H1(qpA8H1)) return true;

    return false;
  }

  protected boolean anyRank(long qpRank) {
    while (qpRank != 0) {
      int n = tw.removeLo(qpRank);
      qpRank = tw.r;
      if (rankFlips(n) != 0) return true;
    }
    return false;
  }

  protected boolean anyFile(long qpFile) {
    while (qpFile != 0) {
      int n = tw.removeLo(qpFile);
      qpFile = tw.r;
      if (fileFlips(n) != 0) return true;
    }
    return false;
  }

  protected boolean anyA1H8(long qpA1H8) {
    while (qpA1H8 != 0) {
      int n = tw.removeLo(qpA1H8);
      qpA1H8 = tw.r;
      if (a1h8Flips(n) != 0) return true;
    }
    return false;
  }

  protected boolean anyA8H1(long qpA8H1) {
    while (qpA8H1 != 0) {
      int n = tw.removeLo(qpA8H1);
      qpA8H1 = tw.r;
      if (a8h1Flips(n) != 0) return true;
    }
    return false;
  }

  protected long findMoves() {
    long qpMoves = 0;
    long qpEmpty = ~(WhiteRankDisc | BlackRankDisc);
    long qpFoe = WTM ? BlackRankDisc : WhiteRankDisc;

    long qpRankUp = (qpFoe & ~qpFileH) << 1;
    long qpRankDn = (qpFoe & ~qpFileA) >> 1;
    long qpRank = (qpRankUp | qpRankDn) & qpEmpty;
    qpMoves |= findRank(qpRank);

    long qpFileUp = qpFoe << nFiles;
    long qpFileDn = qpFoe >> nFiles;
    long qpFile = (qpFileUp | qpFileDn) & qpEmpty;
    qpMoves |= findFile(qpFile);

    long qpA1H8Up = (qpFoe & ~qpFileH) << nA1H8;
    long qpA1H8Dn = (qpFoe & ~qpFileA) >> nA1H8;
    long qpA1H8 = (qpA1H8Up | qpA1H8Dn) & qpEmpty;
    qpMoves |= findA1H8(qpA1H8);

    long qpA8H1Up = (qpFoe & ~qpFileA) << nA8H1;
    long qpA8H1Dn = (qpFoe & ~qpFileH) >> nA8H1;
    long qpA8H1 = (qpA8H1Up | qpA8H1Dn) & qpEmpty;
    qpMoves |= findA8H1(qpA8H1);

    return qpMoves;
  }

  protected long findRank(long qpRank) {
    long qpMoves = 0;
    while (qpRank != 0) {
      int n = tw.removeLo(qpRank);
      qpRank = tw.r;
      if (rankFlips(n) != 0) qpMoves |= tw.s;
    }
    return qpMoves;
  }

  protected long findFile(long qpFile) {
    long qpMoves = 0;
    while (qpFile != 0) {
      int n = tw.removeLo(qpFile);
      qpFile = tw.r;
      if (fileFlips(n) != 0) qpMoves |= tw.s;
    }
    return qpMoves;
  }

  protected long findA1H8(long qpA1H8) {
    long qpMoves = 0;
    while (qpA1H8 != 0) {
      int n = tw.removeLo(qpA1H8);
      qpA1H8 = tw.r;
      if (a1h8Flips(n) != 0) qpMoves |= tw.s;
    }
    return qpMoves;
  }

  protected long findA8H1(long qpA8H1) {
    long qpMoves = 0;
    while (qpA8H1 != 0) {
      int n = tw.removeLo(qpA8H1);
      qpA8H1 = tw.r;
      if (a8h1Flips(n) != 0) qpMoves |= tw.s;
    }
    return qpMoves;
  }

  public void flipWhite(long qp) {
    while (qp != 0) {
      int n = tw.removeLo(qp);
      qp = tw.r;
      setWhite(n);
    }
  }

  public void flipBlack(long qp) {
    while (qp != 0) {
      int n = tw.removeLo(qp);
      qp = tw.r;
      setBlack(n);
    }
  }

  public void setWhite(int n) {
    long qp = BIT0 << n;
    BlackRankDisc &= ~qp;
    WhiteRankDisc |= qp;
    clrBlackRotations(n);
    setWhiteRotations(n);
  }

  public void setBlack(int n) {
    long qp = BIT0 << n;
    WhiteRankDisc &= ~qp;
    BlackRankDisc |= qp;
    clrWhiteRotations(n);
    setBlackRotations(n);
  }

  protected void clrWhiteRotations(int n) {
    WhiteFileDisc &= ~FileBit[n];
    WhiteA1H8Disc &= ~A1H8Bit[n];
    WhiteA8H1Disc &= ~A8H1Bit[n];
  }

  protected void clrBlackRotations(int n) {
    BlackFileDisc &= ~FileBit[n];
    BlackA1H8Disc &= ~A1H8Bit[n];
    BlackA8H1Disc &= ~A8H1Bit[n];
  }

  protected void setWhiteRotations(int n) {
    WhiteFileDisc |= FileBit[n];
    WhiteA1H8Disc |= A1H8Bit[n];
    WhiteA8H1Disc |= A8H1Bit[n];
  }

  protected void setBlackRotations(int n) {
    BlackFileDisc |= FileBit[n];
    BlackA1H8Disc |= A1H8Bit[n];
    BlackA8H1Disc |= A8H1Bit[n];
  }
}
