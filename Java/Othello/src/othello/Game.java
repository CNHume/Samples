/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 *
 * For background on the development of Reversi and Othello
 * see https://en.wikipedia.org/wiki/Reversi
 */
package othello;
import static othello.Board.listMoves;

public class Game implements Command {
  final static Logger logger = new Logger(Logger.DEBUG);
  protected Pool<Board> boardPool;
  protected Board moveBoard;
  protected Board rootBoard;

  public Game() {
    boardPool = new Pool<Board>(new Board.BoardFactory<Board>(), "Board");
    reset();
  }

  public boolean execute(String command) {
    boolean loop = true;
    try {
      loop = dispatch(command);
    }
    catch (MoveException ex) {
      Logger.logLine(ex.getMessage());
    }
    return loop;
  }

  public boolean dispatch(String command) throws MoveException {
    boolean loop = true;
    boolean show = false;
    String commandLower = command.toLowerCase();
    switch (commandLower) {
    case "":
      break;
    case "board":
      moveBoard.showBoard();
      break;
    case "exit":
      loop = false;
      break;
    case "help":
      showHelp();
      break;
    case "hint":
      moveBoard.showMoves();
      break;
    case "list":
      listMoves(moveBoard.movesFromParent(rootBoard));
      break;
    case "reset":
      reset();
      show = true;
      break;
    case "score":
      moveBoard.showScore();
      break;
    case "transcript":
      moveBoard.showTranscript(moveBoard.movesFromParent(rootBoard));
      break;
    case "unmove":
      if (moveBoard == rootBoard)
	throw new MoveException("No Move has been made");
      unmove();
      show = true;
      break;
    default:
      moveBoard = moveBoard.parseMoves(commandLower);
      if (moveBoard.MoreMoves)
	show = true;
      else {				/// Adjudicate
	moveBoard.showBoard();
	moveBoard.showScore();
      }
      break;
    }
    if (show && moveBoard.MoreMoves) moveBoard.showBoard();
    return loop;
  }

  public static void showHelp() {
    StringBuilder sb = new StringBuilder();
    sb.append("exit: Exit program\n");
    sb.append("help: Show this help message\n");
    sb.append("hint: Show squares available for the side to move\n");
    sb.append("list: List game moves\n");
    sb.append("reset: Start a new game\n");
    sb.append("score: Show disc count for each side\n");
    sb.append("transcript: Generate a game transcript\n");
    sb.append("unmove: Undo prior move\n");
    Logger.flushLine(sb);
  }

  public Board push(Board parent) {
    Board child = boardPool.push();
    child.game = this;			//[Init]
    child.parent = parent;
    child.clear();
    return child;
  }

  public void pop(Board child) {
    child.game = null;
    child.parent = null;
    boardPool.pop(child);
  }

  public void unmove() {
    Board parent = moveBoard.parent;
    pop(moveBoard);
    moveBoard = parent;
  }

  protected void unwindBoards() {
    while (moveBoard != null) unmove();
    rootBoard = null;
  }

  public final void newGame() {
    unwindBoards();
    moveBoard = rootBoard
      = push(null);
  }

  public final void reset() {
    newGame();
    moveBoard.setup();
  }
}
