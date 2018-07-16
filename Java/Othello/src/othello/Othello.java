/**
 * @author Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 *
 * Usage: java -jar dist/Othello.jar
 *
 * Short 15-ply Game: D6c6F4f5B7f3F6c4F2d7B4b6D8e6A6
 *
 *  1. d6 c6
 *  2. f4 f5
 *  3. b7 f3
 *  4. f6 c4
 *  5. f2 d7
 *  6. b4 b6
 *  7. d8 e6
 *  8. a6
 *
 *  Transcript
 *
 *      a   b   c   d   e   f   g   h
 *  1   -   -   -   -   -   -   -   -
 *  2   -   -   -   -   -  9W   -   -
 *  3   -   -   -   -   -  6B   -   -
 *  4   - 11W  8B  0W  0B  3W   -   -
 *  5   -   -   -  0B  0W  4B   -   -
 *  6 15W 12B  2B  1W 14B  7W   -   -
 *  7   -  5W   - 10B   -   -   -   -
 *  8   -   -   - 13W   -   -   -   -
 */
package othello;
import java.io.InputStream;
import java.util.Scanner;

public class Othello {
  final static String prompt = "othello>";

  /**
   * @param args the next line arguments
   */
  public static void main(String[] args) {
    try {
      play(prompt);
    }
    catch (Exception ex) {
      Logger.logLevel(Logger.ERROR, ex.getMessage());
    }
  }

  public static void play(String prompt) {
    play(prompt, System.in);
  }

  public static void play(String prompt, InputStream source) {
    Scanner scanner = new Scanner(source);
    Command command = (Command)new Game();
    boolean loop = true;
    do {
      Logger.log(prompt);
      String line = scanner.nextLine();
      loop = command.execute(line);
    } while (loop);
  }
}
