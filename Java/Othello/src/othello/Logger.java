/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 */
package othello;

public class Logger {
  private static final String sSpace = " ";

  // Level Names borrowed from log4net:
  public static final String[] levelName = {
    "OFF", "FATAL", "ERROR", "WARN", "INFO", "DEBUG", "ALL" };

  public static final byte OFF = 0;
  public static final byte FATAL = 1;
  public static final byte ERROR = 2;
  public static final byte WARN = 3;
  public static final byte INFO = 4;
  public static final byte DEBUG = 5;
  public static final byte ALL = 6;

  public static byte Level;

  Logger(byte logLevel) {
    Level = logLevel;
  }

  public static void log(String sFormat, Object... oArgs) {
    String s = String.format(sFormat, oArgs);
    System.out.print(s);
  }

  public static void logLine(String sFormat, Object... oArgs) {
    String s = String.format(sFormat, oArgs);
    System.out.println(s);
  }

  public static void logLine() {
    System.out.println();
  }

  public static void logLevel(byte level, String sFormat, Object... oArgs) {
    if (level <= Level) {
      StringBuilder sb = new StringBuilder(levelName[level]);
      sb.append(sSpace);
      sb.append(String.format(sFormat, oArgs));
      System.out.println(sb);
    }
  }

  public static void logLevelNewLine(byte level) {
    if (level <= Level)
      logLine();
  }

  public static void flushLine(StringBuilder sb) {
    if (sb.length() > 0) {
      logLine(sb.toString());
      sb.setLength(0);
    }
  }
}
