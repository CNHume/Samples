//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-09-24 CNHume]Created Class
//
// Conditionals:
//
#define EnsureLogPathDirectoryExists

using System.Text;

using static System.String;

namespace Logging;

using Command;

static class Logger {
  #region Constants
  private const Char cSpace = ' ';

  private const String sInfo = "info";
  private const String sLogExtensionDefault = "log";
  private const String sLogRelativePath = @"Square\Logs";
  #endregion

  #region Enumerations
  public enum Level : byte { data, note, warn, error, failure, none };
  #endregion

  #region Fields
  private static String? sLogPath;
  #endregion

  #region Properties
  public static Level LogLevel { get; set; }
  public static String? LogPathDefault;
  public static String? LogPath {
    get => sLogPath;
    set {
      sLogPath = value;
      closeLogStream();
      LogStream = openLogStream(sLogPath);
    }
  }
  private static FileStream? LogStream { get; set; }
  #endregion

  #region Constructors
  static Logger() {
    LogLevel = Level.data;
    // Using %LOCALAPPDATA% vs %ALLUSERSPROFILE%
    var sLogRootPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
    LogPathDefault = Path.Combine(sLogRootPath, sLogRelativePath);
    LogPath = LogPathDefault;
  }
  #endregion

  #region Methods
  private static String? combineExtension(String? filename, String? ext) {
    return IsNullOrEmpty(Path.GetExtension(filename)) ? $"{filename}.{ext}" : filename;
  }

  private static String combineFilename(String path, String? filename, String ext) {
    var sFullFilename = combineExtension(filename, ext);
    return Path.Combine(path, sFullFilename ?? Empty);
  }

  private static FileStream? openLogStream(String? path) {
    FileStream? logStream = default;
    try {
      if (path != null) {
#if EnsureLogPathDirectoryExists
        var di = Directory.CreateDirectory(path);
#endif
        var sFullPath = combineFilename(path, Product.ProductName, sLogExtensionDefault);
        logStream = File.Open(sFullPath, FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read);
        logStream.Seek(0, SeekOrigin.End);
      }
    }
    catch (Exception ex) {
      var type = ex.GetType();
    }
    return logStream;
  }

  private static void closeLogStream() {
    if (LogStream != null) {
      LogStream.Dispose();
      LogStream = default;
    }
  }

  public static void Log(String? s = default, Boolean bWriteToConsole = true) {
    if (IsNullOrEmpty(s)) return;
    if (bWriteToConsole) Console.Write(s);

    if (LogStream == null) return;
    var encoding = new UnicodeEncoding();
    var buffer = encoding.GetBytes(s);
    LogStream.Write(buffer, 0, buffer.Length);
  }

  public static void LogLine(String? s = default, Boolean bWriteToConsole = true) {
    Log(s, bWriteToConsole);
    Log("\n", bWriteToConsole);
    LogStream?.Flush();
  }

  public static void LogInfo(Level level, String? s = default) {
    if (level < LogLevel) return;

    if (IsNullOrEmpty(s))
      LogLine();                        // Omitting level
    else {
      var sb = new StringBuilder(sInfo);
      sb.Append(cSpace);
      sb.Append(level);
      sb.Append(cSpace);
      sb.Append(s);
      LogLine(sb.ToString());
    }
  }
  #endregion
}
