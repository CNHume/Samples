//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2013-09-24 CNHume]Created Class
//
// Conditionals:
//
#define EnsureLogPathDirectoryExists

using System.Text;

using static System.IO.Path;
using static System.String;

namespace Logging;

using Commands;

static class Logger {
  #region Constants
  private const Char cSpace = ' ';

  private const String sInfo = "info";
  private const String sFileTypeDefault = "log";
  private const String sRelativePath = @"Games\Logs";
  #endregion

  #region Enumerations
  public enum LogLevel : byte { data, note, warn, error, failure, none };
  #endregion

  #region Fields
  private static String? sPath;
  #endregion

  #region Properties
  public static LogLevel Level { get; set; }
  public static String? PathDefault;
  public static String? Path {
    get => sPath;
    set {
      sPath = value;
      closeLogStream();
      Stream = openLogStream(sPath);
    }
  }
  private static FileStream? Stream { get; set; }
  #endregion

  #region Constructors
  static Logger() {
    Level = LogLevel.data;
    // Using %LOCALAPPDATA% vs %ALLUSERSPROFILE%
    var sRootPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
    PathDefault = Combine(sRootPath, sRelativePath);
    Path = PathDefault;
  }
  #endregion

  #region Methods
  private static String? combineExtension(String? filename, String? ext) {
    return IsNullOrEmpty(GetExtension(filename)) ? $"{filename}.{ext}" : filename;
  }

  private static String combineFilename(String path, String? filename, String ext) {
    var sFullFilename = combineExtension(filename, ext);
    return Combine(path, sFullFilename ?? Empty);
  }

  private static FileStream? openLogStream(String? path) {
    FileStream? logStream = default;
    try {
      if (path != null) {
#if EnsureLogPathDirectoryExists
        var di = Directory.CreateDirectory(path);
#endif
        var sFullPath = combineFilename(path, Product.ProductName, sFileTypeDefault);
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
    if (Stream != null) {
      Stream.Dispose();
      Stream = default;
    }
  }

  public static void Log(String? s = default, Boolean bWriteToConsole = true) {
    if (IsNullOrEmpty(s)) return;
    if (bWriteToConsole) Console.Write(s);

    if (Stream == null) return;
    var encoding = new UnicodeEncoding();
    var buffer = encoding.GetBytes(s);
    Stream.Write(buffer, 0, buffer.Length);
  }

  public static void LogLine(String? s = default, Boolean bWriteToConsole = true) {
    Log(s, bWriteToConsole);
    Log("\n", bWriteToConsole);
    Stream?.Flush();
  }

  public static void LogInfo(LogLevel level, String? s = default) {
    if (level < Level) return;

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
