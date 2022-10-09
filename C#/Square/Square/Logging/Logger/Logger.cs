//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2013-09-24 CNHume]Created Class
//
// Conditionals:
//
#define EnsureLogPathDirectoryExists

namespace Logging {
  using Command;

  using System;
  using System.IO;
  using System.Text;

  using static System.String;

  static class Logger {
    #region Constants
    private const String sSpace = " ";
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
        CloseLogStream();
        LogStream = OpenLogStream(sLogPath);
      }
    }
    private static FileStream? LogStream { get; set; }
    #endregion

    #region Constructors
    static Logger() {
      LogLevel = Level.data;
      // Using %LOCALAPPDATA% vs. %ALLUSERSPROFILE%
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

    private static FileStream? OpenLogStream(String? path) {
      FileStream? logStream = default;
      try {
        if (path is not null) {
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

    private static void CloseLogStream() {
      if (LogStream is not null) {
        LogStream.Dispose();
        LogStream = default;
      }
    }

    public static void LogFlush() {
      if (LogStream is not null) LogStream.Flush();
    }

    public static void LogWrite(String s) {
      if (LogStream is not null && !IsNullOrEmpty(s)) {
        var encoding = new UnicodeEncoding();
        var buffer = encoding.GetBytes(s);
        LogStream.Write(buffer, 0, buffer.Length);
      }
    }

    public static void LogWriteLine() {
      LogWrite("\n");
      LogFlush();
    }

    public static void LogWriteLine(String s) {
      if (s is not null) {
        LogWrite(s);
        LogWriteLine();
      }
    }

    private static void Write(String sFormat, params Object[] oArgs) {
      var s = Format(sFormat, oArgs);
      Console.Write(s);
      LogWrite(s);
    }

    private static void WriteLine() {
      Write("\n");
      LogFlush();
    }

    private static void WriteLine(String s) {
      if (s is not null) {
        Write(s);
        WriteLine();
      }
    }

    private static void WriteLine(String sFormat, params Object[] oArgs) {
      var s = Format(sFormat, oArgs);
      WriteLine(s);
    }

    public static void Log(String sFormat, params Object[] oArgs) {
      Write(sFormat, oArgs);
    }

    public static void LogLine(String sFormat, params Object[] oArgs) {
      WriteLine(sFormat, oArgs);
    }

    public static void LogLine() {
      WriteLine();
    }

    public static void LogInfo(Level level, String sFormat, params Object[] oArgs) {
      if (LogLevel <= level) {
        var sb = new StringBuilder(sInfo);
        sb.Append(sSpace);
        sb.Append(level);
        sb.Append(sSpace);
        sb.AppendFormat(sFormat, oArgs);
        WriteLine(sb.ToString());
      }
    }

    public static void LogInfoNewLine(Level level) {
      if (LogLevel <= level)
        WriteLine();
    }
    #endregion
  }
}
