//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2020-09-04 CNHume]Collapsed TokenScanner into Scanner.  Added a new StreamScanner sub-class
//                   which has a Rewind() method.
//[2016-09-04 CNHume]Introduced the new Scanner base class; and isolated
//                   original behaviour into a new TokenScanner subclass.
//[2014-07-04 CNHume]Created Class
//
// Conditionals:
//
namespace Command {
  using Command.Exceptions;

  using System;
  using System.Collections.Generic;
  using System.IO;
  using System.Text;
  using System.Text.RegularExpressions;

  using static System.String;

  public class Scanner : IDisposable {
    #region Constants
    protected const String sSpace = " ";
    protected const String sWhitespacePlus = @"\s+";
    private const Int32 nRowsCapacity = 64;
    #endregion                          // Constants

    #region Fields
    private Boolean disposed = false;
    private TextReader? reader;
    private String? sText;
    #endregion                          // Fields

    #region Properties
    protected Int32 NextRow { get; set; }
    public Int32 Row { get; private set; }
    public Int32 Column { get; private set; }
    public (Int32, Int32) Position { get => (Row, Column); set => Rewind(value); }

    // The following works for TextReader as well as StreamReader:
    public Boolean EndOfStream { get => Text is null; }
    public Boolean EndOfLine { get => IsNullOrEmpty(Text); }

    public TextReader? Reader {
      get => reader;
      set {
        Rows.Clear();
        NextRow = 0;
        Column = 0;
        reader?.Dispose();
        reader = value;

        ReadLine();
      }
    }

    // Text remaining to be scanned on the current line
    public String? Text {
      get => sText;
      private set {
        TextSpans = default;
        sText = value;
      }
    }

    protected List<String> Rows { get; }

    #region TextSpan Properties
    protected String[]? TextSpans { get; private set; }
    protected Int32 Index { get; set; }
    #endregion                          // TextSpan Properties
    #endregion                          // Properties

    #region Constructors
    public Scanner() {
      NextRow = 0;
      Rows = new List<String>(nRowsCapacity);
    }

    public Scanner(TextReader reader) : this() {
      Reader = reader;
    }

    public Scanner(String text) : this(new StringReader(text)) {
    }
    #endregion                          // Constructors

    #region IDisposable Interface
    public void Dispose() {
      Dispose(true);
      GC.SuppressFinalize(this);
    }

    protected virtual void Dispose(bool disposing) {
      if (!disposed) {
        if (disposing) Reader?.Dispose();
        disposed = true;
      }
    }
    #endregion                          // IDisposable Interface

    #region Methods
    public void Close() {
      Reader?.Close();
    }

    public void ReadLine() {
      Row = NextRow++;
      Column = 0;

      if (Row < Rows.Count)
        Text = Rows[Row];
      else {
        Text = Reader?.ReadLine();
        Rows.Add(Text);
      }
    }

    public void Rewind(Int32 nRow = 0, Int32 nColumn = 0) {
      if (Rows.Count < nRow)
        throw new ArgumentException($"Row = {nRow} must be <= Rows.Count = {Rows.Count}", nameof(nRow));

      var nLength = Rows[nRow]?.Length;
      if (nLength < nColumn)
        throw new ArgumentException($"Column = {nColumn} must be <= Rows[{nRow}].Length = {nLength}", nameof(nColumn));

      NextRow = nRow;
      ReadLine();

      if (nColumn > 0)
        Skip(nColumn);
    }

    public void Rewind((Int32 nRow, Int32 nColumn) position) {
      Rewind(position.nRow, position.nColumn);
    }

    public void Skip(Int32 nLength) {
      Column += nLength;
      Text = Text.Substring(nLength);
    }

    public String Scan(Int32 nLength) {
      var sValue = Text.Substring(0, nLength);
      Skip(nLength);
      return sValue;
    }

    public String AppendDetails(String message) {
      return $@"Row {Row}, Column {Column}, {message}: ""{Text}""";
    }

    #region TextSpan Methods
    public void SplitTextSpans(String regex = sWhitespacePlus) {
      TextSpans = Regex.Split(Text ?? Empty, regex);
      Index = 0;
    }

    private void ensureTextSpans(String regex) {
      if (TextSpans is null) SplitTextSpans(regex);
    }

    public Boolean HasTextSpan(String regex = sWhitespacePlus) {
      ensureTextSpans(regex);
      return Index < TextSpans?.Length;
    }

    public String Peek(String regex = sWhitespacePlus) {
      return HasTextSpan(regex) ? TextSpans[Index] : null;
    }

    public String Next(String regex = sWhitespacePlus) {
      var textSpan = Peek(regex);
      Index++;
      return textSpan;
    }

    //
    // Returns remaining spans as a single concatenated String
    //
    public String Tail(String delimiter = sSpace, String regex = sWhitespacePlus) {
      var sb = new StringBuilder();
      while (HasTextSpan(regex)) {
        if (sb.Length > 0) sb.Append(delimiter);
        sb.Append(Next());
      }
      return sb.ToString();
    }
    #endregion
    #endregion                          // Methods
  }

  public class StreamScanner : Scanner {
    #region Properties
    public new StreamReader? Reader {
      get => (StreamReader?)base.Reader;
      set => base.Reader = value;
    }
    #endregion                          // Properties

    #region Constructors
    public StreamScanner() : base() {
    }

    public StreamScanner(StreamReader reader) : base(reader) {
    }

    public StreamScanner(String text) : base(
      new StreamReader(
        new MemoryStream(
          Encoding.UTF8.GetBytes(text)
          ))) {
    }
    #endregion                          // Constructors

    #region Methods
    //
    //[Note]The Reader.BaseStream.Position property cannot be used to implement random access Seek()
    // for StreamScanner, as follows:
    //
    // Discard unread characters whenever BaseStream.Position is modified.
    // Reader.DiscardBufferedData();
    // Reader.BaseStream.Position = 0;
    //
    // This is because BaseStream manages an internal buffer in order to support Character Encodings.
    // Thus, one must implement Rewind() via Row and Column properties.
    //
    // See related discussion on Stackoverflow of "Tracking the position of the line of a streamreader"
    // https://stackoverflow.com/questions/10189270/tracking-the-position-of-the-line-of-a-streamreader
    //
    //public override void Rewind(Int32 nRow = 0, Int32 nColumn = 0) {
    //  base.Rewind(nRow, nColumn);
    //}
    #endregion                          // Methods
  }
}
