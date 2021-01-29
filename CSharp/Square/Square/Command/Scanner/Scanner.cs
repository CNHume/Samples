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
    #endregion

    #region Fields
    private Boolean disposed = false;
    private TextReader reader;
    private String sText;
    private Int32 nNextRow;
    private Boolean bEndOfLine;
    #endregion

    #region Properties
    public Int32 Row { get; private set; }
    public Int32 Column { get; set; }

    // The following works for TextReader as well as StreamReader:
    public Boolean EndOfStream { get => Text is null; }
    public Boolean EndOfLine {
      get => bEndOfLine || EndOfStream;
      private set => bEndOfLine = value;
    }

    public TextReader Reader {
      get => reader;
      set {
        Rows.Clear();
        EndOfLine = false;
        nNextRow = 0;
        Column = 0;
        reader?.Dispose();
        reader = value;

        ReadLine();
      }
    }

    // Text remaining to be scanned on the current line
    public String Text {
      get => sText;
      private set {
        Tokens = default;
        sText = value;
      }
    }

    protected List<String> Rows { get; }
    #endregion

    #region Token Properties
    protected String[] Tokens { get; private set; }
    protected Int32 Index { get; set; }
    #endregion

    #region Constructors
    public Scanner() {
      EndOfLine = false;
      nNextRow = 0;
      Rows = new List<String>(nRowsCapacity);
    }

    public Scanner(TextReader reader) : this() {
      Reader = reader;
    }

    public Scanner(String text) : this(new StringReader(text)) {
    }
    #endregion

    #region IDisposable Methods
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
    #endregion

    #region Methods
    public void Close() {
      Reader.Close();
    }

    public void ReadLine() {
      Row = nNextRow++;
      Column = 0;

      if (Row < Rows.Count)
        Text = Rows[Row];
      else {
        Text = Reader.ReadLine();
        Rows.Add(Text);
      }
    }

    public virtual void Rewind(Int32 nRow = 0) {
      if (Rows.Count <= nRow)
        throw new ParseException($"Row = {nRow} must be < Rows.Count = {Rows.Count}");
      EndOfLine = false;
      nNextRow = nRow;
      ReadLine();
    }

    public void Skip(Int32 nLength) {
      Column += nLength;
      Text = Text.Substring(nLength);
      EndOfLine = Text.Length == 0;
      if (EndOfLine) ReadLine();
    }

    public String Scan(Int32 nLength) {
      var sValue = Text.Substring(0, nLength);
      Skip(nLength);
      return sValue;
    }

    public String AppendDetails(String message) {
      return $@"Row {Row}, Column {Column}, {message}: ""{Text}""";
    }
    #endregion

    #region Token Methods
    public void SplitTokens(String regex = sWhitespacePlus) {
      Tokens = Regex.Split(Text ?? Empty, regex);
      Index = 0;
    }

    private void ensureTokens(String regex) {
      if (Tokens is null) SplitTokens(regex);
    }

    public Boolean HasToken(String regex = sWhitespacePlus) {
      ensureTokens(regex);
      return Index < Tokens?.Length;
    }

    public String Peek(String regex = sWhitespacePlus) {
      return HasToken(regex) ? Tokens[Index] : null;
    }

    public String Next(String regex = sWhitespacePlus) {
      var token = Peek(regex);
      Index++;
      return token;
    }

    //
    // Returns remaining tokens as a single concatenated String
    //
    public String Tail(String delimiter = sSpace, String regex = sWhitespacePlus) {
      var sb = new StringBuilder();
      while (HasToken(regex)) {
        if (sb.Length > 0) sb.Append(delimiter);
        sb.Append(Next());
      }
      return sb.ToString();
    }
    #endregion
  }

  public class StreamScanner : Scanner {
    #region Properties
    public new StreamReader Reader {
      get => (StreamReader)base.Reader;
      set => base.Reader = value;
    }
    #endregion

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
    #endregion

    #region Methods
    //
    //[Note]Reader.BaseStream.Position property cannot be used to implement random access Seek() for StreamScanner.
    // The BaseStream manages an internal buffer which supports alternative Character Encodings.
    //
    // It will be much more practical to base random access on the Scanner base class Row and Column properties.
    //
    // See related discussion on Stackoverflow of "Tracking the position of the line of a streamreader"
    // https://stackoverflow.com/questions/10189270/tracking-the-position-of-the-line-of-a-streamreader
    //
    public override void Rewind(Int32 nRow = 0) {
      //
      // Discard unread characters whenever BaseStream.Position is modified.
      //
      //Reader.DiscardBufferedData();
      //Reader.BaseStream.Position = 0;

      base.Rewind(nRow);
    }
    #endregion
  }
}
