﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2016-09-05 CNHume]Created File
//

using System.Diagnostics.CodeAnalysis;
using System.Text.RegularExpressions;

using static System.String;
using static System.Text.RegularExpressions.RegexOptions;

namespace Commands;

using Engine;

using Exceptions;

using static Logging.Logger;
using static Parser.TokenRuleType;

partial class Parser : IDisposable {
  #region Constants
  private const String sSpace = " ";
  private const String sHyphen = "-";
  private const Char cDoubleQuote = '"';
  private const String sDoubleQuote = @"""";
  private const String sEscapedDoubleQuote = @"""""";
  #endregion                            // Constants

  #region Fields
  private Boolean disposed = false;
  private Scanner? scanner;

  private static readonly TokenRule[] codeTokenRules;
  private static readonly TokenRule[] opcodeDelimiterTokenRules;
  private static readonly TokenRule[] eolTokenRules;
  private static readonly TokenRule[] lineTokenRules;
  private static readonly TokenRule[] spaceTokenRule;

  private static readonly TokenRule[] enableKeywordTokenRules;
  private static readonly TokenRule[] registerKeywordTokenRules;
  private static readonly TokenRule[] goKeywordTokenRules;
  private static readonly TokenRule[] movesKeywordTokenRules;
  private static readonly TokenRule[] nameKeywordTokenRules;
  private static readonly TokenRule[] opcodeTokenRules;
  private static readonly TokenRule[] operandTokenRules;
  private static readonly TokenRule[] optionTokenRules;
  private static readonly TokenRule[] pacnMoveTokenRules;
  private static readonly TokenRule[] setupTokenRules;
  private static readonly TokenRule[] setupTypeTokenRules;
  private static readonly TokenRule[] countTokenRules;
  private static readonly TokenRule[] unsignedTokenRules;
  private static readonly TokenRule[] valueKeywordTokenRules;
  private static readonly TokenRule[] verbTokenRules;

  private Token codeToken;
  private Token eolToken;
  private Token opcodeDelimiterToken;
  private Token lineToken;
  public Token SpaceToken;

  private Token enableKeywordToken;
  public Token GoKeywordToken;
  private Token movesKeywordToken;
  private Token nameKeywordToken;
  private Token opcodeToken;
  private Token operandToken;
  private Token optionToken;
  public Token PACNMoveToken;
  public Token RegisterKeywordToken;
  public Token SetupToken;
  private Token setupTypeToken;
  public Token CountToken;
  public Token UnsignedToken;
  private Token valueKeywordToken;
  private Token verbToken;
  #endregion                            // Fields

  #region Properties
  public Scanner? Scanner {
    get => scanner;
    set {
      scanner?.Dispose();
      scanner = value;
    }
  }

  public Boolean IsVerbose { get; }
  #endregion                            // Properties

  #region Constructors
  public Parser(Boolean bVerbose = false) {
    IsVerbose = bVerbose;
    init();
  }

  public Parser(Scanner scanner, Boolean bVerbose = false) : this(bVerbose) {
    Scanner = scanner;
  }

  public Parser(String text, Boolean bVerbose = false) : this(new Scanner(text), bVerbose) {
  }

  /*
   * Please see https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol
   * for details of the UCI Commands Interface as documented by Stefan Meyer-Kahlen
   */
  static Parser() {
    codeTokenRules = [
      new(code, @"[-\w]+\b"),
    ];
    opcodeDelimiterTokenRules = [
      new(opcodeDelimiter, @"\s*;")
    ];
    eolTokenRules = [
      new(eol, @"\s*$")
    ];
    lineTokenRules = [
      new(line, @".*$")
    ];
    spaceTokenRule = [
      new(space, @"\s+")
    ];
    verbTokenRules = [
      new(
        verb,
        @"(best|board|debug|exit|getoption|go|isready|list|moves|perft|ponderhit|position|quit|register|reset|resetoption|setoption|status|stop|tabiya|test|testepd|timertest|uci|ucinewgame|unmove)\b",
        IgnoreCase)
    ];
    enableKeywordTokenRules = [
      new(enableKeyword, @"(on|off)\b", IgnoreCase)
    ];
    goKeywordTokenRules = [
      new(
        goKeyword,
        @"(searchmoves|ponder|wtime|btime|winc|binc|movestogo|depth|nodes|mate|movetime|infinite)\b",
        IgnoreCase)
    ];
    registerKeywordTokenRules = [
      new(registerKeyword, @"(later|code|name)\b", IgnoreCase)
    ];
    setupTypeTokenRules = [
      new(setupType, @"(fen|epd|startpos|random)\b", IgnoreCase)
    ];
    setupTokenRules = [
      new(
        setup,
        @"[pnbrqkPNBRQK1-8]{0,8}(/[pnbrqkPNBRQK1-8]{0,8}){7}(\s+[wb](\s+(-|[KQkq|A-H|a-h]{1,4})(\s+(-|[a-h][36]\b))?)?)?")
    ];
    opcodeTokenRules = [
      new(opcode, @"[a-zA-Z]\w{0,14}\b"),
    ];
    operandTokenRules = [
      new(@float, @"[+-](0|[1-9]\d*)(\.\d+)?"),
      new(unsigned, @"0|[1-9]\d*"),
      new(sanMove, @"([NBRQK]?[a-h]?[1-8]?[x-]?[a-h][1-8](=[NBRQ])?|O-O|O-O-O)\b[+#]?", IgnoreCase),
      new(@string, @"""([^""]|"""")*""")
    ];
    optionTokenRules = [
      new(option, @"[a-zA-Z]\w*\b"),
    ];
    countTokenRules = [
      new(hyphen, @"-"),
      new(unsigned, @"(0|[1-9]\d*)")
    ];
    unsignedTokenRules = [
      new(unsigned, @"(0|[1-9]\d*)")
    ];
    movesKeywordTokenRules = [
      new(movesKeyword, @"moves\b", IgnoreCase)
    ];
    nameKeywordTokenRules = [
      new(nameKeyword, @"name\b", IgnoreCase)
    ];
    //
    // UCI moves are expressed in Pure Algebraic Coordinate Notation (PACN):
    // See https://www.chessprogramming.org/Algebraic_Chess_Notation
    //
    // PACN is similar to a notation attributed to Warren Smith:
    // Regex for Smith Notation (SN): ([a-h][1-8]){2}[pnbrqkEcC]?[NBRQ]?
    // Lowercase is used to render captures "reversible" in SN.  SN uses uppercase for promotions, where PACN uses lowercase.
    //
    pacnMoveTokenRules = [
      new(pacnMove, @"([a-h][1-8]){2}[nbrq]?|0000|000?|OOO?|O-O(-O)?", IgnoreCase)
    ];
    valueKeywordTokenRules = [
      new(valueKeyword, @"(=|value\b)", IgnoreCase)
    ];
  }

  [MemberNotNull(
    nameof(codeToken),
    nameof(eolToken),
    nameof(opcodeDelimiterToken),
    nameof(lineToken),
    nameof(SpaceToken),
    nameof(GoKeywordToken),
    nameof(enableKeywordToken),
    nameof(movesKeywordToken),
    nameof(nameKeywordToken),
    nameof(opcodeToken),
    nameof(operandToken),
    nameof(optionToken),
    nameof(PACNMoveToken),
    nameof(RegisterKeywordToken),
    nameof(SetupToken),
    nameof(setupTypeToken),
    nameof(CountToken),
    nameof(UnsignedToken),
    nameof(valueKeywordToken),
    nameof(verbToken)
    )]
  private void init() {
    codeToken = new(this, TokenType.code, codeTokenRules);
    eolToken = new(this, TokenType.eol, eolTokenRules);
    opcodeDelimiterToken = new(this, TokenType.opcodeDelimiter, opcodeDelimiterTokenRules);
    lineToken = new(this, TokenType.line, lineTokenRules);
    SpaceToken = new(this, TokenType.space, spaceTokenRule);

    GoKeywordToken = new(this, TokenType.goKeyword, goKeywordTokenRules);
    enableKeywordToken = new(this, TokenType.enableKeyword, enableKeywordTokenRules);
    movesKeywordToken = new(this, TokenType.movesKeyword, movesKeywordTokenRules);
    nameKeywordToken = new(this, TokenType.nameKeyword, nameKeywordTokenRules);
    opcodeToken = new(this, TokenType.opcode, opcodeTokenRules);
    operandToken = new(this, TokenType.operand, operandTokenRules);
    optionToken = new(this, TokenType.option, optionTokenRules);
    PACNMoveToken = new(this, TokenType.pacnMove, pacnMoveTokenRules);
    RegisterKeywordToken = new(this, TokenType.registerKeyword, registerKeywordTokenRules);
    SetupToken = new(this, TokenType.setup, setupTokenRules);
    setupTypeToken = new(this, TokenType.setupType, setupTypeTokenRules);
    CountToken = new(this, TokenType.counter, countTokenRules);
    UnsignedToken = new(this, TokenType.unsigned, unsignedTokenRules);
    valueKeywordToken = new(this, TokenType.valueKeyword, valueKeywordTokenRules);
    verbToken = new(this, TokenType.verb, verbTokenRules);
  }

  [MemberNotNull(nameof(Scanner))]
  public void EnsureScanner(String sCommand) {
    if (Scanner == null)                // Update existing Parser
      Scanner = NewScanner(sCommand);
    else                                // Update existing Scanner
      Scanner.Reader = newReader(sCommand);
  }

  public static Scanner NewScanner(String sCommand) {
    var reader = newReader(sCommand);
    //return new StreamScanner((StreamReader)reader);
    return new Scanner(reader);
  }

  private static TextReader newReader(String sCommand) {
    //return new StreamReader(
    //  new MemoryStream(
    //    Encoding.UTF8.GetBytes(sCommand)
    //    ));
    return new StringReader(sCommand);
  }
  #endregion                            // Constructors

  #region IDisposable Interface
  public void Dispose() {
    Dispose(true);
    GC.SuppressFinalize(this);
  }

  protected virtual void Dispose(Boolean disposing) {
    if (!disposed) {
      if (disposing) Scanner?.Dispose();
      disposed = true;
    }
  }
  #endregion                            // IDisposable Interface

  #region Enumerations
  #region Position Setup Type Enum
  protected enum SetupType : byte { None, EPD, FEN, Random, StartPos };
  #endregion                            // Position Setup Type Enum
  #endregion                            // Enumerations

  #region Methods
  public void Close() {
    Scanner?.Close();
  }

  public Boolean AcceptEOL(String? sMethodName = default, Boolean bShowText = true) {
    ArgumentNullException.ThrowIfNull(Scanner);

    var bAccepted = Scanner.EndOfLine || eolToken.Accept();
    if (bAccepted)
      Scanner.ReadLine();
    else if (bShowText && IsVerbose)
      LogLine(eolMessage(sMethodName));
    return bAccepted;
  }

  public void ExpectEOL(String? sMethodName = default, Boolean bShowText = true) {
    if (!AcceptEOL(sMethodName, bShowText))
      throw new ParseException(eolMessage(sMethodName));
  }

  private String eolMessage(String? sMethodName = default) {
    const String sContext = "Could not parse text at End of Line";
    ArgumentNullException.ThrowIfNull(Scanner);

    var message = IsNullOrEmpty(sMethodName) ? sContext : $"{sMethodName} {sContext}";
    return Scanner.AppendDetails(message);
  }

  //[ToDo]Refactor top-level Commands Loop
  public IEnumerable<object?> Parse() {
    ArgumentNullException.ThrowIfNull(Scanner);

    while (!Scanner.EndOfStream) {
      // Preserve value of Text prior to the Scanner side-effects of the Accept Method
      var sText = Scanner.Text;

      var obj = ParseRow();
      var bAccepted = obj != null;

      if (bAccepted)
        yield return obj;
      else {
        //[Test]
        //if (IsVerbose) {
        //  LogLine(@$"Rejected: ""{sText}""");
        //}

        // Skip past any Row that cannot be parsed.
        Scanner.ReadLine();
      }
    }
  }

  //[ToDo]
  public object? ParseRow() {
    object? obj = default;
    return obj;
  }

  public String ParseVerb() {
    SpaceToken.Accept();
    if (AcceptEOL())
      return Empty;

    verbToken.Expect();
    return verbToken.Value;
  }

  private String? parseOptionName(out Control? control) {
    SpaceToken.Accept();
    nameKeywordToken.Accept();          //[UCI]Technically, the option "name" keyword is required.

    String? sKeyword = default;
    String? sName = default;
    while (true) {
      SpaceToken.Accept();
      //[UCI]Accummulate space separated, compound names until a "value" keyword is found.
      if (valueKeywordToken.Accept()) {
        sKeyword = valueKeywordToken.Value;
        break;
      }
      else if (optionToken.Accept()) {
        var sToken = optionToken.Value;
        sName = IsNullOrEmpty(sName) ? sToken : $"{sName} {sToken}";
      }
      else break;
    }

    control = Control.FindControl(GameState.ControlPanel, sName);

    //
    // The "value" Keyword is returned only to indicate whether or not it was present.
    // The Keyword was already validated by valueKeywordTokenRules.
    //
    return sKeyword;
  }

  private String? parseOptionValue(String? sKeyword) {
    String? sValue = default;
    if (sKeyword != null) {
      SpaceToken.Accept();
      lineToken.Expect();
      sValue = lineToken.Value;
    }
    return sValue;
  }

  private static void rejectOptionValue(String? sKeyword) {
    if (sKeyword != null)
      throw new ParseException($"Superfluous {sKeyword} keyword specified");
  }

  public void GetOptionCommand() {
    var sKeyword = parseOptionName(out Control? control);
    // Option names may be delimited by the "value" keyword.
    rejectOptionValue(sKeyword);
    var setting = control?.AsSetting();
    var value = setting?.GetValue();
    if (value != null)
      LogLine(value.ToString());
  }

  public void ResetOptionCommand() {
    var sKeyword = parseOptionName(out Control? control);
    // Option names may be delimited by the "value" keyword.
    rejectOptionValue(sKeyword);
    var setting = control?.AsSetting();
    setting?.SetDefault();
  }

  public void SetOptionCommand() {
    var sValueKeyword = parseOptionName(out Control? control);
    var sValue = parseOptionValue(sValueKeyword);
    var setting = control?.AsSetting();
    setting?.SetValue(sValue);
  }

  public Boolean MovesKeyword() {
    return SpaceToken.Accept() && movesKeywordToken.Accept();
  }

  private static Position? findNamedPosition(Position? position, Position? parent) {
    while (position is not null &&
           !ReferenceEquals(position, parent) &&
           IsNullOrEmpty(position.Name))
      position = position.Parent;
    return position;
  }

  public void TabiyaCommand(Position position) {
    var state = position.State;
    var namedPosition = findNamedPosition(position, state?.RootPosition);
    if (namedPosition is not null && !IsNullOrEmpty(namedPosition.Name)) {
      namedPosition.Display(namedPosition.Name);
      var bChess960 = state?.IsChess960 ?? false;
      position.ListMovesFromParent(namedPosition, bChess960, MovesKeyword());
    }
  }

  private void setupPosition(SetupType setupType, ref Position position) {
    switch (setupType) {
    case SetupType.EPD:
      SpaceToken.Expect();
      SetupToken.Expect();
      var operations = ParseOperations();
      position.ParseEPD(SetupToken.Value, operations);
      break;
    case SetupType.FEN:
      SpaceToken.Expect();
      SetupToken.Expect();
      var sHalfMoveCount = ParseCount("0");
      var sFullMoveNumber = ParseCount("1");
      position.ParseFEN(SetupToken.Value, sHalfMoveCount, sFullMoveNumber);
      position = parseMoves(position);
      break;
    case SetupType.Random:
      UInt16 wChess960;
      if (SpaceToken.Accept() && UnsignedToken.Accept()) {
        wChess960 = ParseUInt16("Chess960 Index", UnsignedToken.Value);
        position.SetFischerRandom(wChess960);
        position = parseMoves(position);
      }
      else {
        //
        // The following generates a random Chess960 Index using a SeededRandom instance:
        //
        wChess960 = (UInt16)position.State.SeededRandom.Next(960);
        position.SetFischerRandom(wChess960);
        //[Note]Subsequent moves cannot be specified from a random position.
      }
      break;
    case SetupType.StartPos:
      position.SetFEN();
      position = parseMoves(position);
      break;
    default:
      throw new NotImplementedException($"{setupType}");
    }
  }

  public Position PositionCommand(Position position) {
    const Boolean ignoreCase = true;
    SpaceToken.Expect();
    setupTypeToken.Expect();
    var sKeyword = setupTypeToken.Value;

    var setupType = sKeyword.TryParseEnum<SetupType>(ignoreCase);
    if (!setupType.HasValue)
      throw new ParseException($"Unknown Position Setup Type: {sKeyword}");

    try {
      //[Note]parseMoves() may result in a new position.
      setupPosition(setupType.Value, ref position);
    }
    catch (InvalidPositionException) {
      position.Display("Invalid Position");
      throw;
    }

    return position;
  }

  public String ParseCount(String sDefault = "0") {
    if (SpaceToken.Accept() && CountToken.Accept()) {
      var sValue = CountToken.Value;
      return sValue == sHyphen ? sDefault : sValue;
    }
    return sDefault;
  }

  private Position parseMoves(Position position) {
    if (SpaceToken.Accept() && movesKeywordToken.Accept()) {
      position = position.ParsePACNMakeMoves(this);
    }
    return position;
  }

  public Dictionary<String, List<String>?>? ParseOperations() {
    Dictionary<String, List<String>?>? operations = default;
    if (SpaceToken.Accept()) {
      operations = new(Board.OperationsCapacity);
      while (opcodeToken.Accept()) {
        //
        // For a list of standard EPD opcodes see:
        // https://www.chessprogramming.org/Extended_Position_Description#Opcode_mnemonics
        //
        //  acn analysis count nodes
        //  acs analysis count seconds
        //  am avoid move(s)
        //  bm best move(s)
        //  c0 comment (primary, also c1 though c9)
        //  ce centipawn evaluation
        //  dm direct mate fullmove count
        //  draw_accept accept a draw offer
        //  draw_claim claim a draw
        //  draw_offer offer a draw
        //  draw_reject reject a draw offer
        //  eco Encyclopedia of Chess Openings opening code
        //=>fmvn fullmove number
        //=>hmvc halfmove clock
        //=>id position identification
        //  nic _New In Chess_ opening code
        //  noop no operation
        //  pm predicted move
        //  pv predicted variation
        //  rc repetition count
        //  resign game resignation
        //  sm supplied move
        //  tcgs telecommunication game selector
        //  tcri telecommunication receiver identification
        //  tcsi telecommunication sender identification
        //  v0 variation name(primary, also v1 though v9)
        //
        var sOpcode = opcodeToken.Value;
        var operands = parseOperands(operations, sOpcode);

        if (operations.ContainsKey(sOpcode)) {
          var sOperands = operands == null ?
            String.Empty : Join(sSpace, operands);
          throw new ParseException($"EPD opcode already specified for: {sOpcode} {sOperands}");
        }

        operations.Add(sOpcode, operands);
        if (!SpaceToken.Accept()) break;
      }
    }

    return operations;
  }

  private List<String>? parseOperands(
    Dictionary<String, List<String>?> operations, String sOpcode) {
    List<String>? operands = default;
    if (SpaceToken.Accept()) {
      operands = [];
      while (operandToken.Accept()) {
        operands.Add(operandToken.Value);
        if (opcodeDelimiterToken.Accept()) break;
        if (AcceptEOL()) throw new ParseException($"Undelimited {sOpcode} operand");
        SpaceToken.Expect();
      }
    }

    return operands;
  }

  public static String? GetSingleValue(
    Dictionary<String, List<String>?>? operations, String sOpcode, String? sDefault = default) {
    if (operations == null) return sDefault;

    operations.TryGetValue(sOpcode, out List<String>? operands);
    if (operands == null) return sDefault;

    if (operands.Count == 0)          // The Opcode is assumed to provide a Single Value, i.e., an Operand
      throw new ParseException($"EPD opcode {sOpcode} has no operand");

    if (operands.Count > 1) {
      var sOperands = Join(sSpace, operands);
      throw new ParseException($"Multiple operands specified with EPD opcode: {sOpcode} {sOperands}");
    }

    return operands[0];
  }

  // For UCI.parseRegister
  public String ParseCode() {
    SpaceToken.Expect();
    codeToken.Expect();
    return codeToken.Value;
  }

  #region Numeric Parsers
  public static Byte ParseByte(String sName, String? sValue) {
    if (Byte.TryParse(sValue, out Byte vValue)) return vValue;
    throw new ParseException($"Invalid {sName} = {sValue}");
  }

  public static UInt16 ParseUInt16(String sName, String? sValue) {
    if (UInt16.TryParse(sValue, out UInt16 wValue)) return wValue;
    throw new ParseException($"Invalid {sName} = {sValue}");
  }

  public static UInt32 ParseUInt32(String sName, String? sValue) {
    if (UInt32.TryParse(sValue, out UInt32 uValue)) return uValue;
    throw new ParseException($"Invalid {sName} = {sValue}");
  }

  public static UInt64 ParseUInt64(String sName, String? sValue) {
    if (UInt64.TryParse(sValue, out UInt64 qValue)) return qValue;
    throw new ParseException($"Invalid {sName} = {sValue}");
  }

  public Boolean ParseEnableKeyword() {
    var bFoundKeyword = true;
    var bEnabled = false;

    SpaceToken.Expect();
    enableKeywordToken.Expect();

    var sKeyword = enableKeywordToken.Value;
    switch (sKeyword.ToLower()) {
    case "on":
      bEnabled = true;
      break;
    case "off":
      bEnabled = false;
      break;
    default:
      bFoundKeyword = false;
      break;
    }

    if (!bFoundKeyword)
      throw new ParseException($"Unknown {sKeyword} keyword");

    return bEnabled;
  }
  #endregion                            // Numeric Parsers

  #region Verbatim Literal Methods
  public static Boolean IsVerbatimLiteral(String sValue) {
    return !IsNullOrEmpty(sValue) && sValue[0] == cDoubleQuote;
  }

  public static String VerbatimLiteralToString(String sValue) {
    const String paramName = nameof(sValue);

    if (!IsVerbatimLiteral(sValue))
      throw new ArgumentException("Not a verbatim literal", paramName);

    if (sValue.Length < 2 || sValue[sValue.Length - 1] != cDoubleQuote)
      throw new ArgumentException("Improperly delimited verbatim literal", paramName);

    var split = Regex.Split(sValue[1..^1], sEscapedDoubleQuote);

    if (split.Any(s => s.Contains(cDoubleQuote)))
      throw new ArgumentException("Contains an improperly escaped double quote", paramName);

    return Join(sDoubleQuote, split);
  }

  public static String StringToVerbatimLiteral(String sInput) {
    return @$"""{sInput.Replace(sDoubleQuote, sEscapedDoubleQuote)}""";
  }
  #endregion                            // Verbatim Literal Methods
  #endregion                            // Methods
}
