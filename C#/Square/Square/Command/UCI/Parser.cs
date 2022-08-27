//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2016-09-05 CNHume]Created File
//

namespace Command {
  using Engine;

  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics.CodeAnalysis;
  using System.Linq;
  using System.Text.RegularExpressions;

  using static Logging.Logger;
  using static System.String;
  using static System.Text.RegularExpressions.RegexOptions;

  partial class Parser : IDisposable {
    #region Constants
    protected const String sSpace = " ";
    protected const String sHyphen = "-";
    protected const Char cDoubleQuote = '"';
    protected const String sDoubleQuote = @"""";
    protected const String sEscapedDoubleQuote = @"""""";
    #endregion                          // Constants

    #region Fields
    private Boolean disposed = false;
    private Scanner? scanner;

    protected static readonly TokenRule[] codeTokenRules;
    protected static readonly TokenRule[] opcodeDelimiterTokenRules;
    protected static readonly TokenRule[] eolTokenRules;
    protected static readonly TokenRule[] lineTokenRules;
    protected static readonly TokenRule[] spaceTokenRule;

    protected static readonly TokenRule[] enableKeywordTokenRules;
    protected static readonly TokenRule[] registerKeywordTokenRules;
    protected static readonly TokenRule[] goKeywordTokenRules;
    protected static readonly TokenRule[] movesKeyworTokendRules;
    protected static readonly TokenRule[] nameKeywordTokenRules;
    protected static readonly TokenRule[] opcodeTokenRules;
    protected static readonly TokenRule[] operandTokenRules;
    protected static readonly TokenRule[] optionTokenRules;
    protected static readonly TokenRule[] pacnMoveTokenRules;
    protected static readonly TokenRule[] setupTokenRules;
    protected static readonly TokenRule[] setupTypeTokenRules;
    protected static readonly TokenRule[] countTokenRules;
    protected static readonly TokenRule[] unsignedTokenRules;
    protected static readonly TokenRule[] valueKeywordTokenRules;
    protected static readonly TokenRule[] verbTokenRules;

    protected Token codeToken;
    protected Token eolToken;
    protected Token opcodeDelimiterToken;
    protected Token lineToken;
    public Token SpaceToken;

    protected Token enableKeywordToken;
    public Token GoKeywordToken;
    protected Token movesKeywordToken;
    protected Token nameKeywordToken;
    protected Token opcodeToken;
    protected Token operandToken;
    protected Token optionToken;
    public Token PACNMoveToken;
    public Token RegisterKeywordToken;
    public Token SetupToken;
    protected Token setupTypeToken;
    public Token CountToken;
    public Token UnsignedToken;
    protected Token valueKeywordToken;
    protected Token verbToken;
    #endregion                          // Fields

    #region Properties
    public Scanner? Scanner {
      get => scanner;
      set {
        scanner?.Dispose();
        scanner = value;
      }
    }

    public Boolean IsVerbose { get; }
    #endregion                          // Properties

    #region Constructors
    public Parser(Boolean isVerbose = false) {
      IsVerbose = isVerbose;
      Init();
    }

    public Parser(Scanner scanner, Boolean isVerbose = false) : this(isVerbose) {
      Scanner = scanner;
    }

    public Parser(String text, Boolean isVerbose = false) : this(new Scanner(text), isVerbose) {
    }

    /*
     * Please see https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol
     * for details of the UCI Command Interface as documented by Stefan Meyer-Kahlen
     */
    static Parser() {
      codeTokenRules = new[] {
          new TokenRule(TokenRuleType.code, @"[-\w]+\b"),
      };
      opcodeDelimiterTokenRules = new[] {
          new TokenRule(TokenRuleType.opcodeDelimiter, @"\s*;")
      };
      eolTokenRules = new[] {
        new TokenRule(TokenRuleType.eol, @"\s*$")
      };
      lineTokenRules = new[] {
          new TokenRule(TokenRuleType.line, @".*$")
      };
      spaceTokenRule = new[] {
          new TokenRule(TokenRuleType.space, @"\s+")
      };
      verbTokenRules = new[] {
          new TokenRule(TokenRuleType.verb,
            @"(best|board|debug|exit|getoption|go|isready|list|moves|perft|ponderhit|position|quit|status|register|reset|resetoption|setoption|status|stop|tabiya|test|testepd|timertest|uci|ucinewgame|unmove)\b",
          IgnoreCase)
      };
      enableKeywordTokenRules = new[] {
          new TokenRule(TokenRuleType.enableKeyword, @"(on|off)\b", IgnoreCase)
      };
      goKeywordTokenRules = new[] {
          new TokenRule(TokenRuleType.goKeyword, @"(searchmoves|ponder|wtime|btime|winc|binc|movestogo|depth|nodes|mate|movetime|infinite)\b", IgnoreCase)
      };
      registerKeywordTokenRules = new[] {
          new TokenRule(TokenRuleType.registerKeyword, @"(later|code|name)\b", IgnoreCase)
      };
      setupTypeTokenRules = new[] {
          new TokenRule(TokenRuleType.setupType, @"(fen|epd|startpos|random)\b", IgnoreCase)
      };
      setupTokenRules = new[] {
          new TokenRule(TokenRuleType.setup, @"[pnbrqkPNBRQK1-8]{0,8}(/[pnbrqkPNBRQK1-8]{0,8}){7}(\s+[wb](\s+(-|[KQkq|A-H|a-h]{1,4})(\s+(-|[a-h][36]\b))?)?)?")
      };
      opcodeTokenRules = new[] {
          new TokenRule(TokenRuleType.opcode, @"[a-zA-Z]\w{0,14}\b"),
      };
      operandTokenRules = new[] {
          new TokenRule(TokenRuleType.@float, @"[+-](0|[1-9]\d*)(\.\d+)?"),
          new TokenRule(TokenRuleType.unsigned, @"0|[1-9]\d*"),
          new TokenRule(TokenRuleType.sanMove, @"([NBRQK]?[a-h]?[1-8]?[x-]?[a-h][1-8](=[NBRQ])?|O-O|O-O-O)\b[+#]?", IgnoreCase),
          new TokenRule(TokenRuleType.@string, @"""([^""]|"""")*""")
      };
      optionTokenRules = new[] {
          new TokenRule(TokenRuleType.option, @"[a-zA-Z]\w*\b"),
      };
      countTokenRules = new[] {
          new TokenRule(TokenRuleType.hyphen, @"-"),
          new TokenRule(TokenRuleType.unsigned, @"(0|[1-9]\d*)")
      };
      unsignedTokenRules = new[] {
          new TokenRule(TokenRuleType.unsigned, @"(0|[1-9]\d*)")
      };
      movesKeyworTokendRules = new[] {
          new TokenRule(TokenRuleType.movesKeyword, @"moves\b", IgnoreCase)
      };
      nameKeywordTokenRules = new[] {
          new TokenRule(TokenRuleType.nameKeyword, @"name\b", IgnoreCase)
      };
      //
      // UCI moves are expressed in Pure Algebraic Coordinate Notation (PACN):
      // See https://www.chessprogramming.org/Algebraic_Chess_Notation
      //
      // PACN is similar to a notation attributed to Warren Smith:
      // Regex for Smith Notation (SN): ([a-h][1-8]){2}[pnbrqkEcC]?[NBRQ]?
      // Lowercase is used to render captures "reversible" in SN.  SN uses uppercase for promotions, where PACN uses lowercase.
      //
      pacnMoveTokenRules = new[] {
          new TokenRule(TokenRuleType.pacnMove, @"([a-h][1-8]){2}[nbrq]?|0000|000?|OOO?|O-O(-O)?", IgnoreCase)
      };
      valueKeywordTokenRules = new[] {
          new TokenRule(TokenRuleType.valueKeyword, @"(=|value\b)", IgnoreCase)
      };
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
    protected void Init() {
      codeToken = new Token(this, TokenType.code, codeTokenRules);
      eolToken = new Token(this, TokenType.eol, eolTokenRules);
      opcodeDelimiterToken = new Token(this, TokenType.opcodeDelimiter, opcodeDelimiterTokenRules);
      lineToken = new Token(this, TokenType.line, lineTokenRules);
      SpaceToken = new Token(this, TokenType.space, spaceTokenRule);

      GoKeywordToken = new Token(this, TokenType.goKeyword, goKeywordTokenRules);
      enableKeywordToken = new Token(this, TokenType.enableKeyword, enableKeywordTokenRules);
      movesKeywordToken = new Token(this, TokenType.movesKeyword, movesKeyworTokendRules);
      nameKeywordToken = new Token(this, TokenType.nameKeyword, nameKeywordTokenRules);
      opcodeToken = new Token(this, TokenType.opcode, opcodeTokenRules);
      operandToken = new Token(this, TokenType.operand, operandTokenRules);
      optionToken = new Token(this, TokenType.option, optionTokenRules);
      PACNMoveToken = new Token(this, TokenType.pacnMove, pacnMoveTokenRules);
      RegisterKeywordToken = new Token(this, TokenType.registerKeyword, registerKeywordTokenRules);
      SetupToken = new Token(this, TokenType.setup, setupTokenRules);
      setupTypeToken = new Token(this, TokenType.setupType, setupTypeTokenRules);
      CountToken = new Token(this, TokenType.counter, countTokenRules);
      UnsignedToken = new Token(this, TokenType.unsigned, unsignedTokenRules);
      valueKeywordToken = new Token(this, TokenType.valueKeyword, valueKeywordTokenRules);
      verbToken = new Token(this, TokenType.verb, verbTokenRules);
    }

    [MemberNotNull(nameof(Scanner))]
    public void EnsureScanner(String sCommand) {
      if (Scanner is null)       // Update existing Parser
        Scanner = NewScanner(sCommand);
      else                              // Update existing Scanner
        Scanner.Reader = NewReader(sCommand);
    }

    public static Scanner NewScanner(String sCommand) {
      var reader = NewReader(sCommand);
      //return new StreamScanner((StreamReader)reader);
      return new Scanner(reader);
    }

    protected static TextReader NewReader(String sCommand) {
      //return new StreamReader(
      //  new MemoryStream(
      //    Encoding.UTF8.GetBytes(sCommand)
      //    ));
      return new StringReader(sCommand);
    }
    #endregion                          // Constructors

    #region IDisposable Interface
    public void Dispose() {
      Dispose(true);
      GC.SuppressFinalize(this);
    }

    protected virtual void Dispose(bool disposing) {
      if (!disposed) {
        if (disposing) Scanner?.Dispose();
        disposed = true;
      }
    }
    #endregion                          // IDisposable Interface

    #region Methods
    public void Close() {
      Scanner?.Close();
    }

    public Boolean AcceptEOL(String? sMethodName = default, Boolean bShowText = true) {
      if (Scanner is null)
        throw new ArgumentNullException(nameof(Scanner));

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
      var message = IsNullOrEmpty(sMethodName) ? sContext : $"{sMethodName} {sContext}";

      if (Scanner is null)
        throw new ArgumentNullException(nameof(Scanner));

      return Scanner.AppendDetails(message);
    }

    //[ToDo]Refactor top-level Command Loop
    public IEnumerable<object?> Parse() {
      if (Scanner is null)
        throw new ArgumentNullException(nameof(Scanner));

      while (!Scanner.EndOfStream) {
        // Preserve value of Text prior to the Scanner side-effects of the Accept Method
        var sText = Scanner.Text;

        var obj = ParseRow();
        var bAccepted = obj is not null;

        if (bAccepted)
          yield return obj;
        else {
          //[Test]
          //if (Log) {
          //  var message = $@"Rejected: ""{sText}""";
          //  if (bAccepted) LogLine(message);
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

    private String parseOptionName(out Control control) {
      SpaceToken.Accept();
      nameKeywordToken.Accept();        //[UCI]Technically, the option "name" keyword is required.

      String? sKeyword = default;
      String? sName = default;
      while (true) {
        SpaceToken.Accept();
        if (valueKeywordToken.Accept()) {
          sKeyword = valueKeywordToken.Value;
          break;
        }
        else if (optionToken.Accept()) {
          //[UCI]Accummulate space separated, compound names until a value keyword is found
          var sToken = optionToken.Value;
          sName = IsNullOrEmpty(sName) ? sToken : $"{sName} {sToken}";
        }
        else
          break;
      }

      control = Control.FindOption(GameState.Controls, sName);

      //
      // The "value" Keyword is returned only to indicate whether or not it was present.
      // Its own value was validated by valueKeywordRules.
      //
      return sKeyword;
    }

    private String? parseOptionValue(String? sKeyword) {
      String? sValue = default;
      if (sKeyword is not null) {
        SpaceToken.Accept();
        lineToken.Expect();
        sValue = lineToken.Value;
      }
      return sValue;
    }

    private static void rejectValue(String sKeyword) {
      if (sKeyword is not null)
        throw new ParseException($"Superfluous {sKeyword} keyword specified");
    }

    public void ParseGetOption() {
      var sKeyword = parseOptionName(out Control control);
      rejectValue(sKeyword);
      var setting = control.AsSetting();
      var s = setting.Value?.ToString();
      if (s is not null)
        LogLine(s);
    }

    public void ParseResetOption() {
      var sKeyword = parseOptionName(out Control control);
      rejectValue(sKeyword);
      var setting = control.AsSetting();
      setting.SetDefault();
    }

    public void ParseSetOption() {
      var sValueKeyword = parseOptionName(out Control control);
      control.SetValue(parseOptionValue(sValueKeyword));
    }

    public Boolean ParseList() {
      return SpaceToken.Accept() && movesKeywordToken.Accept();
    }

    protected static Position? findNamedPosition(Position? position, Position? parent) {
      while (position is not null && !ReferenceEquals(position, parent) && IsNullOrEmpty(position.Name))
        position = position.Parent;
      return position;
    }

    public void ParseTabiya(Position position) {
      var state = position.State;
      var named = findNamedPosition(position, state.RootPosition);
      var sName = named?.Name;
      if (!IsNullOrEmpty(sName)) {
        named.Display(sName);
        state.ListMovesFromParent(position, named, ParseList());
      }
    }

    public Position SetupPosition(Position position) {
      SpaceToken.Expect();
      setupTypeToken.Expect();

      var bFoundKeyword = true;
      var sKeyword = setupTypeToken.Value;
      switch (sKeyword.ToLower()) {
      case "epd":
        SpaceToken.Expect();
        SetupToken.Expect();
        var operations = ParseOperations();
        position.ParseEPD(SetupToken.Value, operations);
        break;
      case "fen":
        SpaceToken.Expect();
        SetupToken.Expect();
        var sHalfMoveCount = ParseCount("0");
        var sFullMoveNumber = ParseCount("1");
        position.ParseFEN(SetupToken.Value, sHalfMoveCount, sFullMoveNumber);
        position = parseMoves(position);
        break;
      case "random":
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
          //[Note]A list of moves is disallowed, because the choice of position was random
        }
        break;
      case "startpos":
        position.SetFEN();
        position = parseMoves(position);
        break;
      default:
        bFoundKeyword = false;
        break;
      }

      if (!bFoundKeyword)
        throw new ParseException($"Unknown {sKeyword} keyword");

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
        operations = new Dictionary<String, List<String>?>();
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
            var sOperands = operands is null ?
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
        operands = new List<String>();
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
      Dictionary<String, List<String>?> operations, String sOpcode, String? sDefault = default) {
      if (operations is null) return sDefault;

      operations.TryGetValue(sOpcode, out List<String>? operands);
      if (operands is null) return sDefault;

      if (operands.Count == 0)          // The Opcode is assumed to provide a Single Value, i.e., an Operand
        throw new ParseException($"EPD opcode {sOpcode} has no operand");
      else if (operands.Count > 1) {
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
    public static Byte ParseByte(String sName, String sValue) {
      if (Byte.TryParse(sValue, out Byte vValue)) return vValue;
      throw new ParseException($"Invalid {sName} = {sValue}");
    }

    public static UInt16 ParseUInt16(String sName, String sValue) {
      if (UInt16.TryParse(sValue, out UInt16 wValue)) return wValue;
      throw new ParseException($"Invalid {sName} = {sValue}");
    }

    public static UInt32 ParseUInt32(String sName, String sValue) {
      if (UInt32.TryParse(sValue, out UInt32 uValue)) return uValue;
      throw new ParseException($"Invalid {sName} = {sValue}");
    }

    public static UInt64 ParseUInt64(String sName, String sValue) {
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
    #endregion                          // Numeric Parsers

    #region Verbatim Literal Methods
    public static Boolean IsVerbatimLiteral(String sValue) {
      return !IsNullOrEmpty(sValue) && sValue[0] == cDoubleQuote;
    }

    public static String VerbatimLiteralToString(String sValue) {
      if (!IsVerbatimLiteral(sValue))
        throw new ArgumentException(nameof(sValue), "Not a verbatim literal");

      if (sValue.Length < 2 || sValue[sValue.Length - 1] != cDoubleQuote)
        throw new ArgumentException(nameof(sValue), "Improperly delimited verbatim literal");

      var sBody = sValue.Substring(1, sValue.Length - 2);
      var split = Regex.Split(sBody, sEscapedDoubleQuote);

      if (split.Any(s => s.Contains(cDoubleQuote)))
        throw new ArgumentException(nameof(sValue), "Contains an improperly escaped double quote");

      return Join(sDoubleQuote, split);
    }

    public static String StringToVerbatimLiteral(String sInput) {
      return $@"""{sInput.Replace(sDoubleQuote, sEscapedDoubleQuote)}""";
    }
    #endregion                          // Verbatim Literal Methods
    #endregion                          // Methods
  }
}
