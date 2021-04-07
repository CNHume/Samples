//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2016-09-05 CNHume]Created File
//

namespace Command {
  using Engine;

  using Exceptions;

  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text.RegularExpressions;

  using static Logging.Logger;
  using static System.String;

  partial class Parser : IDisposable {
    #region Constants
    protected const String sSpace = " ";
    protected const String sHyphen = "-";
    protected const Char cDoubleQuote = '"';
    protected const String sDoubleQuote = @"""";
    protected const String sEscapedDoubleQuote = @"""""";
    #endregion

    #region Fields
    private Boolean disposed = false;
    private Scanner scanner;

    protected static readonly TokenRule[] codeTokenRules;
    protected static readonly TokenRule[] operandDelimiterTokenRules;
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

    protected readonly Lexeme codeLexeme;
    protected readonly Lexeme eolLexeme;
    protected readonly Lexeme operandDelimiterLexeme;
    protected readonly Lexeme lineLexeme;
    public readonly Lexeme SpaceLexeme;

    protected readonly Lexeme enableKeywordLexeme;
    public readonly Lexeme GoKeywordLexeme;
    protected readonly Lexeme movesKeywordLexeme;
    protected readonly Lexeme nameKeywordLexeme;
    protected readonly Lexeme opcodeLexeme;
    protected readonly Lexeme operandLexeme;
    protected readonly Lexeme optionLexeme;
    public readonly Lexeme PACNMoveLexeme;
    public readonly Lexeme RegisterKeywordLexeme;
    public readonly Lexeme SetupLexeme;
    protected readonly Lexeme setupTypeLexeme;
    public readonly Lexeme CountLexeme;
    public readonly Lexeme UnsignedLexeme;
    protected readonly Lexeme valueKeywordLexeme;
    protected readonly Lexeme verbLexeme;
    #endregion

    #region Properties
    public Scanner Scanner {
      get => scanner;
      set {
        scanner?.Dispose();
        scanner = value;
      }
    }

    public Boolean IsVerbose { get; }
    #endregion

    #region Constructors

    public Parser(Boolean isVerbose = false) {
      IsVerbose = isVerbose;

      codeLexeme = new Lexeme(this, codeTokenRules, "code");
      eolLexeme = new Lexeme(this, eolTokenRules, "eol");
      operandDelimiterLexeme = new Lexeme(this, operandDelimiterTokenRules, "operandDelimiter");
      lineLexeme = new Lexeme(this, lineTokenRules, "line");
      SpaceLexeme = new Lexeme(this, spaceTokenRule, "space");

      GoKeywordLexeme = new Lexeme(this, goKeywordTokenRules, "goKeyword");
      enableKeywordLexeme = new Lexeme(this, enableKeywordTokenRules, "enableKeyword");
      movesKeywordLexeme = new Lexeme(this, movesKeyworTokendRules, "moveKeyword");
      nameKeywordLexeme = new Lexeme(this, nameKeywordTokenRules, "nameKeyword");
      opcodeLexeme = new Lexeme(this, opcodeTokenRules, "opcode");
      operandLexeme = new Lexeme(this, operandTokenRules, "operand");
      optionLexeme = new Lexeme(this, optionTokenRules, "option");
      PACNMoveLexeme = new Lexeme(this, pacnMoveTokenRules, "pacnMove");
      RegisterKeywordLexeme = new Lexeme(this, registerKeywordTokenRules, "registerKeyword");
      SetupLexeme = new Lexeme(this, setupTokenRules, "setup");
      setupTypeLexeme = new Lexeme(this, setupTypeTokenRules, "setupType");
      CountLexeme = new Lexeme(this, countTokenRules, "counter");
      UnsignedLexeme = new Lexeme(this, unsignedTokenRules, "unsigned");
      valueKeywordLexeme = new Lexeme(this, valueKeywordTokenRules, "valueKeyword");
      verbLexeme = new Lexeme(this, verbTokenRules, "verb");
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
      codeTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.code, @"[-\w]+\b"),
      };
      operandDelimiterTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.delimiter, @"\s*;")
      };
      eolTokenRules = new TokenRule[] {
        new TokenRule(TokenRuleType.eol, @"\s*$")
      };
      lineTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.line, @".*$")
      };
      spaceTokenRule = new TokenRule[] {
          new TokenRule(TokenRuleType.space, @"\s+")
      };
      verbTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.verb,
                   @"(best|board|debug|exit|getoption|go|isready|list|moves|perft|ponderhit|position|quit|status|register|reset|resetoption|setoption|status|stop|tabiya|test|testepd|timertests|uci|ucinewgame|unmove)\b",
          RegexOptions.IgnoreCase)
      };
      enableKeywordTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.enableKeyword, @"(on|off)\b", RegexOptions.IgnoreCase)
      };
      goKeywordTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.goKeyword, @"(searchmoves|ponder|wtime|btime|winc|binc|movestogo|depth|nodes|mate|movetime|infinite)\b", RegexOptions.IgnoreCase)
      };
      registerKeywordTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.registerKeyword, @"(later|code|name)\b", RegexOptions.IgnoreCase)
      };
      setupTypeTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.setupType, @"(fen|epd|startpos|random)\b", RegexOptions.IgnoreCase)
      };
      setupTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.setup, @"[pnbrqkPNBRQK1-8]{0,8}(/[pnbrqkPNBRQK1-8]{0,8}){7}(\s+[wb](\s+(-|[KQkq|A-H|a-h]{1,4})(\s+(-|[a-h][36]\b))?)?)?")
      };
      opcodeTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.opcode, @"[a-zA-Z]\w{0,14}\b"),
      };
      operandTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.@float, @"[+-](0|[1-9]\d*)(\.\d+)?"),
          new TokenRule(TokenRuleType.unsigned, @"0|[1-9]\d*"),
          new TokenRule(TokenRuleType.sanMove, @"([NBRQK]?[a-h]?[1-8]?[x-]?[a-h][1-8](=[NBRQ])?|O-O|O-O-O)\b[+#]?", RegexOptions.IgnoreCase),
          new TokenRule(TokenRuleType.@string, @"""([^""]|"""")*""")
      };
      optionTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.option, @"[a-zA-Z]\w*\b"),
      };
      countTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.hyphen, @"-"),
          new TokenRule(TokenRuleType.unsigned, @"(0|[1-9]\d*)")
      };
      unsignedTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.unsigned, @"(0|[1-9]\d*)")
      };
      movesKeyworTokendRules = new TokenRule[] {
          new TokenRule(TokenRuleType.movesKeyword, @"moves\b", RegexOptions.IgnoreCase)
      };
      nameKeywordTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.nameKeyword, @"name\b", RegexOptions.IgnoreCase)
      };
      //
      // UCI moves are expressed in Pure Algebraic Coordinate Notation (PACN):
      // See https://www.chessprogramming.org/Algebraic_Chess_Notation
      //
      // PACN is similar to a notation attributed to Warren Smith:
      // Regex for Smith Notation (SN): ([a-h][1-8]){2}[pnbrqkEcC]?[NBRQ]?
      // Lowercase is used to render captures "reversible" in SN.  SN uses uppercase for promotions, where PACN uses lowercase.
      //
      pacnMoveTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.pacnMove, @"([a-h][1-8]){2}[nbrq]?|0000|000?|OOO?|O-O(-O)?", RegexOptions.IgnoreCase)
      };
      valueKeywordTokenRules = new TokenRule[] {
          new TokenRule(TokenRuleType.valueKeyword, @"(=|value\b)", RegexOptions.IgnoreCase)
      };
    }
    #endregion

    #region IDisposable Methods
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
    #endregion

    #region Methods
    public void Close() {
      Scanner.Close();
    }

    public Boolean AcceptEOL(String sMethodName = default) {
      var bAccepted = Scanner.EndOfLine || eolLexeme.Accept();
      if (IsVerbose && !bAccepted)
        LogLine(eolMessage(sMethodName));
      return bAccepted;
    }

    public void ExpectEOL(String sMethodName = default) {
      if (!AcceptEOL(sMethodName))
        throw new ParseException(eolMessage(sMethodName));
    }

    private String eolMessage(String sMethodName = default) {
      const String sContext = "Could not parse text at End of Line";
      var message = IsNullOrEmpty(sMethodName) ? sContext : $"{sMethodName} {sContext}";
      return Scanner.AppendDetails(message);
    }

    //[ToDo]Refactor top-level Command Loop
    public List<object> Parse() {
      var objects = new List<object>();

      while (!Scanner.EndOfStream) {
        // Preserve value of Text prior to the Scanner side-effects of the Accept Method
        var sText = Scanner.Text;

        var obj = ParseRow();
        var bAccepted = obj is not null;

        //[Debug]
        //if (Log) {
        //  var result = bAccepted ? "Accepted" : "Rejected";
        //  var message = $@"{result}: ""{sText}""";
        //  if (bAccepted) LogLine(message);
        //}

        if (bAccepted)
          objects.Add(obj);
        else {
          // Skip past any Row that cannot be parsed.
          Scanner.ReadLine();
        }
      }

      return objects;
    }

    //[ToDo]
    public object ParseRow() {
      object obj = default;
      return obj;
    }

    public String ParseVerb() {
      SpaceLexeme.Accept();
      if (AcceptEOL())
        return Empty;

      verbLexeme.Expect();
      return verbLexeme.Value;
    }

    private String parseOptionName(out Control control) {
      var sKeyword = (String)null;
      SpaceLexeme.Accept();
      nameKeywordLexeme.Accept();        //[UCI]Technically, the option "name" keyword is required.

      var sName = (String)null;
      while (true) {
        SpaceLexeme.Accept();
        if (valueKeywordLexeme.Accept()) {
          sKeyword = valueKeywordLexeme.Value;
          break;
        }
        else if (optionLexeme.Accept()) {
          //[UCI]Accummulate space separated, compound names until a value keyword is found
          var sToken = optionLexeme.Value;
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

    private String parseOptionValue(String sKeyword) {
      var sValue = (String)null;
      if (sKeyword is not null) {
        SpaceLexeme.Accept();
        lineLexeme.Expect();
        sValue = lineLexeme.Value;
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
      LogLine(setting.Value.ToString());
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
      return SpaceLexeme.Accept() && movesKeywordLexeme.Accept();
    }

    protected static Position findNamedPosition(Position position, Position parent) {
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

    public Position ParsePosition(Position position) {
      SpaceLexeme.Expect();
      setupTypeLexeme.Expect();

      var bFoundKeyword = true;
      var sKeyword = setupTypeLexeme.Value;
      switch (sKeyword.ToLower()) {
      case "epd":
        SpaceLexeme.Expect();
        SetupLexeme.Expect();
        var operations = ParseOperations();
        position.ParseEPD(SetupLexeme.Value, operations);
        break;
      case "fen":
        SpaceLexeme.Expect();
        SetupLexeme.Expect();
        var sHalfMoveCount = ParseCount("0");
        var sFullMoveNumber = ParseCount("1");
        position.ParseFEN(SetupLexeme.Value, sHalfMoveCount, sFullMoveNumber);
        position = parseMoves(position);
        break;
      case "random":
        UInt16 wChess960;
        if (SpaceLexeme.Accept() && UnsignedLexeme.Accept()) {
          wChess960 = ParseUInt16("Chess960 Index", UnsignedLexeme.Value);
          position.SetFischerRandom(wChess960);
          position = parseMoves(position);
        }
        else {
          //
          // The following generates a random Chess960 Index using a SeededRandom instance:
          //
          wChess960 = (UInt16)position.State.SeededRandom.Next(960);
          position.SetFischerRandom(wChess960);
          //[Note]A list of moves is disallowed here, because the choice of position is random
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
      if (SpaceLexeme.Accept() && CountLexeme.Accept()) {
        var sValue = CountLexeme.Value;
        return sValue == sHyphen ? sDefault : sValue;
      }
      return sDefault;
    }

    private Position parseMoves(Position position) {
      if (SpaceLexeme.Accept() && movesKeywordLexeme.Accept()) {
        position = position.ParsePACNMakeMoves(this);
      }
      return position;
    }

    public Dictionary<String, List<String>> ParseOperations() {
      var operations = (Dictionary<String, List<String>>)null;
      if (SpaceLexeme.Accept()) {
        operations = new Dictionary<String, List<String>>();
        while (opcodeLexeme.Accept()) {
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
          var sOpcode = opcodeLexeme.Value;
          var operands = parseOperands(operations, sOpcode);

          if (operations.ContainsKey(sOpcode)) {
            var sOperands = Join(sSpace, operands);
            throw new ParseException($"EPD opcode already specified for: {sOpcode} {sOperands}");
          }

          operations.Add(sOpcode, operands);
          if (!SpaceLexeme.Accept()) break;
        }
      }

      return operations;
    }

    private List<String> parseOperands(
      Dictionary<String, List<String>> operations, String sOpcode) {
      var operands = (List<String>)null;
      if (SpaceLexeme.Accept()) {
        operands = new List<String>();
        while (operandLexeme.Accept()) {
          operands.Add(operandLexeme.Value);
          if (operandDelimiterLexeme.Accept()) break;
          if (AcceptEOL()) throw new ParseException($"Undelimited {sOpcode} operand");
          SpaceLexeme.Expect();
        }
      }
      return operands;
    }

    public static String GetSingleValue(
      Dictionary<String, List<String>> operations, String sOpcode, String sDefault = default) {
      if (operations is null) return sDefault;

      operations.TryGetValue(sOpcode, out List<String> operands);
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
      SpaceLexeme.Expect();
      codeLexeme.Expect();
      return codeLexeme.Value;
    }
    #endregion

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

      SpaceLexeme.Expect();
      enableKeywordLexeme.Expect();

      var sKeyword = enableKeywordLexeme.Value;
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
    #endregion

    #region Verbatim Literal Methods
    public static Boolean IsVerbatimLiteral(String sValue) {
      return !IsNullOrEmpty(sValue) && sValue[0] == cDoubleQuote;
    }

    public static String VerbatimLiteralToString(String sValue) {
      if (!IsVerbatimLiteral(sValue))
        throw new ArgumentException("String is not a verbatim literal");

      if (sValue.Length < 2 || sValue[sValue.Length - 1] != cDoubleQuote)
        throw new ArgumentException("Improperly delimited verbatim literal");

      var sBody = sValue.Substring(1, sValue.Length - 2);
      var split = Regex.Split(sBody, sEscapedDoubleQuote);

      if (split.Any(s => s.Contains(cDoubleQuote)))
        throw new ArgumentException("Improperly escaped double quote");

      return Join(sDoubleQuote, split);
    }

    public static String StringToVerbatimLiteral(String sInput) {
      return $@"""{sInput.Replace(sDoubleQuote, sEscapedDoubleQuote)}""";
    }
    #endregion
  }
}
