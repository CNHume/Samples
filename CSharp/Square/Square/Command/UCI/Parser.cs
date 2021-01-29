//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2016-09-05 CNHume]Created File
//

namespace Command {
  using Engine;
  using Exceptions;
  using static Logging.Logger;

  using System;
  using System.Collections.Generic;
  using System.Linq;
  using static System.String;
  using System.Text.RegularExpressions;

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

    protected static readonly Rule[] codeRules;
    protected static readonly Rule[] delimiterRule;
    protected static readonly Rule[] eolRules;
    protected static readonly Rule[] lineRules;
    protected static readonly Rule[] spaceRule;

    protected static readonly Rule[] enableKeywordRules;
    protected static readonly Rule[] registerKeywordRules;
    protected static readonly Rule[] goKeywordRules;
    protected static readonly Rule[] movesKeywordRules;
    protected static readonly Rule[] nameKeywordRules;
    protected static readonly Rule[] opcodeRules;
    protected static readonly Rule[] operandRules;
    protected static readonly Rule[] optionRules;
    protected static readonly Rule[] pacnMoveRules;
    protected static readonly Rule[] setupRules;
    protected static readonly Rule[] setupTypeRules;
    protected static readonly Rule[] countRules;
    protected static readonly Rule[] unsignedRules;
    protected static readonly Rule[] valueKeywordRules;
    protected static readonly Rule[] verbRules;

    protected Lexeme codeLexeme;
    protected Lexeme delimiterLexeme;
    protected Lexeme eolLexeme;
    protected Lexeme lineLexeme;
    public Lexeme SpaceLexeme;

    protected Lexeme enableKeywordLexeme;
    public Lexeme GoKeywordLexeme;
    protected Lexeme movesKeywordLexeme;
    protected Lexeme nameKeywordLexeme;
    protected Lexeme opcodeLexeme;
    protected Lexeme operandLexeme;
    protected Lexeme optionLexeme;
    public Lexeme PACNMoveLexeme;
    public Lexeme RegisterKeywordLexeme;
    public Lexeme SetupLexeme;
    protected Lexeme setupTypeLexeme;
    public Lexeme CountLexeme;
    public Lexeme UnsignedLexeme;
    protected Lexeme valueKeywordLexeme;
    protected Lexeme verbLexeme;
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
      codeRules = new Rule[] {
          new Rule(RuleType.code, @"[-\w]+\b"),
      };
      delimiterRule = new Rule[] {
          new Rule(RuleType.delimiter, @"\s*;")
      };
      eolRules = new Rule[] {
        new Rule(RuleType.eol, @"\s*$")
      };
      lineRules = new Rule[] {
          new Rule(RuleType.line, @".*$")
      };
      spaceRule = new Rule[] {
          new Rule(RuleType.space, @"\s+")
      };
      verbRules = new Rule[] {
          new Rule(RuleType.verb,
                   @"(best|board|debug|exit|getoption|go|isready|list|moves|perft|ponderhit|position|quit|status|register|reset|resetoption|setoption|status|stop|tabiya|test|testepd|timertests|uci|ucinewgame|unmove)\b",
          RegexOptions.IgnoreCase)
      };
      enableKeywordRules = new Rule[] {
          new Rule(RuleType.enableKeyword, @"(on|off)\b", RegexOptions.IgnoreCase)
      };
      goKeywordRules = new Rule[] {
          new Rule(RuleType.goKeyword, @"(searchmoves|ponder|wtime|btime|winc|binc|movestogo|depth|nodes|mate|movetime|infinite)\b", RegexOptions.IgnoreCase)
      };
      registerKeywordRules = new Rule[] {
          new Rule(RuleType.registerKeyword, @"(later|code|name)\b", RegexOptions.IgnoreCase)
      };
      setupTypeRules = new Rule[] {
          new Rule(RuleType.setupType, @"(fen|epd|startpos|random)\b", RegexOptions.IgnoreCase)
      };
      setupRules = new Rule[] {
          new Rule(RuleType.setup, @"[pnbrqkPNBRQK1-8]{0,8}(/[pnbrqkPNBRQK1-8]{0,8}){7}(\s+[wb](\s+(-|[KQkq|A-H|a-h]{1,4})(\s+(-|[a-h][36]\b))?)?)?")
      };
      opcodeRules = new Rule[] {
          new Rule(RuleType.opcode, @"[a-zA-Z]\w{0,14}\b"),
      };
      operandRules = new Rule[] {
          new Rule(RuleType.@float, @"[+-](0|[1-9]\d*)(\.\d+)?"),
          new Rule(RuleType.unsigned, @"0|[1-9]\d*"),
          new Rule(RuleType.sanMove, @"([NBRQK]?[a-h]?[1-8]?[x-]?[a-h][1-8](=[NBRQ])?|O-O|O-O-O)\b[+#]?", RegexOptions.IgnoreCase),
          new Rule(RuleType.@string, @"""([^""]|"""")*""")
      };
      optionRules = new Rule[] {
          new Rule(RuleType.option, @"[a-zA-Z]\w*\b"),
      };
      countRules = new Rule[] {
          new Rule(RuleType.hyphen, @"-"),
          new Rule(RuleType.unsigned, @"(0|[1-9]\d*)")
      };
      unsignedRules = new Rule[] {
          new Rule(RuleType.unsigned, @"(0|[1-9]\d*)")
      };
      movesKeywordRules = new Rule[] {
          new Rule(RuleType.movesKeyword, @"moves\b", RegexOptions.IgnoreCase)
      };
      nameKeywordRules = new Rule[] {
          new Rule(RuleType.nameKeyword, @"name\b", RegexOptions.IgnoreCase)
      };
      //
      // UCI moves are expressed in Pure Algebraic Coordinate Notation (PACN):
      // See https://www.chessprogramming.org/Algebraic_Chess_Notation
      //
      // PACN is similar to a notation attributed to Warren Smith:
      // Regex for Smith Notation (SN): ([a-h][1-8]){2}[pnbrqkEcC]?[NBRQ]?
      // Lowercase is used to render captures "reversible" in SN.  SN uses uppercase for promotions, where PACN uses lowercase.
      //
      pacnMoveRules = new Rule[] {
          new Rule(RuleType.pacnMove, @"([a-h][1-8]){2}[nbrq]?|0000|000?|OOO?|O-O(-O)?", RegexOptions.IgnoreCase)
      };
      valueKeywordRules = new Rule[] {
          new Rule(RuleType.valueKeyword, @"(=|value\b)", RegexOptions.IgnoreCase)
      };
    }

    protected void Init() {
      codeLexeme = new Lexeme(this, codeRules, "code");
      delimiterLexeme = new Lexeme(this, delimiterRule, "delimiter");
      eolLexeme = new Lexeme(this, eolRules, "eol");

      lineLexeme = new Lexeme(this, lineRules, "line");
      SpaceLexeme = new Lexeme(this, spaceRule, "space");

      GoKeywordLexeme = new Lexeme(this, goKeywordRules, "goKeyword");
      enableKeywordLexeme = new Lexeme(this, enableKeywordRules, "enableKeyword");
      movesKeywordLexeme = new Lexeme(this, movesKeywordRules, "moveKeyword");
      nameKeywordLexeme = new Lexeme(this, nameKeywordRules, "nameKeyword");
      opcodeLexeme = new Lexeme(this, opcodeRules, "opcode");
      operandLexeme = new Lexeme(this, operandRules, "operand");
      optionLexeme = new Lexeme(this, optionRules, "option");
      PACNMoveLexeme = new Lexeme(this, pacnMoveRules, "pacnMove");
      RegisterKeywordLexeme = new Lexeme(this, registerKeywordRules, "registerKeyword");
      SetupLexeme = new Lexeme(this, setupRules, "setup");
      setupTypeLexeme = new Lexeme(this, setupTypeRules, "setupType");
      CountLexeme = new Lexeme(this, countRules, "counter");
      UnsignedLexeme = new Lexeme(this, unsignedRules, "unsigned");
      valueKeywordLexeme = new Lexeme(this, valueKeywordRules, "valueKeyword");
      verbLexeme = new Lexeme(this, verbRules, "verb");
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
        position.SetRandom();
        //[Note]A list of moves is not allowed, because the position is non-deterministic
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
      if (SpaceLexeme.Accept()) {
        if (movesKeywordLexeme.Accept()) {
          position = position.ParsePACNMakeMoves(this);
        }
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
          if (delimiterLexeme.Accept()) break;
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
