//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
//[2016-09-04 CNHume]Created Class
//
// Conditionals:
//
#define DebugLexeme

namespace Command {
  using Exceptions;
  using static Logging.Logger;

  using System;
  using static System.String;

  //
  // Adapted from the answer given by Paul Hollingsworth to the
  // Stack Overflow question entitled Poor man's “lexer” for C#
  //
  // See http://stackoverflow.com/questions/673113/poor-mans-lexer-for-c-sharp
  //
  class Lexeme {
    #region Properties
    public Parser Parser { get; }
    private Rule[] Rules { get; }
    public String Name { get; }
    public RuleType RuleType { get; private set; }
    public String Value { get; private set; }
    public Boolean IsVerbose => Parser is not null && Parser.IsVerbose;
    #endregion

    #region Constructors
    public Lexeme(Parser parser, Rule[] rules, String sName) {
      Parser = parser;
      Rules = rules;
      Name = sName;
    }
    #endregion

    #region Methods
    public Boolean Accept() {
      var scanner = Parser.Scanner;
      if (scanner.Text is not null) {
        foreach (var rule in Rules) {
          var match = rule.Match(scanner.Text);

          if (match.Success) {
            RuleType = rule.Type;
            Value = match.Value;
            scanner.Skip(match.Length);
#if DebugLexeme
            if (IsVerbose) {
              switch (rule.Type) {
              case RuleType.delimiter:
              case RuleType.eol:
              case RuleType.space:
                break;
              default:
                LogLine($@"Matched {RuleType} ""{Value}""");
                break;
              }
            }
#endif
            return true;
          }
        }
      }

      return false;
    }

    public void Expect() {
      if (Accept()) return;

      var scanner = Parser.Scanner;
      var type = GetType();
      var message = IsNullOrEmpty(scanner.Text) ?
        $"{Name} expected" : $@"The {Name} {type.Name} does not accept ""{scanner.Text}""";
      throw new ChessException(message);
    }

    public override String ToString() {
      return Name;
    }
    #endregion
  }
}
