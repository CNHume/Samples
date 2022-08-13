//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
//[2016-09-04 CNHume]Created Class
//
// Conditionals:
//
#define IgnoreSpace

namespace Command {
  using Exceptions;

  using System;
  using System.Linq;

  using static Logging.Logger;
  using static System.String;

  //
  // Adapted from the answer given by Paul Hollingsworth to the
  // Stack Overflow question entitled Poor man's “lexer” for C#
  //
  // See http://stackoverflow.com/questions/673113/poor-mans-lexer-for-c-sharp
  //
  class Token {
    #region Properties
    public Parser Parser { get; }
    private TokenRule[] TokenRules { get; }
    public TokenType TokenType { get; }
    public TokenRuleType TokenRuleType { get; private set; }
    public String Value { get; private set; }
    public Boolean IsVerbose => Parser?.IsVerbose == true;
    #endregion

    #region Constructors
    public Token(Parser parser, TokenType tokenType, TokenRule[] tokenRules) {
      Parser = parser;
      TokenRules = tokenRules;
      TokenType = tokenType;
    }
    #endregion

    #region Methods
    public Boolean Accept(Boolean bShowMatch = true) {
      return Parser?.Scanner?.Text is null ?
        false : TokenRules.Any(tokenRule => match(Parser.Scanner, tokenRule, bShowMatch));
    }

    private Boolean match(Scanner scanner, TokenRule tokenRule, Boolean bShowMatch = true) {
      var match = tokenRule.Match(scanner.Text);
      if (match.Success) {
        TokenRuleType = tokenRule.TokenRuleType;
        Value = match.Value;
        scanner.Skip(match.Length);
        if (bShowMatch && IsVerbose) {
          switch (tokenRule.TokenRuleType) {
#if IgnoreSpace
          case TokenRuleType.opcodeDelimiter:
          case TokenRuleType.eol:
          case TokenRuleType.space:
            break;
#endif
          default:
            LogLine($@"Matched {TokenRuleType} ""{Value}""");
            break;
          }
        }
      }

      return match.Success;
    }

    public void Expect(Boolean bShowMatch = true) {
      if (Accept(bShowMatch)) return;

      var scanner = Parser?.Scanner;
      var type = GetType();
      var message = IsNullOrEmpty(scanner?.Text) ?
        $"{TokenType} expected" : $@"The {TokenType} {type.Name} does not accept ""{scanner.Text}""";
      throw new ChessException(message);
    }
    #endregion
  }
}
