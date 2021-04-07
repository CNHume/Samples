namespace Command {
  using System;

  #region Enumerations
  enum TokenRuleType : byte {
    None = 0,
    code,
    delimiter,
    enableKeyword,
    eol,
    @float,
    goKeyword,
    hyphen,
    line,
    movesKeyword,
    nameKeyword,
    pacnMove,
    opcode,
    option,
    registerKeyword,
    rootKeyword,
    sanMove,
    setup,
    setupType,
    space,
    @string,
    unsigned,
    valueKeyword,
    verb
  }
  #endregion
}
