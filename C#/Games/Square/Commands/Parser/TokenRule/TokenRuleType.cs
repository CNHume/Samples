namespace Commands;

partial class Parser : IDisposable {
  #region Enumerations
  internal enum TokenRuleType : byte {
    None = 0,
    code,
    opcodeDelimiter,
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
