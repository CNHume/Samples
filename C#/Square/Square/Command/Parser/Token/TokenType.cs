namespace Command {
  partial class Parser : IDisposable {
    #region Enumerations
    internal enum TokenType : byte {
      None = 0,
      code,
      eol,
      opcodeDelimiter,
      line,
      space,
      goKeyword,
      enableKeyword,
      movesKeyword,
      nameKeyword,
      opcode,
      operand,
      option,
      pacnMove,
      registerKeyword,
      setup,
      setupType,
      counter,
      unsigned,
      valueKeyword,
      verb
    }
    #endregion
  }
}
