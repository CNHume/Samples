//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
namespace Fermat.Exceptions;

class CommandException : ApplicationException {
  #region Constructors
  public CommandException() {
  }

  public CommandException(string message)
    : base(message) {
  }

  public CommandException(string message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
