//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
namespace SortTest.Exceptions;

public class CommandException : ApplicationException {
  #region Constructors
  public CommandException() {
  }

  public CommandException(String message)
    : base(message) {
  }

  public CommandException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
