//
// Copyright (C) 2019-2025, Christopher N. Hume.  All rights reserved.
//
namespace Fermat.Exceptions;

class ParseException : ApplicationException {
  #region Constructors
  public ParseException() {
  }

  public ParseException(string message)
    : base(message) {
  }

  public ParseException(string message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
