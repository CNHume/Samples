//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
namespace Fermat.Exceptions;

class ValidationException : ApplicationException {
  #region Constructors
  public ValidationException() {
  }

  public ValidationException(string message)
    : base(message) {
  }

  public ValidationException(string message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
