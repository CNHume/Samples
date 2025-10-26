//
// Copyright (C) 2019-2025, Christopher N. Hume.  All rights reserved.
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
