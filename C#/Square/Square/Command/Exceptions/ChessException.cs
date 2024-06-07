//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2014-09-16 CNHume]Created Class
//
// Conditionals:
//
namespace Exceptions;

class ChessException : ApplicationException {
  #region Constructors
  public ChessException() {
  }

  public ChessException(String message)
    : base(message) {
  }

  public ChessException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class ControlException : ChessException {
  #region Constructors
  public ControlException() {
  }

  public ControlException(String message)
    : base(message) {
  }

  public ControlException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class ParseException : ChessException {
  #region Constructors
  public ParseException() {
  }

  public ParseException(String message)
    : base(message) {
  }

  public ParseException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class MoveException : ChessException {
  #region Constructors
  public MoveException() {
  }

  public MoveException(String message)
    : base(message) {
  }

  public MoveException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
