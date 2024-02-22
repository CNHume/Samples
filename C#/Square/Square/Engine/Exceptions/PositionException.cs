//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
namespace Exceptions;

class PositionException : BoardException {
  #region Constructors
  public PositionException() {
  }

  public PositionException(String message)
    : base(message) {
  }

  public PositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class ParsePositionException : PositionException {
  #region Constructors
  public ParsePositionException() {
  }

  public ParsePositionException(String message)
    : base(message) {
  }

  public ParsePositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class InvalidPositionException : PositionException {
  #region Constructors
  public InvalidPositionException() {
  }

  public InvalidPositionException(String message)
    : base(message) {
  }

  public InvalidPositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class PerftException : PositionException {
  #region Constructors
  public PerftException() {
  }

  public PerftException(String message)
    : base(message) {
  }

  public PerftException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class FinalPositionException : PositionException {
  #region Constructors
  public FinalPositionException() {
  }

  public FinalPositionException(String message)
    : base(message) {
  }

  public FinalPositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
