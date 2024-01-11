//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
namespace Exceptions;
class BoardException : ApplicationException {
  #region Constructors
  public BoardException() {
  }

  public BoardException(String message)
    : base(message) {
  }

  public BoardException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors

}
class PieceException : BoardException {
  #region Constructors
  public PieceException() {
  }

  public PieceException(String message)
    : base(message) {
  }

  public PieceException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
class ColorException : PieceException {
  #region Constructors
  public ColorException() {
  }

  public ColorException(String message)
    : base(message) {
  }

  public ColorException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
