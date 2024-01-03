//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
using System.Runtime.Serialization;

namespace Exceptions;
[Serializable]
class BoardException : ApplicationException, ISerializable {
  #region Constructors
  public BoardException() {
  }
  public BoardException(String message)
    : base(message) {
  }
  public BoardException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected BoardException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class PieceException : BoardException, ISerializable {
  #region Constructors
  public PieceException() {
  }
  public PieceException(String message)
    : base(message) {
  }
  public PieceException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected PieceException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class ColorException : PieceException, ISerializable {
  #region Constructors
  public ColorException() {
  }
  public ColorException(String message)
    : base(message) {
  }
  public ColorException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected ColorException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}
