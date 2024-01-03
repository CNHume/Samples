//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
using System.Runtime.Serialization;

namespace Exceptions;
[Serializable]
class PositionException : BoardException, ISerializable {
  #region Constructors
  public PositionException() {
  }
  public PositionException(String message)
    : base(message) {
  }
  public PositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected PositionException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class ParsePositionException : PositionException, ISerializable {
  #region Constructors
  public ParsePositionException() {
  }
  public ParsePositionException(String message)
    : base(message) {
  }
  public ParsePositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected ParsePositionException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class InvalidPositionException : PositionException, ISerializable {
  #region Constructors
  public InvalidPositionException() {
  }
  public InvalidPositionException(String message)
    : base(message) {
  }
  public InvalidPositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected InvalidPositionException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class PerftException : PositionException, ISerializable {
  #region Constructors
  public PerftException() {
  }
  public PerftException(String message)
    : base(message) {
  }
  public PerftException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected PerftException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class FinalPositionException : PositionException, ISerializable {
  #region Constructors
  public FinalPositionException() {
  }
  public FinalPositionException(String message)
    : base(message) {
  }
  public FinalPositionException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected FinalPositionException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}
