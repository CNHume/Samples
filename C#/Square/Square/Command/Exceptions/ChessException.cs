//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-09-16 CNHume]Created Class
//
// Conditionals:
//

using System.Runtime.Serialization;

namespace Exceptions;
[Serializable]
class ChessException : ApplicationException, ISerializable {
  #region Constructors
  public ChessException() {
  }
  public ChessException(String message)
    : base(message) {
  }
  public ChessException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected ChessException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class ControlException : ChessException, ISerializable {
  #region Constructors
  public ControlException() {
  }
  public ControlException(String message)
    : base(message) {
  }
  public ControlException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected ControlException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class ParseException : ChessException, ISerializable {
  #region Constructors
  public ParseException() {
  }
  public ParseException(String message)
    : base(message) {
  }
  public ParseException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected ParseException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}

[Serializable]
class MoveException : ChessException, ISerializable {
  #region Constructors
  public MoveException() {
  }
  public MoveException(String message)
    : base(message) {
  }
  public MoveException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected MoveException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}
