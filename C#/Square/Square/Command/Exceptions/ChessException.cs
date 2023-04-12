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
  public ChessException(string message)
    : base(message) {
  }
  public ChessException(string message, Exception inner)
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
  public ControlException(string message)
    : base(message) {
  }
  public ControlException(string message, Exception inner)
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
  public ParseException(string message)
    : base(message) {
  }
  public ParseException(string message, Exception inner)
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
  public MoveException(string message)
    : base(message) {
  }
  public MoveException(string message, Exception inner)
    : base(message, inner) {
  }
  #endregion

  #region ISerializable Interface
  protected MoveException(SerializationInfo info, StreamingContext context)
    : base(info, context) {
  }
  #endregion
}
