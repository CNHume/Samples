//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace Engine.Exceptions {
  using System;
  using System.Runtime.Serialization;

  [Serializable]
  class PositionException : BoardException, ISerializable {
    #region Constructors
    public PositionException() {
    }
    public PositionException(string message)
      : base(message) {
    }
    public PositionException(string message, Exception inner)
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
    public ParsePositionException(string message)
      : base(message) {
    }
    public ParsePositionException(string message, Exception inner)
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
    public InvalidPositionException(string message)
      : base(message) {
    }
    public InvalidPositionException(string message, Exception inner)
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
    public PerftException(string message)
      : base(message) {
    }
    public PerftException(string message, Exception inner)
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
    public FinalPositionException(string message)
      : base(message) {
    }
    public FinalPositionException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected FinalPositionException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }
}
