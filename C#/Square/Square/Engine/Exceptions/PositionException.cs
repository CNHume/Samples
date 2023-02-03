//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
using System.Runtime.Serialization;

namespace Engine.Exceptions {
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
