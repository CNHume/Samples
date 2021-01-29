//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace Engine.Exceptions {
  using System;
  using System.Runtime.Serialization;

  [Serializable]
  class BoardException : ApplicationException, ISerializable {
    #region Constructors
    public BoardException() {
    }
    public BoardException(string message)
      : base(message) {
    }
    public BoardException(string message, Exception inner)
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
    public PieceException(string message)
      : base(message) {
    }
    public PieceException(string message, Exception inner)
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
    public ColorException(string message)
      : base(message) {
    }
    public ColorException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected ColorException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }
}
