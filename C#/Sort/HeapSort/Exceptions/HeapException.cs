//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
namespace HeapSort.Exceptions {
  using System;
  using System.Runtime.Serialization;

  [Serializable]
  class HeapException : ApplicationException, ISerializable {
    #region Constructors
    public HeapException() {
    }
    public HeapException(string message)
      : base(message) {
    }
    public HeapException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected HeapException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }

  [Serializable]
  class HeapOverflowException : HeapException, ISerializable {
    #region Constructors
    public HeapOverflowException() {
    }
    public HeapOverflowException(string message)
      : base(message) {
    }
    public HeapOverflowException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected HeapOverflowException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }

  [Serializable]
  class HeapUnderflowException : HeapException, ISerializable {
    #region Constructors
    public HeapUnderflowException() {
    }
    public HeapUnderflowException(string message)
      : base(message) {
    }
    public HeapUnderflowException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected HeapUnderflowException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }
}
