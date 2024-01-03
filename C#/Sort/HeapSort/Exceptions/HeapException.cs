//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
using System.Runtime.Serialization;

namespace HeapSort.Exceptions {
  [Serializable]
  class HeapException : ApplicationException, ISerializable {
    #region Constructors
    public HeapException() {
    }
    public HeapException(String message)
      : base(message) {
    }
    public HeapException(String message, Exception inner)
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
    public HeapOverflowException(String message)
      : base(message) {
    }
    public HeapOverflowException(String message, Exception inner)
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
    public HeapUnderflowException(String message)
      : base(message) {
    }
    public HeapUnderflowException(String message, Exception inner)
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
