//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
namespace Sorters.Exceptions;

class HeapException : ApplicationException {
  #region Constructors
  public HeapException() {
  }

  public HeapException(String message)
    : base(message) {
  }

  public HeapException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class HeapOverflowException : HeapException {
  #region Constructors
  public HeapOverflowException() {
  }

  public HeapOverflowException(String message)
    : base(message) {
  }

  public HeapOverflowException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}

class HeapUnderflowException : HeapException {
  #region Constructors
  public HeapUnderflowException() {
  }

  public HeapUnderflowException(String message)
    : base(message) {
  }

  public HeapUnderflowException(String message, Exception inner)
    : base(message, inner) {
  }
  #endregion                            // Constructors
}
