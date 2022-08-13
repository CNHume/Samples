//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using System;
using System.Runtime.Serialization;

namespace Fermat.Exceptions {
  [Serializable]
  class ParseException : ApplicationException, ISerializable {
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
}
