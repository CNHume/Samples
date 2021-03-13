//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace Sort.Exceptions {
  using System;
  using System.Runtime.Serialization;

  [Serializable]
  class CommandException : ApplicationException, ISerializable {
    #region Constructors
    public CommandException() {
    }
    public CommandException(string message)
      : base(message) {
    }
    public CommandException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected CommandException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }
}
