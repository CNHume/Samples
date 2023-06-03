//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
using System.Runtime.Serialization;

namespace SortTest.Exceptions {
  [Serializable]
  public class CommandException : ApplicationException, ISerializable {
    #region Constructors
    public CommandException() {
    }
    public CommandException(String message)
      : base(message) {
    }
    public CommandException(String message, Exception inner)
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
