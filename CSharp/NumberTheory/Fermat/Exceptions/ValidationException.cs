using System;
using System.Runtime.Serialization;

namespace Fermat.Exceptions {
  [Serializable]
  class ValidationException : ApplicationException, ISerializable {
    #region Constructors
    public ValidationException() {
    }
    public ValidationException(string message)
      : base(message) {
    }
    public ValidationException(string message, Exception inner)
      : base(message, inner) {
    }
    #endregion

    #region ISerializable Interface
    protected ValidationException(SerializationInfo info, StreamingContext context)
      : base(info, context) {
    }
    #endregion
  }
}
