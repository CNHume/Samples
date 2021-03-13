//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace Sort.Extension {
  using System;
  using System.Text;

  static class Extension {
    #region Constants
    private const String delim = ", ";
    private const char grave = '`';
    private const char lab = '<';
    private const char rab = '>';
    #endregion

    #region StringBuilder Methods
    public static StringBuilder AppendTypeName(this StringBuilder sb, Type type) {
      var name = type.Name;
      if (!type.IsGenericType) return sb.Append(name);

      sb.Append(name.Remove(name.IndexOf(grave)))
        .Append(lab);

      var once = false;
      foreach (var param in type.GenericTypeArguments) {
        if (once)
          sb.Append(delim);
        else
          once = true;

        sb.AppendTypeName(param);
      }

      return sb.Append(rab);
    }
    #endregion
  }
}
