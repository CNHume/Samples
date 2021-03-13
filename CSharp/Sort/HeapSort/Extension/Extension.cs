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

    #region Methods
    public static StringBuilder AppendTypeName(this StringBuilder sb, Type type) {
      if (type.IsGenericType) {
        var name = type.Name;
        sb.Append(name.Remove(name.IndexOf(grave)));
        sb.Append(lab);
        var delimit = false;
        foreach (var param in type.GenericTypeArguments) {
          if (delimit)
            sb.Append(delim);
          else
            delimit = true;

          sb.AppendTypeName(param);
        }

        return sb.Append(rab);
      }

      return sb.Append(type.Name);
    }
    #endregion
  }
}
