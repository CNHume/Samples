//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace SortTest.Extensions {
  using System;
  using System.Collections.Generic;
  using System.ComponentModel.DataAnnotations;
  using System.Linq;
  using System.Reflection;
  using System.Text;

  using static System.String;

  public static class Extension {
    #region Constants
    private const char grave = '`';
    private const char lab = '<';
    private const char rab = '>';

    private const string colonSpace = ": ";
    private const String commaSpace = ", ";
    #endregion

    #region StringBuilder Methods
    public static StringBuilder AppendDelim(this StringBuilder sb, string next, string delim = commaSpace) {
      if (IsNullOrEmpty(next)) return sb;
      if (sb.Length > 0) sb.Append(delim);
      return sb.Append(next);
    }

    public static StringBuilder AppendKeyValuePair(
      this StringBuilder sb, string key, string value) {
      sb.AppendDelim(key)
        .AppendDelim(value, colonSpace);
      return sb;
    }

    public static StringBuilder AppendTypeName(this StringBuilder sb, Type type) {
      var name = type.Name;
      if (!type.IsGenericType) return sb.Append(name);

      sb.Append(name.Remove(name.IndexOf(grave)))
        .Append(lab);

      var once = false;
      foreach (var param in type.GenericTypeArguments) {
        if (once)
          sb.Append(commaSpace);
        else
          once = true;

        sb.AppendTypeName(param);
      }

      return sb.Append(rab);
    }
    #endregion

    #region Parser Methods
    public static DateTime? TryParseDateTime(this String s) {
      return DateTime.TryParse(s, out DateTime result) ? (DateTime?)result : null;
    }

    public static Decimal? TryParseDecimal(this String s) {
      return Decimal.TryParse(s, out Decimal result) ? (Decimal?)result : null;
    }

    public static Int32? TryParseInt32(this String s) {
      return Int32.TryParse(s, out Int32 result) ? (Int32?)result : null;
    }

    public static T ParseEnum<T>(this String value, bool ignoreCase = false) {
      return (T)Enum.Parse(typeof(T), value, ignoreCase);
    }
    #endregion

    #region ParseEnumFromName Helper
    // Based on the answer to the Stackoverflow Question: "Enum value from display name"
    // See https://stackoverflow.com/questions/33225729/enum-value-from-display-name
    public static T ParseEnumFromName<T>(this String name) {
      var type = typeof(T);
      if (!type.IsEnum)
        throw new InvalidOperationException($"{type.Name} is not an Enum");

      foreach (var field in type.GetFields()) {
        var attribute = Attribute.GetCustomAttribute(field, typeof(DisplayAttribute)) as DisplayAttribute;
        if (attribute != null && name.Equals(attribute.Name, StringComparison.InvariantCultureIgnoreCase)) {
          return (T)field.GetValue(null);
        }
        else if (name.Equals(field.Name, StringComparison.InvariantCultureIgnoreCase))
          return (T)field.GetValue(null);
      }

      throw new ArgumentOutOfRangeException($"{type.Name} does not contain {name}");
    }
    #endregion

    #region Enum Methods
    // Based on Enum and [Display(Name = "")] by Pawan Pal 2016-02-17
    // See https://forums.asp.net/t/2085611.aspx?Enum+and+Display+Name+
    public static string GetDisplayName(this Enum enumeration) {
      var attr = GetDisplayAttribute(enumeration);
      return attr is null ? enumeration.ToString() : attr.Name;
    }

    public static string GetDescription(this Enum enumeration) {
      var attr = GetDisplayAttribute(enumeration);
      return attr is null ? enumeration.ToString() : attr.Description;
    }

    private static DisplayAttribute GetDisplayAttribute(Object obj) {
      var type = obj.GetType();
      if (!type.IsEnum)
        throw new InvalidOperationException($"{type.Name} is not an Enum");

      // Get the Field
      var name = obj.ToString();
      var field = type.GetField(name);
      return field?.GetCustomAttribute<DisplayAttribute>();
    }
    #endregion

    #region IEnumerable Methods
    public static Boolean IsSorted<T>(this IEnumerable<T> en, Boolean isAscending = true) where T : IComparable {
      if (en.Any()) {
        var last = en.First();
        foreach (var next in en.Skip(1)) {
          if (IsPredecessor(next, last, isAscending))
            return false;
          // Equal OK
          last = next;
        }
      }

      return true;
    }

    public static bool IsPredecessor<T>(T x, T y, Boolean isAscending = true) where T : IComparable {
      var sense = x.CompareTo(y);
      return sense < 0 && isAscending ||
             sense > 0 && !isAscending;
    }
    #endregion
  }
}
