//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
namespace SortTest.Extensions {
  using Microsoft.Extensions.Logging;

  using System;
  using System.Collections.Generic;
  using System.ComponentModel.DataAnnotations;
  using System.Linq;
  using System.Reflection;
  using System.Text;

  using static System.String;
  using static System.StringComparison;

  public static class Extension {
    #region Constants
    private const char grave = '`';
    private const char lab = '<';
    private const char rab = '>';

    private const string colonSpace = ": ";
    private const String commaSpace = ", ";
    #endregion

    #region Methods
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
    #endregion                          // StringBuilder Methods

    #region Parser Methods
    public static DateTime? TryParseDateTime(this String s) {
      return DateTime.TryParse(s, out DateTime result) ?
        (DateTime?)result : default;
    }

    public static Decimal? TryParseDecimal(this String s) {
      return Decimal.TryParse(s, out Decimal result) ?
        (Decimal?)result : default;
    }

    public static Int32? TryParseInt32(this String s) {
      return Int32.TryParse(s, out Int32 result) ?
        (Int32?)result : default;
    }

    public static UInt32? TryParseUInt32(this String s) {
      return UInt32.TryParse(s, out UInt32 result) ?
        (UInt32?)result : default;
    }

    #region Enum Methods
    #region ParseEnumFromName Helper
    //
    // Based on the answer to the Stackoverflow Question: "Enum value from display name"
    // See https://stackoverflow.com/questions/33225729/enum-value-from-display-name
    //
    //[Note]Certain Enums, e.g., IdentifierType may need to become Codeable Concepts.
    //
    public static TEnum? TryParseEnumFromName<TEnum>(
      this string name, bool ignoreCase = default)
      where TEnum : Enum {
      if (!IsNullOrEmpty(name)) {
        var stringComparison = ignoreCase ? InvariantCultureIgnoreCase : CurrentCulture;
        var type = typeof(TEnum);
        foreach (var field in type.GetFields()) {
          var attribute =
            Attribute.GetCustomAttribute(field, typeof(DisplayAttribute)) as DisplayAttribute;
          var found =
            name.Equals(field.Name, stringComparison) ||
            attribute is not null && name.Equals(attribute.Name, stringComparison);

          if (found)
            return (TEnum?)field.GetValue(null);
        }
      }
      return default;
    }

    public static TEnum ParseEnumFromName<TEnum>(
      this string name, bool ignoreCase = default)
      where TEnum : Enum {
      var result = name.TryParseEnumFromName<TEnum>(ignoreCase);
      if (result is null) {
        var type = typeof(TEnum);
        throw new ArgumentOutOfRangeException($"{type.Name} does not contain {name}");
      }
      return result;
    }

    // Based on Enum and [Display(Name = "")] by Pawan Pal 2016-02-17
    // See https://forums.asp.net/t/2085611.aspx?Enum+and+Display+Name+
    public static string GetDisplayName(this Enum enumeration) {
      var attr = GetDisplayAttribute(enumeration);
      return attr?.Name is not null ? attr.Name : enumeration.ToString();
    }

    public static string GetDescription(this Enum enumeration) {
      var attr = GetDisplayAttribute(enumeration);
      return attr?.Description is not null ? attr.Description : enumeration.ToString();
    }

    private static DisplayAttribute? GetDisplayAttribute(object obj) {
      var type = obj.GetType();
      if (!type.IsEnum) {
        throw new InvalidOperationException($"{type.Name} is not an Enum");
      }

      // Get the enum field
      var name = obj.ToString();
      if (name is null)
        return default;

      var field = type.GetField(name);
      return field?.GetCustomAttribute<DisplayAttribute>();
    }
    #endregion                          // ParseEnumFromName Helper

    public static TStruct? TryParseEnum<TStruct>(
      this string s, bool ignoreCase = default)
      where TStruct : struct {
      return Enum.TryParse(s, ignoreCase, out TStruct result) ?
        (TStruct?)result : default;
    }

    public static TEnum ParseEnum<TEnum>(
      this string value, bool ignoreCase = default)
      where TEnum : Enum {
      return (TEnum)Enum.Parse(typeof(TEnum), value, ignoreCase);
    }

    public static TEnum? ValidEnumFromName<TEnum, TLogger>(
      this string name, TLogger logger, bool ignoreCase = default)
      where TEnum : Enum
      where TLogger : ILogger {
      var result = name.TryParseEnumFromName<TEnum>(ignoreCase);
      if (result is null && !IsNullOrEmpty(name)) {
        var type = typeof(TEnum);
        logger.LogError($"{type.Name} does not contain {name}");
      }
      return result;
    }

    public static TStruct? ValidEnum<TStruct, TLogger>(
      this string name, TLogger logger, bool ignoreCase = default)
      where TStruct : struct
      where TLogger : ILogger {
      var result = name.TryParseEnum<TStruct>(ignoreCase);
      if (result is null && !IsNullOrEmpty(name) && logger is not null) {
        var type = typeof(TStruct);
        logger.LogError($"{type.Name} does not contain {name}");
      }
      return result;
    }
    #endregion                          // Enum Methods

    #region IEnumerable Methods
    public static Boolean IsSorted<T>(
      this IEnumerable<T> en, Boolean isAscending = true) where T : IComparable {
      if (en.Any()) {
        var last = en.First();
        foreach (var next in en.Skip(1)) {
          if (next.IsPredecessor(last, isAscending))
            return false;
          // Equal OK
          last = next;
        }
      }

      return true;
    }

    public static bool IsPredecessor<T>(
      this T x, T y, Boolean isAscending = true) where T : IComparable {
      var sense = x.CompareTo(y);
      return sense < 0 && isAscending ||
             sense > 0 && !isAscending;
    }
    #endregion
    #endregion                          // Parser Methods
    #endregion                          // Methods
  }
}
