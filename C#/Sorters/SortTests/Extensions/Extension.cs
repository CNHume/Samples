//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Globalization;
using System.Reflection;                // For FieldInfo
using System.Text;

using Microsoft.Extensions.Logging;

namespace SortTests.Extensions;

using static System.String;
using static System.StringComparison;

public static class Extension {
  #region Constants
  private const Char grave = '`';
  private const Char lab = '<';
  private const Char rab = '>';

  private const String colonSpace = ": ";
  private const String commaSpace = ", ";
  #endregion

  #region Methods
  #region StringBuilder Methods
  public static StringBuilder AppendDelim(
    this StringBuilder sb, String? next, String delim = commaSpace) {
    if (IsNullOrEmpty(next)) return sb;
    if (sb.Length > 0) sb.Append(delim);
    return sb.Append(next);
  }

  public static StringBuilder AppendKeyValuePair<TKey, TValue>(
    this StringBuilder sb, TKey key, TValue value)
    where TKey : notnull
    where TValue : notnull {
    sb.AppendDelim(key.ToString())
      .AppendDelim(value.ToString(), colonSpace);
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
  #endregion                            // StringBuilder Methods

  #region Parser Methods
  public static DateTime? TryParseDateTime(this String s) {
    return DateTime.TryParse(s, out DateTime result) ?
      (DateTime?)result : null;
  }

  public static DateTime? TryParseDateTimeExact(this String s, String format) {
    return DateTime.TryParseExact(
      s, format, CultureInfo.InvariantCulture, DateTimeStyles.None, out DateTime result) ?
      result : null;
  }

  public static Decimal? TryParseDecimal(this String s) {
    return Decimal.TryParse(s, out Decimal result) ?
      (Decimal?)result : null;
  }

  public static Int32? TryParseInt32(this String s) {
    return Int32.TryParse(s, out Int32 result) ?
      (Int32?)result : null;
  }

  public static UInt32? TryParseUInt32(this String s) {
    return UInt32.TryParse(s, out UInt32 result) ?
      (UInt32?)result : null;
  }

  public static Boolean? TryParseBoolean(this String s) {
    return Boolean.TryParse(s, out Boolean result) ?
      (Boolean?)result : null;
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
    this String name, Boolean ignoreCase = default)
    where TEnum : Enum {
    TEnum? value = default;
    if (!IsNullOrEmpty(name)) {
      var comparisonType = ignoreCase ? CurrentCultureIgnoreCase : CurrentCulture;
      var type = typeof(TEnum);
      foreach (var field in type.GetFields()) {
        var descriptionAttribute =
          Attribute.GetCustomAttribute(field, typeof(DescriptionAttribute)) as DescriptionAttribute;
        var displayAttribute =
          Attribute.GetCustomAttribute(field, typeof(DisplayAttribute)) as DisplayAttribute;

        var found =
          name.Equals(field.Name, comparisonType) ||
          descriptionAttribute != null &&
          name.Equals(descriptionAttribute.Description, comparisonType) ||
          displayAttribute != null &&
          name.Equals(displayAttribute.Name, comparisonType);

        if (found) {
          value = (TEnum?)field.GetValue(default);
          return value;
        }
      }
    }
    return value;
  }

  public static TEnum ParseEnumFromName<TEnum>(
    this String name, Boolean ignoreCase = default)
    where TEnum : Enum {
    var result = name.TryParseEnumFromName<TEnum>(ignoreCase);
    if (result == null) {
      var type = typeof(TEnum);
      throw new ArgumentOutOfRangeException($"{type.Name} does not contain {name}");
    }
    return result;
  }

  // Based on Enum and [Display(Name = "")] by Pawan Pal 2016-02-17
  // See https://forums.asp.net/t/2085611.aspx?Enum+and+Display+Name+
  public static String GetDisplayName(this Enum enumeration) {
    var attr = GetDisplayAttribute(enumeration);
    return attr?.Name != null ? attr.Name : enumeration.ToString();
  }

  public static String GetDescription(this Enum enumeration) {
    var attr = GetDisplayAttribute(enumeration);
    return attr?.Description != null ? attr.Description : enumeration.ToString();
  }

  private static DisplayAttribute? GetDisplayAttribute(object obj) {
    var type = obj.GetType();
    if (!type.IsEnum) {
      throw new InvalidOperationException($"{type.Name} is not an Enum");
    }

    // Get the enum field
    var name = obj.ToString();
    if (name == null)
      return default;

    var field = type.GetField(name);
    return field?.GetCustomAttribute<DisplayAttribute>();
  }
  #endregion                            // ParseEnumFromName Helper

  public static TStruct? TryParseEnum<TStruct>(
    this String s, Boolean ignoreCase = default)
    where TStruct : struct {
    return Enum.TryParse(s, ignoreCase, out TStruct result) ?
      (TStruct?)result : default;
  }

  public static TEnum ParseEnum<TEnum>(
    this String value, Boolean ignoreCase = default)
    where TEnum : Enum {
    return (TEnum)Enum.Parse(typeof(TEnum), value, ignoreCase);
  }

  public static TEnum? ValidEnumFromName<TEnum, TLogger>(
    this String name, TLogger logger, Boolean ignoreCase = default)
    where TEnum : Enum
    where TLogger : ILogger {
    var result = name.TryParseEnumFromName<TEnum>(ignoreCase);
    if (result == null && !IsNullOrEmpty(name)) {
      var type = typeof(TEnum);
      logger.LogError($"{type.Name} does not contain {name}");
    }
    return result;
  }

  public static TStruct? ValidEnum<TStruct, TLogger>(
    this String name, TLogger logger, Boolean ignoreCase = default)
    where TStruct : struct
    where TLogger : ILogger {
    var result = name.TryParseEnum<TStruct>(ignoreCase);
    if (result == null && !IsNullOrEmpty(name) && logger != null) {
      var type = typeof(TStruct);
      logger.LogError($"{type.Name} does not contain {name}");
    }
    return result;
  }
  #endregion                            // Enum Methods

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

  public static Boolean IsPredecessor<T>(
    this T x, T y, Boolean isAscending = true) where T : IComparable {
    var sense = x.CompareTo(y);
    return sense < 0 && isAscending ||
           sense > 0 && !isAscending;
  }
  #endregion
  #endregion                            // Parser Methods
  #endregion                            // Methods
}
