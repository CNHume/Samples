//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2013-08-28 CNHume]Created Class
//
namespace Command {
  using System;
  using static System.String;
  using System.Text;

  //
  // The (@) At Sign escapes the string keyword,
  // allowing its use as a "verbatim identifier"
  //
  public enum OptionType : byte { none, check, spin, combo, button, @string };

  struct Option {
    #region Virtual Fields
    public String Name;
    public OptionType Type;
    public String Default;
    public String[] Items;              // Enumerates possible values for options of type combo
    public Int32? Min;
    public Int32? Max;
    public Boolean IsHidden;
    #endregion

    #region Methods
    public override String ToString() {
      var sb = new StringBuilder("option");

      if (!IsNullOrEmpty(Name))
        sb.Append(" name ").Append(Name);

      if (Type != OptionType.none) {
        sb.Append(" type ").Append(Type);

        if (Type != OptionType.button && !IsNullOrEmpty(Default))
          sb.Append(" default ").Append(Default);

        if (Type == OptionType.spin) {
          if (Min.HasValue)
            sb.Append(" min ").Append(Min);

          if (Max.HasValue)
            sb.Append(" max ").Append(Max);
        }
        else if (Type == OptionType.combo && Items is not null) {
          const String sPrefix = " var ";
          sb.Append(sPrefix).Append(Join(sPrefix, Items));
        }
      }

      return sb.ToString();
    }
    #endregion
  }
}
