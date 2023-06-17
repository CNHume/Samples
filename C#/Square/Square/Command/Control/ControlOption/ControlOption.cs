//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-08-28 CNHume]Created Class
//
using System.Text;

using static System.String;

namespace Command;

using static Command.Control.OptionType;

public partial class Control {
  public struct ControlOption {
    #region Fields
    public OptionName Name;
    public OptionType Type;
    public String? Default;
    public String?[] Items;             // Enumerates possible values for options of type combo
    public Int32? Min;
    public Int32? Max;
    public Boolean IsHidden;
    #endregion                          // Fields

    #region Methods
    public override String ToString() {
      var sb = new StringBuilder("option");

      sb.Append(" name ").Append(Name);

      if (Type != none) {
        sb.Append(" type ").Append(Type);

        if (Type != button && !IsNullOrEmpty(Default))
          sb.Append(" default ").Append(Default);

        if (Type == spin) {
          if (Min.HasValue)
            sb.Append(" min ").Append(Min);

          if (Max.HasValue)
            sb.Append(" max ").Append(Max);
        }
        else if (Type == combo && Items != null) {
          const String sPrefix = " var ";
          sb.Append(sPrefix).Append(Join(sPrefix, Items));
        }
      }

      return sb.ToString();
    }
    #endregion                          // Methods
  }
}
