//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-09-08 CNHume]Created Class
//
using System.ComponentModel;
using System.Text;

using static System.String;

namespace Command;

using Engine;

using Exceptions;

[DisplayName("button")]
public class Button : Control {
  #region Events
  internal event EventHandler? Click;
  #endregion

  #region Methods
  internal virtual void OnClick(EventArgs e) {
    Click?.Invoke(this, e);
  }

  public void SetValue(String? sValue) {
    if (!IsNullOrEmpty(sValue)) {
      throw new ControlException(
        $@"Superfluous value ""{sValue}"" supplied for {Name}");
    }

    // Step 4a/6 Fire Button Click Event:
    OnClick(EventArgs.Empty);
  }
  #endregion                            // Methods
}

[DisplayName("check")]
public class CheckSetting : Setting {
  #region Properties
  public Boolean? Value { get; set; }
  #endregion                            // Properties

  #region Methods
  public override Object? GetValue() { return Value; }

  protected override Boolean TryParse(String? sValue) {
    if (IsNullOrEmpty(sValue))
      throw new ControlException($"No value specified for {GetType()}");

    var bTypeParsed = Boolean.TryParse(sValue, out Boolean bChecked);

    if (bTypeParsed)
      Value = bChecked;

    return bTypeParsed;
  }
  #endregion                            // Methods
}

[DisplayName("spin")]
public class SpinSetting : Setting {
  #region Properties
  public Int32? Min;
  public Int32? Max;
  private Int32? Value { get; set; }
  #endregion                            // Properties

  #region Methods
  public override Object? GetValue() { return Value; }

  #region ToString() Override
  public override String ToString() {
    var sb = new StringBuilder(base.ToString());

    if (Min.HasValue)
      sb.Append(" min ").Append(Min);

    if (Max.HasValue)
      sb.Append(" max ").Append(Max);

    return sb.ToString();
  }
  #endregion                          // ToString() Override

  protected override Boolean TryParse(String? sValue) {
    if (IsNullOrEmpty(sValue))
      throw new ControlException($"No value specified for {GetType()}");

    var bTypeParsed = false;
    var bValueValid = false;

    bTypeParsed = true;
    bValueValid = true;

    if (bTypeParsed = Int32.TryParse(sValue, out Int32 nSelection)) {
      if (bValueValid = (!Min.HasValue || Min <= nSelection) &&
                        (!Max.HasValue || nSelection <= Max)) {
        Value = nSelection;
      }
    }

    return bTypeParsed && bValueValid;
  }
  #endregion                            // Methods
}

[DisplayName("combo")]
public class ComboSetting : Setting {
  #region Properties
  public String[]? Items;               // Enumerates possible values for options of type combo

  private String? Value { get; set; }
  #endregion                            // Properties

  #region Methods
  public override String? GetValue() { return Value; }

  #region ToString() Override
  public override String ToString() {
    const String sPrefix = " var ";
    var sb = new StringBuilder(base.ToString());

    if (Items != null)
      sb.Append(sPrefix).Append(Join(sPrefix, Items));

    return sb.ToString();
  }
  #endregion                          // ToString() Override

  protected override Boolean TryParse(String? sValue) {
    if (IsNullOrEmpty(sValue))
      throw new ControlException($"No value specified for {GetType()}");

    var bValueValid = false;

    if (Items != null) {
      var selection = Items.FirstOrDefault(
        item => sValue.Equals(item, StringComparison.InvariantCultureIgnoreCase));
      if (selection != null) {
        Value = selection;
        bValueValid = true;
      }
    }

    return bValueValid;
  }
  #endregion                            // Methods
}

[DisplayName("string")]
public class StringSetting : Setting {
  #region Properties
  private String? Value { get; set; }
  #endregion                            // Properties

  #region Methods
  public override String? GetValue() { return Value; }

  protected override Boolean TryParse(String? sValue) {
    if (IsNullOrEmpty(sValue))
      throw new ControlException($"No value specified for {GetType()}");

    Value = sValue;
    return true;
  }
  #endregion                            // Methods
}

public abstract class Setting : Control {
  #region Methods
  protected abstract Boolean TryParse(String? sValue);
  public abstract Object? GetValue();

  #region ToString() Override
  public override String ToString() {
    var s = base.ToString();

    if (!IsNullOrEmpty(Default)) {
      var sb = new StringBuilder(" default ").Append(Default);
      s += sb;
    }

    return s;
  }
  #endregion                          // ToString() Override

  public void SetValue(String? sValue) {
    if (sValue == null || !TryParse(sValue))
      throw new ControlException(
        $"Could not parse {sValue} as a value for {GetType()}");

    // Step 4b/6 Fire Property Changed Event:
    OnPropertyChanged(new PropertyChangedEventArgs());
  }

  public void SetDefault() {
    if (TryParse(Default))
      OnPropertyChanged(new PropertyChangedEventArgs());
    else
      throw new ControlException(
        $@"Could not set a default of ""{Default}"" for the {Name} {GetType()} control");
  }
  #endregion                            // Methods
}

public partial class Control {
  #region Fields
  public ControlName Name;
  public String? Default;
  public Boolean IsHidden;
  #endregion                            // Fields

  #region Methods
  public override String ToString() {
    var type = GetType();
    var displayName = type
      .GetCustomAttributes(typeof(DisplayNameAttribute), true)
      .FirstOrDefault() as DisplayNameAttribute;
    var typeName = displayName?.DisplayName ?? type.Name;

    var sb = new StringBuilder("option")
      .Append(" name ")
      .Append(Name)
      .Append(" type ")
      .Append(typeName);

    return sb.ToString();
  }
  #endregion                            // Methods
}
