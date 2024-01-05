//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2013-09-08 CNHume]Created Class
//
using System.ComponentModel.DataAnnotations;
using System.Text;

using static System.String;
using static System.StringComparison;

namespace Command;

using static Engine.Extension;

using Events;

using Exceptions;

[Display(Name = "button")]
public class Button : Control {
  #region Events
  public event EventHandler? Click;

  protected virtual void OnClick(EventArgs e) {
    Click?.Invoke(this, e);
  }
  #endregion                            // Events

  #region Methods
  public void SetValue(String? sValue) {
    if (!IsNullOrEmpty(sValue))
      throw new ControlException(
        @$"Superfluous value ""{sValue}"" supplied for {Name}");

    // Step 4a/6 Fire Button Click Event:
    OnClick(EventArgs.Empty);
  }
  #endregion                            // Methods
}

[Display(Name = "check")]
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

[Display(Name = "spin")]
public class SpinSetting : Setting {
  #region Fields
  public Int32? Min;
  public Int32? Max;
  #endregion                            // Fields

  #region Properties
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

    var bTypeParsed = Int32.TryParse(sValue, out Int32 nSelection);
    var bValueValid = false;

    if (bTypeParsed) {
      if (bValueValid = (!Min.HasValue || Min <= nSelection) &&
                        (!Max.HasValue || nSelection <= Max))
        Value = nSelection;
    }

    return bTypeParsed && bValueValid;
  }
  #endregion                            // Methods
}

[Display(Name = "combo")]
public class ComboSetting : Setting {
  #region Fields
  public String?[]? Items;              // Enumerates possible values for options of type combo
  #endregion                            // Fields

  #region Properties
  private String? Value { get; set; }
  #endregion                            // Properties

  #region Methods
  public override String? GetValue() { return Value; }

  #region ToString() Override
  public override String ToString() {
    const String sComboPrefix = " var ";
    var sb = new StringBuilder(base.ToString());

    if (Items != null)
      sb.Append(sComboPrefix)
        .Append(Join(sComboPrefix, Items));

    return sb.ToString();
  }
  #endregion                            // ToString() Override

  protected override Boolean TryParse(String? sValue) {
    if (IsNullOrEmpty(sValue))
      throw new ControlException($"No value specified for {GetType()}");

    var bValueValid = false;
    if (Items != null) {
      var selection = Items.FirstOrDefault(item =>
        sValue.Equals(item, InvariantCultureIgnoreCase));

      if (selection != null) {
        Value = selection;
        bValueValid = true;
      }
    }
    return bValueValid;
  }
  #endregion                            // Methods
}

[Display(Name = "string")]
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
  #region Fields
  public String? Default;
  #endregion

  //
  // Event Handler Implementation [cf. New Control Checklist in GameState Controls]
  // ----------------------------
  // Step 1 Define EventArgs Type
  // Step 2 Declare Event
  // Step 3 Define "On" method to fire event
  // Step 4 Fire Event by calling "On" method
  // Step 5 Define Event Handler to receive input from sender
  // Step 6 Subscribe to Event Handler in Wireup method
  //
  #region Events
  // Step 2/6: Declare Event [of the EventHandler Delegate Type]
  // Step 2a: Provides explicit implementation of the add and remove accessors
  public event EventHandler<PropertyChangedEventArgs>? PropertyChanged;

  //
  // Step 3/6: Define a virtual method to fire the event,
  // passing this instance as the sender
  //
  internal virtual void OnPropertyChanged(PropertyChangedEventArgs e) {
    PropertyChanged?.Invoke(this, e);
  }
  #endregion                            // Events

  #region Methods
  protected abstract Boolean TryParse(String? sValue);
  public abstract Object? GetValue();

  public void SetValue(String? sValue) {
    if (sValue == null || !TryParse(sValue)) {
      var typeName = GetType().GetTypeName();
      throw new ControlException(
        @$"Could not set value of ""{sValue}"" for the {Name} {typeName} control");
    }

    // Step 4b/6 Fire Property Changed Event:
    OnPropertyChanged(new PropertyChangedEventArgs());
  }

  public void SetDefault() {
    if (!TryParse(Default)) {
      var typeName = GetType().GetTypeName();
      throw new ControlException(
        @$"Could not set default of ""{Default}"" for the {Name} {typeName} control");
    }

    // Step 4b/6 Fire Property Changed Event:
    OnPropertyChanged(new PropertyChangedEventArgs());
  }

  #region ToString() Override
  public override String ToString() {
    var sb = new StringBuilder(base.ToString());

    if (!IsNullOrEmpty(Default))
      sb.Append(" default ")
        .Append(Default);

    return sb.ToString();
  }
  #endregion                            // ToString() Override
  #endregion                            // Methods
}

public partial class Control {
  #region Fields
  public ControlName Name;
  public Boolean IsHidden;
  #endregion                            // Fields

  #region Methods
  public Setting AsSetting() {
    var setting = this as Setting;
    if (setting == null) {
      var settingName = typeof(Setting).Name;
      throw new ControlException(
        $"{Name} control of type {GetType()} is not a {settingName}");
    }

    return setting;
  }

  #region Find Methods
  public static Control? FindControl(
    IEnumerable<Control> controls, ControlName controlName) {
    return controls.FirstOrDefault(control => control?.Name == controlName);
  }

  public static Control FindControl(
    IEnumerable<Control> controls, String? sName, Boolean ignoreCase = true) {
    if (IsNullOrEmpty(sName))
      throw new ControlException("No control name specified");

    var controlName = sName.TryParseEnumFromName<ControlName>(ignoreCase);
    var uciControl = FindControl(controls, controlName);
    if (uciControl == default)
      throw new ControlException($"There is no control named {sName}");

    return uciControl;
  }
  #endregion                            // Find Methods

  #region ToString() Override
  public override String ToString() {
    var typeName = GetType().GetTypeName();

    var sb = new StringBuilder("option")
      .Append(" name ")
      .Append(Name)
      .Append(" type ")
      .Append(typeName);

    return sb.ToString();
  }
  #endregion                            // ToString() Override
  #endregion                            // Methods
}
