//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2023-06-19 CNHume]Created Class
//
using static System.String;

namespace Command;

using Engine;

using Exceptions;

public partial class Control {
  //
  // Event Handler Implementation cf. New Control Checklist
  // ----------------------------
  // Step 1 Define EventArgs Type
  // Step 2 Declare Event
  // Step 3 Define "On" method to fire event
  // Step 4 Fire Event
  // Step 5 Define Event Handler to receive input from sender
  // Step 6 Subscribe Handler to event
  //
  #region Events
  // Step 2/6: Declare Event [of the EventHandler Delegate Type]
  // Step 2a: Provide explicit implementation of the add and remove accessors
  public event EventHandler<PropertyChangedEventArgs>? PropertyChanged;
  #endregion                            // Events

  #region Methods
  //
  // Step 3/6: Define a virtual method to fire the event,
  // passing this instance as the sender
  //
  internal virtual void OnPropertyChanged(PropertyChangedEventArgs e) {
    PropertyChanged?.Invoke(this, e);
  }

  public static Control? FindControl(
    IEnumerable<Control> controls, ControlName optionName) {
    return controls.FirstOrDefault(control => control?.Name == optionName);
  }

  public static Control FindOption(
    IEnumerable<Control> controls, String? sName, Boolean ignoreCase = true) {
    if (IsNullOrEmpty(sName))
      throw new ControlException("No option name specified");

    var optionName = sName.TryParseEnumFromName<ControlName>(ignoreCase);
    var uciControl = FindControl(controls, optionName);
    if (uciControl == default)
      throw new ControlException($"There is no option named {sName}");

    return uciControl;
  }

  public Setting AsSetting() {
    var setting = this as Setting;
    if (setting == null) {
      var settingName = typeof(Setting).Name;
      throw new ControlException(
        $"{Name} control of OptionType {GetType()} is not a {settingName}");
    }

    return setting;
  }
  #endregion                            // Methods
}
