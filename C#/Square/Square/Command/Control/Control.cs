//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-09-08 CNHume]Created Class
//

namespace Command {
  using Exceptions;

  using static System.String;
  using static Command.Control;

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
  // Step 1/6: Define or choose an EventArgs Type derived from the EventArgs base type
  public class PropertyChangedEventArgs : EventArgs {
    #region Constructors
    public PropertyChangedEventArgs(OptionType Type) {
      this.Type = Type;
    }
    #endregion

    #region Properties
    private OptionType Type { get; set; }
    #endregion
  }

  public class Button : Control {
    #region Events
    internal event EventHandler? Click;
    #endregion

    #region Methods
    internal virtual void OnClick(EventArgs e) {
      Click?.Invoke(this, e);
    }
    #endregion
  }

  public class Setting : Control {
    #region Fields
    //
    // Current Value:
    //
    public Boolean? IsChecked;          // For type check or button
    public Int32? Selection;            // For type spin
    public String? Text;                // For type combo or string
    #endregion                          // Fields

    #region Properties
    public Object? Value {
      get {
        switch (Option.Type) {
        case OptionType.check:
          return IsChecked;

        case OptionType.spin:
          return Selection;

        case OptionType.combo:
        case OptionType.@string:
          return Text;

        default:
          throw new ControlException($"Unexpected OptionType: {Option.Type}");
        }
      }
    }
    #endregion                          // Properties

    #region Events
    // Step 2/6: Declare Event [of the EventHandler Delegate Type]
    // Step 2a: Provide explicit implementation of the add and remove accessors
    public event EventHandler<PropertyChangedEventArgs>? PropertyChanged;
    #endregion

    #region Methods
    //
    // Step 3/6: Define a virtual method to fire the event,
    // passing this instance as the sender
    //
    internal virtual void OnPropertyChanged(PropertyChangedEventArgs e) {
      PropertyChanged?.Invoke(this, e);
    }

    public Boolean TryParse(String sValue) {
      if (IsNullOrEmpty(sValue))
        throw new ControlException($"No value specified for {Option.Type}");

      var bTypeParsed = false;
      var bValueValid = false;

      bTypeParsed = true;
      bValueValid = true;

      switch (Option.Type) {
      case OptionType.check:
        if (bTypeParsed = Boolean.TryParse(sValue, out Boolean bChecked)) {
          IsChecked = bChecked;
        }
        break;

      case OptionType.spin:
        if (bTypeParsed = Int32.TryParse(sValue, out Int32 nSelection)) {
          if (bValueValid = (!Option.Min.HasValue || Option.Min <= nSelection) &&
                            (!Option.Max.HasValue || nSelection <= Option.Max)) {
            Selection = nSelection;
          }
        }
        break;

      case OptionType.combo:
        bValueValid = false;
        if (Option.Items != null) {
          var selection = Option.Items.FirstOrDefault(
            item => sValue.Equals(item, StringComparison.InvariantCultureIgnoreCase));
          if (selection != null) {
            Text = selection;
            bValueValid = true;
          }
        }
        break;

      case OptionType.@string:
        Text = sValue;
        break;

      default:                          //[Note]Option.button case should not occur
        bValueValid =
          bTypeParsed = false;
        break;
      }

      return bTypeParsed && bValueValid;
    }

    public void SetDefault() {
      if (TryParse(Option.Default))
        OnPropertyChanged(new PropertyChangedEventArgs(Option.Type));
      else
        throw new ControlException(
          $@"Could not set a default of ""{Option.Default}"" for the {Option.Name} {Option.Type} control");
    }
    #endregion                          // Methods
  }

  public partial class Control {
    #region Fields
    public ControlOption Option;
    #endregion

    #region Methods
    public static Control? FindControl(IEnumerable<Control> controls, String? sName) {
      if (IsNullOrEmpty(sName))
        throw new ControlException("No option name specified");

      return controls.FirstOrDefault<Control>(
        control => sName.Equals(control?.Option.Name, StringComparison.InvariantCultureIgnoreCase));
    }

    public static Control? FindOption(IEnumerable<Control> controls, String? sName) {
      var uciControl = FindControl(controls, sName);
      if (uciControl == default(Control))
        throw new ControlException($"There is no option named {sName}");

      return uciControl;
    }

    public Setting AsSetting() {
      var setting = this as Setting;
      if (setting == null) {
        var settingName = typeof(Setting).Name;
        throw new ControlException(
          $"{Option.Name} control of OptionType {Option.Type} is not a {settingName}");
      }

      return setting;
    }

    public void SetValue(String? sValue) {
      switch (Option.Type) {
      case OptionType.button:
        var button = (Button)this;
        if (!IsNullOrEmpty(sValue)) {
          throw new ControlException(
            $@"Superfluous value ""{sValue}"" supplied for {Option.Name}");
        }

        // Step 4a/6 Fire Button Click Event:
        button.OnClick(EventArgs.Empty);
        break;

      default:
        var setting = (Setting)this;
        if (sValue == null || !setting.TryParse(sValue))
          throw new ControlException(
            $"Could not parse {sValue} as a value for {Option.Type}");

        // Step 4b/6 Fire Property Changed Event:
        setting.OnPropertyChanged(new PropertyChangedEventArgs(Option.Type));
        break;
      }
    }
    #endregion                          // Methods
  }
}
