//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2014-09-11 CNHume]Created Class
//
// Conditionals:
//
using System.Diagnostics;               // For FileVersionInfo
using System.Reflection;                // For Assembly, AssemblyDescriptionAttribute, AssemblyTitleAttribute

using static System.String;

namespace Command;

static class Product {
  #region Constants
  private const String sDefaultTitle = "square";
  #endregion

  #region Properties
  public static String? Title { get; }
  public static String? CompanyName { get; }
  public static String? Copyright { get; }
  public static String? Description { get; }
  public static String? ProductName { get; }
  public static String? ProductVersion { get; }
  #endregion                            // Properties

  #region Constructors
  static Product() {
    // GetExecutingAssembly() may refer to the local DLL
    var assy = Assembly.GetEntryAssembly();
    if (assy == null)
      return;

    //
    // GetCustomAttribute() is now the best answer to "Simplified way to get assembly description in C#?"
    // See https://stackoverflow.com/questions/10203575/simplified-way-to-get-assembly-description-in-c
    //
    var descriptionAttribute = assy.GetCustomAttribute<AssemblyDescriptionAttribute>();
    var fvi = FileVersionInfo.GetVersionInfo(assy.Location);

    Title = getTitle(assy);             // Name for display purposes
    CompanyName = fvi.CompanyName;
    Copyright = fvi.LegalCopyright;
    Description = descriptionAttribute?.Description;
    ProductName = fvi.ProductName;
    ProductVersion = fvi.ProductVersion;
  }
  #endregion                            // Constructors

  #region Methods
  private static String? getTitle(Assembly assy) {
    var titleAttribute = assy.GetCustomAttribute<AssemblyTitleAttribute>();
    var title = titleAttribute?.Title;
    if (IsNullOrWhiteSpace(title)) {
      var name = assy.GetName();
      return name == null ? sDefaultTitle : name.Name;
    }
    else
      return title;
  }
  #endregion                            // Methods
}
