﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2014-09-11 CNHume]Created Class
//
// Conditionals:
//
using System.Diagnostics;               // For FileVersionInfo
using System.Reflection;                // For Assembly, AssemblyDescriptionAttribute, AssemblyTitleAttribute

using static System.String;

namespace Commands;

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
  public static Version? ProductVersion { get; }
  #endregion                            // Properties

  #region Constructors
  static Product() {
    // GetExecutingAssembly() may refer to the local DLL
    var assy = Assembly.GetEntryAssembly();
    if (assy == null)
      return;

    var name = assy.GetName();
    ProductVersion = name.Version;
    Title = getTitle(assy);             // Name for display purposes

    //
    // GetCustomAttribute() is now the best answer to "Simplified way to get assembly description in C#?"
    // See https://stackoverflow.com/questions/10203575/simplified-way-to-get-assembly-description-in-c
    //
    var descriptionAttribute = assy.GetCustomAttribute<AssemblyDescriptionAttribute>();
    Description = descriptionAttribute?.Description;

    var fvi = FileVersionInfo.GetVersionInfo(assy.Location);
    CompanyName = fvi.CompanyName;
    Copyright = fvi.LegalCopyright;
    ProductName = fvi.ProductName;
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
