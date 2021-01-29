//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2014-09-11 CNHume]Created Class
//
// Conditionals:
//

namespace Command {
  using System;
  using System.Diagnostics;             // For FileVersionInfo
  using System.Linq;
  using System.Reflection;              // For Assembly, AssemblyTitleAttribute
  using static System.String;

  using Engine;

  static class Product {
    #region Constants
    private const String sDefaultTitle = "square";
    #endregion

    #region Constructors
    static Product() {
      var assy = Assembly.GetEntryAssembly();   // GetExecutingAssembly() may refer to the local DLL
      //
      // GetCustomAttribute() is now the best answer to "Simplified way to get assembly description in C#?"
      // See https://stackoverflow.com/questions/10203575/simplified-way-to-get-assembly-description-in-c
      //
      var descriptionAttribute = assy.GetCustomAttribute<AssemblyDescriptionAttribute>();
      var fvi = FileVersionInfo.GetVersionInfo(assy.Location);

      Title = getTitle(assy);           // Name for display purposes
      CompanyName = fvi.CompanyName;
      Copyright = fvi.LegalCopyright;
      Description = descriptionAttribute?.Description;
      ProductName = fvi.ProductName;
      ProductVersion = fvi.ProductVersion;
    }
    #endregion

    #region Methods
    private static String getTitle(Assembly assy) {
      var titleAttribute = assy.GetCustomAttribute<AssemblyTitleAttribute>();
      var title = titleAttribute.Title;
      if (IsNullOrWhiteSpace(title)) {
        var name = assy.GetName();
        return name is null ? sDefaultTitle : name.Name;
      }
      else
        return title;
    }
    #endregion

    #region Properties
    public static String Title { get; private set; }
    public static String CompanyName { get; private set; }
    public static String Copyright { get; private set; }
    public static String Description { get; private set; }
    public static String ProductName { get; private set; }
    public static String ProductVersion { get; private set; }
    #endregion
  }
}
