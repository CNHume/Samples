//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2014-10-03 CNHume]Created File
//
namespace Command {
  using System;

  #region Interfaces
  public interface ICommand : IDisposable {
    Boolean Execute(String command);
  }
  #endregion
}
