//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-10-03 CNHume]Created File
//
namespace Command;

#region Interfaces
public interface ICommand : IDisposable {
  Boolean Execute(String command);
}
#endregion
