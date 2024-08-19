//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2014-10-03 CNHume]Created File
//
namespace Commands;

#region Interfaces
public interface ICommand : IDisposable {
  Boolean Execute(String command);
}
#endregion
