﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2014-12-14 CNHume  Added Parse() method
// 2012-04-04 CNHume  Created HeapSort
//

using Parsers;

using Sorters;

using SortTests;

try {
  Command command = new();
  command.Parse(args);

  SortData source = new(command.SortCase);

  if (command.Length.HasValue) {
    var entries = source.BuildEntries(command.Length.Value);
    SortTimer<Int32> timer = new();
    timer.Sort(entries, command.Print, command.Trials);
  }
}
catch (ApplicationException ex) {
  Console.WriteLine(ex.Message);
}
catch (Exception ex) {
  Console.WriteLine(ex);
}
#if DEBUG
Console.Write("Press Enter");
Console.ReadLine();
#endif
