//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2016-06-14 CNHume  Bentley-McIlroy 3-way Partitioning
// 2016-05-24 CNHume  Assertion and dynamic pivotSamples
// 2014-12-27 CNHume  Converted to an instantiable class
// 2014-12-13 CNHume  Added Parse() method
//

using Parsers;

using Sorters;

using SortTests;

try {
  Command command = new();
  command.Parse(args);

  if (command.Length.HasValue) {
    //
    //[Note]SortCase.Random ran about 4X faster over
    // 108M entries under the Tripartite conditional
    //
    SortData source = new(command.SortCase);
    var entries = source.BuildEntries(command.Length.Value);

    SortTimer<Int32> timer = new();
    timer.Sort(entries, command.Print, command.Trials, command.InsertionLimit);
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
