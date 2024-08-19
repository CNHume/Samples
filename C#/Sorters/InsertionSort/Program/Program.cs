//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2014-12-13 CNHume  Added Parse() method
//

using Parsers;

using Sorters;

using SortTests;

try {
  var cmd = new Command();
  cmd.Parse(args);

  if (cmd.Length.HasValue) {
    var source = new SortData(cmd.SortCase);
    var entries = source.BuildEntries(cmd.Length.Value);

    var timer = new SortTimer<Int32>();
    timer.Sort(entries, cmd.Print, cmd.Trials);
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
