//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
// 2014-12-14 CNHume  Added Parse() method
// 2012-04-04 CNHume  Created HeapSort
//
namespace Sort {
  using SortTest;

  using System;

  class Program {
    static void Main(String[] args) {
      try {
        var cmd = new Command();
        cmd.Parse(args);

        var source = new SortData(cmd.SortCase);

        if (cmd.Length.HasValue) {
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
    }
  }
}
