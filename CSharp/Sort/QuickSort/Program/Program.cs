//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
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
namespace Sort {
  using SortTest;

  using System;

  class Program {
    static void Main(String[] args) {
      try {
        var cmd = new Command();
        cmd.Parse(args);

        //
        //[Note]SortCase.Random ran about 4X faster over
        // 108M entries under the Tripartite conditional
        //
        var source = new SortData(cmd.SortCase);
        var entries = cmd.Length.HasValue ?
          source.BuildEntries(cmd.Length.Value) :
          default;

        var timer = new SortTimer<Int32>();
        timer.Sort(entries, cmd.Print, cmd.InsertionLimit);
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
