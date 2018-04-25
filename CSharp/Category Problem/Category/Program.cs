//
// Copyright (C) 2018, Christopher N. Hume
//
// 2018-03-28 CNHume    Created Console Application
//
namespace CategoryProblem {
  using System;
  using System.Diagnostics;

  class Program {
    #region Constants
    const int ERROR = 1;
    const int SUCCESS = 0;
    #endregion

    #region Methods
    static int Main(string[] args) {
      var result = ERROR;

      try {
        var cmd = new Command();
        cmd.Parse(args);

        var processor = new Processor();
        processor.Query(cmd);

        result = SUCCESS;
      }
      catch (ArgumentException ex) {
        Console.WriteLine($"\n{ex.Message}");
      }
      catch (Exception ex) {
        Console.WriteLine($"\n{ex}");
      }
#if DEBUG
      Console.Write("Press any key to continue...");
      Console.ReadKey(true);
#endif
      return result;
    }
    #endregion
  }
}
