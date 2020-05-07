//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Microsoft.Extensions.Logging;

using System;

namespace Fermat {
  class Program {
    #region Properties
    public static ILogger<Program> Logger { get; set; }
    #endregion

    #region Methods
    static void Main(string[] args) {
      try {
        var cmd = new Command(args);
        Math.TestModPower(cmd);
      }
      catch (ApplicationException ex) {
        //Logger.LogError(ex, "Fermat Error");
#if DEBUG
        Console.WriteLine(ex.Message);
#endif
      }
#if DEBUG
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }
    #endregion
  }
}
