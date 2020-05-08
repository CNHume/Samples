//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Microsoft.Extensions.DependencyInjection;
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
        var serviceCollection = new ServiceCollection();
        ConfigureServices(serviceCollection);

        var serviceProvider = serviceCollection.BuildServiceProvider();
        Logger = serviceProvider.GetService<ILogger<Program>>();

        var cmd = new Command(args);
        Math.TestModPower(cmd);
      }
      catch (ApplicationException ex) {
        Logger.LogError(ex, "Fermat Error");
#if DEBUG
        Console.WriteLine(ex.Message);
#endif
      }
#if DEBUG
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }

    private static void ConfigureServices(IServiceCollection services) {
      // Configure Logging:
      services.AddLogging(configure => configure.AddConsole())
              .AddTransient<Program>();
    }
    #endregion
  }
}
