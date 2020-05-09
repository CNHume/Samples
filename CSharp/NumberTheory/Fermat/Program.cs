//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Fermat.Math;
using Fermat.Parsing;
using Fermat.Settings;

using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;

using System;

namespace Fermat {
  class Program {
    #region Properties
    public static Command Command { get; set; }
    public static TestSettings Settings { get; set; }
    public static ILogger<Program> Logger { get; set; }
    #endregion

    #region Methods
    static void Main(string[] args) {
      try {
        var serviceCollection = new ServiceCollection();
        ConfigureServices(args, serviceCollection);

        var serviceProvider = serviceCollection.BuildServiceProvider();
        Logger = serviceProvider.GetService<ILogger<Program>>();
        Settings = serviceProvider.GetService<TestSettings>();
        Command = serviceProvider.GetService<Command>();

        var test = new ModularTest(Command, Settings);
        test.TestModPower();
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

    private static void ConfigureServices(string[] args, IServiceCollection services) {
      var command = new Command(args);
      var testSettings = new TestSettings();
      var settings = new ConfigurationBuilder().AddJsonFile("appsettings.json", optional: false).Build();
      settings.GetSection("TestSettings").Bind(testSettings);

      // Configure Logging:
      services.AddLogging(configure => configure.AddConsole())
              .AddTransient<Program>()
              .AddSingleton(command)
              .AddSingleton(testSettings);
    }
    #endregion
  }
}
