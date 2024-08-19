//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//

using Fermat.Maths;
using Fermat.Parsers;
using Fermat.Settings;

using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;

var serviceCollection = new ServiceCollection();
ConfigureServices(args, serviceCollection);
var serviceProvider = serviceCollection.BuildServiceProvider();
var logger = serviceProvider.GetService<ILogger<Program>>();

try {
  var command = serviceProvider.GetService<Command>();
  var settings = serviceProvider.GetService<TestSetting>();

  if (command != null) {
    var rsa = new RSA(command.Power.Value, command.Modulus.Value);
    var input = command.Input.Value;
    var encoded = rsa.TestEncodeAndDecode(input, settings.Totient);

    Console.WriteLine($"{encoded} = Encode(input = {input}, encodePower = {rsa.EncodePower}, modulus = {rsa.Modulus})");
  }
}
catch (ApplicationException ex) {
  logger.LogError(ex, "Fermat Error");
#if DEBUG
  Console.WriteLine(ex.Message);
#endif
}
#if DEBUG
Console.Write("Press Enter");
Console.ReadLine();
#endif

#region Methods
static void ConfigureServices(string[] args, IServiceCollection services) {
  var command = new Command(args);
  var testSettings = new TestSetting();
  var configuration = new ConfigurationBuilder();

  configuration
    .AddJsonFile("appsettings.json", optional: false)
    .Build()
    .GetSection("TestSettings")
    .Bind(testSettings);

  // Configure Logging:
  services
    .AddLogging(configure => configure.AddConsole())
    .AddTransient<Program>()
    .AddSingleton(command)
    .AddSingleton(testSettings);
}
#endregion                              // Methods
