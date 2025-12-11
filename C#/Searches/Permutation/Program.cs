//
// Copyright (C) 2018-2025, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
//[2018-12-22 CNHume]Created Permuter .NET Console Application
//
// To publish:
// dotnet publish Permuter -c Release -r win10-x64
//
// Usage: Permute [-p] [-s] letters
//

using Permuters;

using Parsers;

try {
  var command = new Command();
  command.Parse(args);
  var letters = command.Letters;
  if (letters?.Length > 0) {
    Console.WriteLine($"Letters: {letters}");
    var permuter = new Permuter(letters.Length);
    var permutations = permuter.Permute(letters, command.Optional)
      .ToList();

    if (command.Sort) permutations.Sort();

    Console.WriteLine($"{permutations.Count()} permutations found:");
#if DEBUG
    foreach (var permutation in permutations)
      Console.WriteLine(permutation);
#endif
  }
}
catch (ApplicationException ex) {
  Console.WriteLine(ex.Message);
}
catch (Exception ex) {
  Console.WriteLine(ex.ToString());
}
#if DEBUG
Console.Write("Press Enter");
Console.ReadLine();
#endif
