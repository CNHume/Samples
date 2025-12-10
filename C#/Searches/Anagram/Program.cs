//
// Copyright (C) 2018-2025, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
//[2018-12-22 CNHume]Created Anagram .NET Console Application
//
// To publish:
// dotnet publish Anagram -c Release -r win10-x64
//
// Usage: Anagram [-p] [-s] letters
//

using Anagrams;

using Parsers;

try {
  var command = new Command();
  command.Parse(args);
  var letters = command.Letters;
  if (letters?.Length > 0) {
    Console.WriteLine($"Letters: {letters}");
    var agram = new Anagram(letters.Length);
    var anagrams = agram.Anagrams(letters, command.Prefix)
      .ToList();

    if (command.Sort) anagrams.Sort();

    Console.WriteLine($"{anagrams.Count()} anagrams found:");
#if DEBUG
    foreach (var anagram in anagrams)
      Console.WriteLine(anagram);
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
