﻿//
// Copyright (C) 2018-2024, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
//[2018-12-22 CNHume]Created Anagram (.Net Core) Console Application
//
// To publish:
// dotnet publish Anagram -c Release -r win10-x64
//
// To run:
// Anagram\bin\Release\netcoreapp2.1\win10-x64\publish\Anagram abc...
//
// Usage: Anagram [-s] letters
//

using Commands;

using Searchers;

try {
  var cmd = new Command();
  cmd.Parse(args);

  if (cmd.Letters != null) {
    Console.WriteLine($"Letters: {cmd.Letters}");
    var words = Anagram.Anagrams(cmd.Letters, cmd.Subset);

    Console.WriteLine($"{words.Count()} anagrams found:");
    foreach (var word in words)
      Console.WriteLine(word);
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
