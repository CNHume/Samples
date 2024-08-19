//
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
using System.Text;

//namespace Anagram;

try {
  var cmd = new Command();
  cmd.Parse(args);

  if (cmd.Letters != null) {
    Console.WriteLine($"Letters: {cmd.Letters}");
    var words = Anagram(cmd.Letters, cmd.Subset);

    Console.WriteLine($"{words.Count} anagrams found:");
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

#region Methods
/// <summary>
/// Returns the list of anagrams corresponding to every permutation of a list of letters
/// </summary>
/// <param name="letters">A string containing the list of letters to use</param>
/// <param name="subset">A flag indicating whether anagrams may be formed from subsets of the letters</param>
/// <returns>The list of anagrams</returns>
static List<string> Anagram(string letters, bool subset = false) {
  List<string> words = [];
  var length = letters.Length;
  if (length == 0)
    return words;

  if (subset)
    words.Add(string.Empty);

  if (length == 1) {
    words.Add(letters);
    return words;
  }

  HashSet<char> chosen = [];
  StringBuilder sb = new();
  for (var n = 0; n < length; n++) {
    var letter = letters[n];
    if (chosen.Contains(letter)) continue;
    chosen.Add(letter);
    var remaining = sb
      .Clear()
      .Append(letters.AsSpan(0, n))
      .Append(letters.AsSpan(n + 1))
      .ToString();
    var suffixes = Anagram(remaining, subset);
    foreach (var suffix in suffixes)
      words.Add(letter + suffix);
  }

  return words;
}
#endregion                              // Methods
