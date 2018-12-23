//
// Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
//
//[2018-12-22 CNHume]Created Anagram (.Net Core) Console Application
//
// To publish:
// dotnet publish Anagram -c Release -r win10-x64
//
// To run:
// Anagram\bin\Release\netcoreapp2.1\win10-x64\publish>Anagram.exe abc...
//
namespace Anagram {
  using System;
  using System.Collections.Generic;

  class Program {
    #region Methods
    private static void Main(string[] args) {
      try {
        if (args.Length > 0) {
          var letters = args[0];
          Console.WriteLine($"Leters: {letters}");
          var words = Anagram(letters, true);
          Console.WriteLine($"{words.Count} anagrams found:");
          foreach (var word in words) {
            Console.WriteLine(word);
          }
        }
      }
      catch (Exception ex) {
        Console.WriteLine(ex.ToString());
      }
#if DEBUG
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }

    /// <summary>
    /// Returns the list of anagrams corresponding to every permutation of a list of letters
    /// </summary>
    /// <param name="letters">A string containing the list of letters to use</param>
    /// <param name="subset">A flag indicating whether anagrams may be formed from any subset of the letters</param>
    /// <returns>The list of anagrams</returns>
    public static List<string> Anagram(string letters, bool subset = false) {
      var words = new List<string>();
      if (subset) words.Add(string.Empty);
      var length = letters.Length;
      if (length < 1) { }
      else if (length < 2)
        words.Add(letters);
      else {
        var chosen = new HashSet<char>();
        for (var n = 0; n < length; n++) {
          var letter = letters[n];
          if (!chosen.Contains(letter)) {
            chosen.Add(letter);
            var left = letters.Substring(0, n);
            var right = n + 1 < length ? letters.Substring(n + 1, length - (n + 1)) : string.Empty;
            var suffixes = Anagram(left + right, subset);
            foreach (var suffix in suffixes)
              words.Add(letter + suffix);
          }
        }
      }
      return words;
    }
    #endregion
  }
}
