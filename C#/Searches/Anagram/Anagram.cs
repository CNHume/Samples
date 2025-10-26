using System.Text;

namespace Searches;

using static System.String;

public class Anagram {
  #region Methods
  /// <summary>
  /// Returns the list of anagrams corresponding to every permutation of a string of letters
  /// </summary>
  /// <param name="letters">A string containing the letters to be permuted</param>
  /// <param name="prefix">A flag requesting that anagram prefixes be included</param>
  /// <returns>The list of anagrams</returns>
  public static IEnumerable<string> Anagrams(string letters, bool prefix = false) {
    List<string> anagrams = [];
    if (prefix) anagrams.Add(Empty);
    HashSet<char> chosen = [];
    StringBuilder sb = new();
    for (var n = 0; n < letters.Length; n++) {
      var letter = letters[n];
      if (chosen.Contains(letter)) continue;
      chosen.Add(letter);
      var remaining = sb
        .Clear()
        .Append(letters.AsSpan(0, n))
        .Append(letters.AsSpan(n + 1))
        .ToString();
      var suffixes = Anagrams(remaining, prefix);
      if (suffixes.Any())
        foreach (var suffix in suffixes)
          anagrams.Add(letter + suffix);
      else
        anagrams.Add(letter.ToString());
    }
    return anagrams;
  }
  #endregion                            // Methods
}
