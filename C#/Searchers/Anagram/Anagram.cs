using System.Text;

namespace Searchers;

using static System.String;

public class Anagram {
  #region Methods
  /// <summary>
  /// Returns the list of anagrams corresponding to every permutation of a list of letters
  /// </summary>
  /// <param name="letters">A string containing the list of letters to use</param>
  /// <param name="subset">A flag indicating whether anagrams may be formed from subsets of the letters</param>
  /// <returns>The list of anagrams</returns>
  public static IEnumerable<string> Anagrams(string letters, bool subset = false) {
    List<string> words = [];
    if (subset) words.Add(Empty);
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
      var anagrams = Anagrams(remaining, subset);
      if (anagrams.Any())
        foreach (var anagram in anagrams)
          words.Add(letter + anagram);
      else
        words.Add(letter + Empty);
    }
    return words;
  }
  #endregion                            // Methods
}
