using System.Text;

namespace Searches;

using static System.String;

public class Anagram {
  #region Properties
  public static Dictionary<char, int> Chosen { get; } = [];
  #endregion                            // Properties

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
    StringBuilder sb = new();
    for (var n = 0; n < letters.Length; n++) {
      var letter = letters[n];
      var newCount = Chosen.TryGetValue(letter, out int oldCount) ?
        oldCount + 1 : 1;
      Chosen[letter] = newCount;

      var remaining = Concat(
        letters.AsSpan(0, n),
        letters.AsSpan(n + 1));
      var suffixes = Anagrams(remaining, prefix);

      if (suffixes.Any())
        foreach (var suffix in suffixes)
          anagrams.Add(letter + suffix);
      else
        anagrams.Add(letter.ToString());
#if TIDY_CHOSEN
      if (newCount <= 1)
        Chosen.Remove(letter);
      else
#endif
        Chosen[letter] = newCount - 1;
    }
    return anagrams;
  }
#endregion                            // Methods
}
