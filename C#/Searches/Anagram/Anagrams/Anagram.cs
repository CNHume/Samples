namespace Anagrams;

using Extensions;

using static System.String;

public class Anagram {
  #region Properties
  public HashSet<char>[] Chosen { get; }
  #endregion                            // Properties

  #region Constructors
  public Anagram(int length) {
    Chosen = new HashSet<char>[length];
  }
  #endregion                            // Constructors

  #region Methods
  /// <summary>
  /// Returns the list of anagrams corresponding to every permutation of a string of letters
  /// </summary>
  /// <param name="letters">A string containing the letters to be permuted</param>
  /// <param name="prefix">A flag requesting that anagram prefixes be included</param>
  /// <returns>The list of anagrams</returns>
  public IEnumerable<string> Anagrams(
    string letters, bool prefix = false) {
    HashSet<string> anagrams = [];
    if (prefix) anagrams.Add(Empty);
    var length = letters.Length;
    int index = length - 1;
    if (Chosen[index] == null)
      Chosen[index] = [];
    else
      Chosen[index].Clear();

    for (var n = 0; n < length; n++) {
      var letter = letters[n];
      if (Chosen[index].Contains(letter))
        continue;
      else
        Chosen[index].Add(letter);

      var remaining = Concat(
        letters.AsSpan(0, n),
        letters.AsSpan(n + 1));

      if (length > 1) {
        var suffixes = Anagrams(remaining, prefix);
        foreach (var suffix in suffixes)
          anagrams.AddAnagram(letter + suffix);
      }
      else
        anagrams.AddAnagram(letter.ToString());
    }

    return anagrams;
  }
  #endregion                            // Methods
}
