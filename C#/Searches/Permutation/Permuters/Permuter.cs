namespace Permuters;

using Extensions;

using static System.String;

public class Permuter {
  #region Properties
  public HashSet<char>[] Chosen { get; }
  #endregion                            // Properties

  #region Constructors
  public Permuter(int length) {
    Chosen = new HashSet<char>[length];
  }
  #endregion                            // Constructors

  #region Methods
  /// <summary>
  /// An enumerator over the permutations of a string of letters
  /// </summary>
  /// <param name="letters">A string containing the letters to be permuted</param>
  /// <param name="isOptional">A bool indicating that subpermutations are optional</param>
  /// <returns>An enumerator over the permutations</returns>
  public IEnumerable<string> Permute(
    string letters, bool isOptional = false) {
    HashSet<string> permutations = [];
    if (isOptional) permutations.Add(Empty);

    var length = letters.Length;
    if (length > 1) {
      int index = length - 1;
      if (Chosen[index] == null)
        Chosen[index] = new(length);
      else
        Chosen[index].Clear();

      for (var n = 0; n < length; n++) {
        var letter = letters[n];
        if (Chosen[index].Contains(letter))
          continue;
        else
          Chosen[index].Add(letter);

        var prefix = letters.AsSpan(0, n);
        var suffix = letters.AsSpan(n + 1);
        var subString = Concat(prefix, suffix);
        var subPermutations = Permute(subString, isOptional);
        foreach (var subPermutation in subPermutations)
          permutations.AddPermutation(letter + subPermutation);
      }
    }
    else
      permutations.AddPermutation(letters);

    return permutations;
  }
  #endregion                            // Methods
}
