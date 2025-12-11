//
// Conditionals
//
//#define CheckForDuplicates

namespace Extensions;

public static class PermutationExtensions {
  #region Methods
#if CheckForDuplicates
  extension(HashSet<string> permutations) {
    public void AddPermutation(string permutation) {
      var isUnique = permutations.Add(permutation);
      if (!isUnique)
        throw new ApplicationException($"Duplicate {permutation} found");
    }
  }
#else
  extension(List<string> permutations) {
    public void AddPermutation(string permutation) {
      permutations.Add(permutation);
    }
  }
#endif
  #endregion                            // Methods
}
