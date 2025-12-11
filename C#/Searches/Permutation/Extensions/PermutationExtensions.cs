namespace Extensions;

public static class PermutationExtensions {
  #region Methods
  extension(HashSet<string> permutations) {
    public void AddPermutation(string permutation) {
      var isUnique = permutations.Add(permutation);
      if (!isUnique)
        throw new ApplicationException($"Duplicate {permutation} found");
    }
  }

  extension(List<string> permutations) {
    public void AddPermutation(string permutation) {
      permutations.Add(permutation);
    }
  }
  #endregion                            // Methods
}
