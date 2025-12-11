namespace Extensions;

public static class PermutationExtensions {
  #region Methods
  extension(HashSet<string> permutations) {
    public bool AddPermutation(string permutation) {
      var isUnique = permutations.Add(permutation);
      if (!isUnique)
        throw new ApplicationException($"Duplicate {permutation} found");
      return isUnique;
    }
  }
  #endregion                            // Methods
}
