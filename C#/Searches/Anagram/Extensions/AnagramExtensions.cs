namespace Extensions;

public static class AnagramExtensions {
  #region Methods
  extension(HashSet<string> anagrams) {
    public bool AddAnagram(string anagram) {
      var isAdded = anagrams.Add(anagram);
      if (!isAdded)
        throw new ApplicationException($"Duplicate {anagram} found");
      return isAdded;
    }
  }
  #endregion                            // Methods
}
