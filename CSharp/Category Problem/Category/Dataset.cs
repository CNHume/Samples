//
// Copyright (C) 2018, Christopher N. Hume
//
// Code Sample to solve a Category Problem posed by Ingenio, LLC
//
// 2018-03-28 CNHume    Added Dataset class
//
namespace CategoryProblem {
  using System;
  using System.Collections.Generic;
  using System.Linq;

  using static Category;

  class Dataset {
    #region Constant Data
    /// <summary>
    /// Sample Dataset defined for the Ingenio Category Problem
    /// </summary>
    public static readonly Category[] SampleCategories = {
      //
      // Additional Test Categories:
      //
      //new Category(400, 300, "Foo"),
      //new Category(300, 109, "Bar"),
      //
      new Category(100, NULL_ID, "Business", "Money"),
      new Category(200, NULL_ID, "Tutoring", "Teaching"),
      new Category(101, 100, "Accounting", "Taxes"),
      new Category(102, 100, "Taxation"),
      new Category(201, 200, "Computer"),
      new Category(103, 101, "Corporate Tax"),
      new Category(202, 201, "Operating System"),
      new Category(109, 101, "Small Business Tax"),
    };
    #endregion

    #region Properties
    /// <summary>
    /// Key: CategoryId, Value: Category
    /// </summary>
    private Dictionary<int, Category> CategoryMap { get; set; }
    private List<Category> RootSet { get; set; }
    private List<IEnumerable<Category>> Levels { get; set; }
    #endregion

    #region Initialization
    private void EnsureCategoryMap() {
      if (CategoryMap == null)
        CategoryMap = new Dictionary<int, Category>();
      else
        CategoryMap.Clear();
    }

    private void EnsureRootSet() {
      if (RootSet == null)
        RootSet = new List<Category>();
      else
        RootSet.Clear();
    }

    private void EnsureLevels() {
      if (Levels == null)
        Levels = new List<IEnumerable<Category>>();
      else
        Levels.Clear();
    }
    #endregion

    #region Methods
    public void Load(IEnumerable<Category> categories) {
      AddChildren(categories);
      AddLevels(RootSet);
    }

    /// <summary>
    /// Build a list of sub-lists where each sub-list appears at a position in the outer list
    /// corresponding to the Level of those Categories contained in the sub-list.
    /// </summary>
    /// <param name="categories">Categories to be included in the list of lists</param>
    private void AddLevels(IEnumerable<Category> categories) {
      EnsureLevels();
      for (; categories.Any(); categories = NextGeneration(categories))
        Levels.Add(categories);
    }

    private IEnumerable<Category> NextGeneration(IEnumerable<Category> categories) {
      return categories.SelectMany(cat => CategoryMap[cat.Id].Children);
    }

    /// <summary>
    /// Build Map of Category Id values to their respective Categories 
    /// </summary>
    /// <param name="categories">Categories to be included in map</param>
    private void BuildCategoryMap(IEnumerable<Category> categories) {
      EnsureCategoryMap();
      foreach (var category in categories) {
        category.EnsureChildren();
        CategoryMap.Add(category.Id, category);
      }
    }

    /// <summary>
    /// Populate List of Children for each Category identified as a Parent
    /// </summary>
    /// <param name="categories">Categories to be included as Children</param>
    private void AddChildren(IEnumerable<Category> categories) {
      BuildCategoryMap(categories);
      EnsureRootSet();
      foreach (var category in categories) {
        var id = category.Id;
        var parentId = category.ParentId;

        if (parentId < 0) {
          category.Parent = null;
          RootSet.Add(category);
        }
        else if (CategoryMap.TryGetValue(parentId, out Category parentCategory)) {
          category.Parent = parentCategory;
          parentCategory.AddChild(category);
        }
        else
          throw new Exception($"CategoryId = {id} has an undefined ParentCategoryId = {parentId}");
      }
    }

    /// <summary>
    /// Find Category by Id, supplying the nearest non-empty
    /// Keyword string along the path of ancestral categories
    /// </summary>
    /// <param name="id"></param>
    /// <returns>Category object</returns>
    public Category FindCategoryById(int id) {
      if (CategoryMap.TryGetValue(id, out Category result)) {
        var category = result;
        for (; category != null && string.IsNullOrEmpty(category.Keywords); category = category.Parent) { }

        if (category != null && category != result)
          result = new Category(result) { Keywords = category.Keywords };
      }

      return result;
    }

    /// <summary>
    /// Enumerate the Category Id values appearing at a given level
    /// </summary>
    /// <param name="level">distance from root set</param>
    /// <returns>List of Category Id values</returns>
    public IOrderedEnumerable<int> FindCategoryIdsByLevel(int level) {
      return FindCategoriesByLevel(level).Select(cat => cat.Id).OrderBy(id => id);
    }

    /// <summary>
    /// Enumerate the Categories appearing at a given level
    /// </summary>
    /// <param name="level">distance from root set</param>
    /// <returns>List of Categories</returns>
    public IEnumerable<Category> FindCategoriesByLevel(int level) {
      return Levels[level];
    }
    #endregion
  }
}
