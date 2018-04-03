//
// Copyright (C) 2018, Christopher N. Hume
//
// Code Sample to solve the Category Problem posed by Ingenio, LLC
//
// 2018-03-28 CNHume    Added Category class
//
namespace IngenioSample {
  using System;
  using System.Collections.Generic;

  class Category {
    #region Constants
    public const int NULL_ID = -1;
    #endregion

    #region Properties
    public int Id { get; set; }
    public int ParentId { get; set; }
    public string Name { get; set; }
    public string Keywords { get; set; }

    public Category Parent { get; set; }
    public List<Category> Children { get; set; }
    #endregion

    #region Constructors
    public Category(int id, int parentId = NULL_ID, string name = null, string keywords = null,
      Category parent = null, List<Category> children = null) {
      this.Id = id;
      this.ParentId = parentId;
      this.Name = name;
      this.Keywords = keywords;
      this.Parent = parent;
      this.Children = children;
    }

    public Category(Category category) :
      this(category.Id, category.ParentId, category.Name, category.Keywords,
        category.Parent, category.Children) {
    }
    #endregion

    #region Initialization
    public void EnsureChildren() {
      if (Children == null)
        Children = new List<Category>();
      else
        Children.Clear();
    }
    #endregion

    #region Methods
    public void AddChild(Category child) {
      Children.Add(child);
    }
    #endregion
  }
}
