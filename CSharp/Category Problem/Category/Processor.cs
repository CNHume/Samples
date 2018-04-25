//
// Copyright (C) 2018, Christopher N. Hume
//
// Code Sample to solve a Category Problem posed by Ingenio, LLC
//
// 2018-03-28 CNHume    Added Processor class
//
namespace CategoryProblem {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Text;

  class Processor {
    #region Constants
    private const string SEPARATOR = ", ";
    #endregion

    #region Properties
    public Dataset Dataset { get; set; }
    #endregion

    #region Constructors
    public Processor() {
      Dataset = new Dataset();
      Dataset.Load(Dataset.SampleCategories);
    }
    #endregion

    #region Methods
    /// <summary>
    /// Perform query requested in the Command object
    /// </summary>
    /// <param name="cmd">Parsed Command object</param>
    public void Query(Command cmd) {
      if (cmd.CategoryId.HasValue) {
        var id = (int)cmd.CategoryId;
        var category = Dataset.FindCategoryById(id);
        if (category == null)
          throw new ArgumentException($"CategoryId = {id} not found");

        Console.WriteLine($@"CategoryId = {category.Id},
ParentCategoryId = {category.ParentId},
Name = {category.Name},
Keywords = {category.Keywords}");
      }

      if (cmd.Level.HasValue) {
        var level = (int)cmd.Level;
        var join = string.Join(SEPARATOR, Dataset.FindCategoryIdsByLevel(level));

        Console.WriteLine($"Category Ids at Level {level}: {join}");
      }
    }
    #endregion
  }
}
