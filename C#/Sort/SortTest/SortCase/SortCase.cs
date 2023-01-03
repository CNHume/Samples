//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
using System.ComponentModel.DataAnnotations;

namespace SortTest {
  public enum SortCase : byte {
    None,
    [Display(Name = "Ascending")]
    Asc,
    [Display(Name = "Descending")]
    Desc,
    [Display(Name = "Random")]
    Rand
  }
}
