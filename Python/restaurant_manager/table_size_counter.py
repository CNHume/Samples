# (C) Copyright 2019, Christopher N. Hume.  All rights reserved.
# 2019-01-21  CNHume  Created TableSizeCounter module


class TableSizeCounter:
  """
    Maintain a dictionary of counts for tables of a given size.
    """

  # size_count_pairs is a list of tuples which consist of a size (or seat count) and a count of tables of that size.
  def __init__(self, size_count_pairs):
    # Python allows dict(size_count_pairs) here; but we want to add, rather than replace, tables of the same size:
    self.counter = {}
    self.add(size_count_pairs)

  def __iter__(self):
    "Return size, count pairs"
    #[Version]Replace the following call to iteritems() with items() under Python 3:
    return self.counter.items()

  def dump(self):
    print(f"# tables = {self.tableCount()}")
    print(f"# seats = {self.seatCount()}")
    print
    for size, count in self:
      print(f"size = {size}, count = {count}")

  def add(self, size_count_pairs):
    for size, count in size_count_pairs:
      self.counter[size] = self.counter[size] + count if size in self.counter else count

  def sub(self, size_count_pairs):
    for size, count in size_count_pairs:
      self.counter[size] = self.counter[size] - count if size in self.counter and self.counter[size] > count else 0

  def sizes(self):
    return [size for size in self.counter]

  def seatCount(self):
    return TableSizeCounter.seatSum(self)

  def tableCount(self):
    return TableSizeCounter.tableSum(self)

  @staticmethod
  def seatSum(size_count_pairs):
    return sum([size * count for size, count in size_count_pairs])

  @staticmethod
  def tableSum(size_count_pairs):
    return sum([count for size, count in size_count_pairs])
