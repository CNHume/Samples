# (C) Copyright 2019, Christopher N. Hume.  All rights reserved.
# 2019-01-21  CNHume  Created RestaurantManager module

from functools import reduce
from operator import mul

from table_size_counter import TableSizeCounter
from best_candidate import BestCandidate


class RestaurantManager:
  """
    Manage a restaurant with tables of different sizes that can seat 2, 4 or 6 people.
    Tables can be grouped together to form larger tables in any configuration,
    i.e., you can put two 4 seat tables together to get an 8 seat table.
    """

  def __init__(self, size_count_pairs):
    self.tsc = TableSizeCounter(size_count_pairs)
    self.seatings = []

  def dump(self):
    print
    print(f"# seatings = {self.seatingCount()}")
    self.tsc.dump()

  def seatingCount(self):
    return len(self.seatings)

  def getCountModuli(self):
    return [count + 1 for size, count in self.tsc]

  def getPermutationCount(self, moduli):
    return reduce(mul, moduli, 1)

  def getSizeCounts(self, moduli, n):
    """Decode an index into the table count permutation it represents."""
    counts = []
    for modulus in moduli:
      n, count = divmod(n, modulus)
      counts.append(count)
    return counts

  def generatePermutations(self):
    """Generate all size, count permutations over the free tables"""
    sizes = self.tsc.sizes()
    moduli = self.getCountModuli()
    for n in range(self.getPermutationCount(moduli)):
      counts = self.getSizeCounts(moduli, n)
      size_count_pairs = zip(sizes, counts)
      yield size_count_pairs

  def showPermutations(self):
    print(f"sizes = {self.tsc.sizes()}")
    print
    print("size, count permutations:")
    for permutation in self.generatePermutations():
      print(permutation)

  def getTables(self, group_size):
    """Return optimal seating for group_size"""
    bc = BestCandidate(group_size, self.tsc.tableCount(), self.tsc.seatCount())
    return bc.best(self.generatePermutations())

  def seat(self, group_size):
    """Allocate tables needed to seat a group"""
    seating = self.getTables(group_size)
    if seating:
      self.tsc.sub(seating)
      self.seatings.append(seating)
    return seating

  def unseat(self, seating):
    """Free the tables when a group is unseated"""
    if seating in self.seatings:
      self.seatings.remove(seating)
      self.tsc.add(seating)

  def seatGroups(self, group_sizes):
    return [self.seat(group_size) for group_size in group_sizes]

  def unseatGroups(self, seatings):
    for seating in seatings:
      self.unseat(seating)

  def test(self, group_sizes):
    self.dump()
    print
    seatings = self.seatGroups(group_sizes)
    for group_size, seating in zip(group_sizes, seatings):
      print(f"seated {group_size} at {seating}")
    self.dump()

    self.unseatGroups(seatings)
    self.dump()
