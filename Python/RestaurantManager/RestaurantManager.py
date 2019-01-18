# -*- coding: utf-8 -*-
# (C) Copyright 2019, Christopher N. Hume.  All rights reserved.
# 2019-01-07  CNHume  Restaurant Manager to allocate tables and seat groups of varying size
from __future__ import division
from operator import mul

class TableSizeCounter:
    """
    Maintains a dictionary of counts for tables of a given size.
    """
    # size_count_pairs is a list of tuples which consist of a size (or seat count) and a count of tables of that size.
    def __init__(self, size_count_pairs):
        # Python allows dict(size_count_pairs) here; but we want to add, rather than replace, tables of the same size:
        self.counter = {}
        self.add(size_count_pairs)

    def __iter__(self):
        #[Version]Replace the following call to iteritems() with items() under Python 3:
        return self.counter.iteritems()

    def dump(self):
        print("# tables = {}".format(self.tableCount()))
        print("# seats = {}".format(self.seatCount()))
        print
        for size, count in self:
            print("size = {}, count = {}".format(size, count))

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

class BestCandidate:
    """
    Identify the best seating when presented a series of candidates.  Each candidate is a list of size_count_pairs.
    """
    def __init__(self, group_size, table_count, seat_count):
        self.group_size = group_size
        self.table_count = table_count
        self.seat_count = seat_count
        self.clear()

    def clear(self):
        self.candidate = None
        self.loss = 0

    def loss0(self, table_sum, seat_sum):
        return table_sum * self.seat_count + seat_sum

    def loss1(self, table_sum, seat_sum):
        return seat_sum * self.table_count + table_sum

    def loss2(self, table_sum, seat_sum):
        return (table_sum / self.table_count)**2 + (seat_sum / self.seat_count)**2

    def update(self, candidate):
        seat_sum = TableSizeCounter.seatSum(candidate)
        if seat_sum < self.group_size:
            return

        table_sum = TableSizeCounter.tableSum(candidate)
        loss = self.loss0(table_sum, seat_sum)

        # Does candidate improve the optimal seating?
        update = not self.candidate or loss < self.loss
        if update:
            self.candidate = candidate
            self.loss = loss

    def best(self, candidates):
        """Find optimal candidate for group_size"""
        self.clear()
        for candidate in candidates:
            self.update(candidate)
        return self.candidate

class RestaurantManager:
    """
    Manages a restaurant with tables of different sizes that can seat 2, 4 or 6 people.
    Tables can be grouped together to form larger tables in any configuration,
    i.e., you can put two 4 seat tables together to get an 8 seat table.
    """
    def __init__(self, size_count_pairs):
        self.tsc = TableSizeCounter(size_count_pairs)
        self.seatings = []

    def dump(self):
        print
        print("# seatings = {}".format(self.seatingCount()))
        self.tsc.dump()

    def seatingCount(self):
        return len(self.seatings)

    def getCountModuli(self):
        return [count + 1 for size, count in self.tsc]

    def getCandidateCount(self, moduli):
        return reduce(mul, moduli, 1)

    def getSizeCounts(self, moduli, n):
        """Decode a candidate index into the list of table counts that it represents."""
        counts = []
        for modulus in moduli:
            n, count = divmod(n, modulus)
            counts.append(count)
        return counts

    def genCandidates(self):
        """Generate all size, count permutations over the free tables"""
        sizes = self.tsc.sizes()
        moduli = self.getCountModuli()
        for n in range(self.getCandidateCount(moduli)):
            counts = self.getSizeCounts(moduli, n)
            size_count_pairs = zip(sizes, counts)
            yield size_count_pairs
    
    def getTables(self, group_size):
        """Return optimal seating for group_size"""
        bc = BestCandidate(group_size, self.tsc.tableCount(), self.tsc.seatCount())
        return bc.best(self.genCandidates())

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
 
    def test0(self):
        print("sizes = {}".format(self.tsc.sizes()))
        print
        for candidate in self.genCandidates():
            print("candidate = {}".format(candidate))

    def test1(self):
        self.dump()
        print
        s1 = self.seat(5)
        print("s1 = {}".format(s1))
        s2 = self.seat(6)
        print("s2 = {}".format(s2))
        s3 = self.seat(7)
        print("s3 = {}".format(s3))
        self.dump()

        self.unseat(s1)
        self.unseat(s2)
        self.unseat(s3)
        self.dump()

def main():
    size_count_pairs = [ (2, 4), (4, 2), (6, 1) ]
    rm = RestaurantManager(size_count_pairs)
    # rm.test0()
    rm.test1()

main()
