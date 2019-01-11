# -*- coding: utf-8 -*-
# (C) Copyright 2019, Christopher N. Hume.  All rights reserved.
# 2019-01-07  CNHume  Simple Restaurant Manager written for Dan Dodd of Numerator
from operator import mul

class TableSizeCounter:
    """
    Maintains a dictionary of counts for tables of a given size.
    """
    # size_count_pairs is a list of tuples which consist of a size (or seat count) and a count of tables of that size
    def __init__(self, size_count_pairs):
        # Python allows dict(size_count_pairs) here; but we want to add, rather than replace, tables of the same size:
        self.counter = {}
        self.add(size_count_pairs)

    def __iter__(self):
        #[Version]Replace iteritems() with items() under Python 3:
        return self.counter.iteritems()

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
    Maintains the best among a series of candidate seatings.
    A candidate consists of a list of table size, count pairs.
    """
    def __init__(self, group_size):
        self.group_size = group_size
        self.clear()

    def clear(self):
        self.candidate = None
        self.seat_sum = 0
        self.table_sum = 0

    def update(self, candidate):
        seat_sum = TableSizeCounter.seatSum(candidate)
        if seat_sum < self.group_size:
            return

        table_sum = TableSizeCounter.tableSum(candidate)

        # The following relation controls the optimization policy:
        updated = not self.candidate or table_sum < self.table_sum or table_sum == self.table_sum and seat_sum < self.seat_sum

        if updated:
            self.candidate = candidate
            self.seat_sum = seat_sum
            self.table_sum = table_sum

        return updated

class RestaurantManager:
    """
    Manages a restaurant with tables of different sizes that can seat 2, 4 or 6 people.
    Tables can be grouped together to form larger tables in any configuration,
    i.e., you can put two 4 seat tables together to get an 8 seat table.
    """
    def __init__(self, size_count_pairs):
        self.tsc = TableSizeCounter(size_count_pairs)
        self.seatings = []

    def seatingCount(self):
        return len(self.seatings)

    def getCountModuli(self):
        return [count + 1 for size, count in self.tsc]

    def getCandidateCount(self, moduli):
        return reduce(mul, moduli, 1)

    def getSizeCounts(self, moduli, n):
        counts = []
        for modulus in moduli:
            n, count = divmod(n, modulus)
            counts.append(count)
        return counts

    def genCandidates(self):
        # Return all size, count permutations over the free tables
        sizes = self.tsc.sizes()
        moduli = self.getCountModuli()
        for n in range(self.getCandidateCount(moduli)):
            counts = self.getSizeCounts(moduli, n)
            candidate = zip(sizes, counts)
            yield candidate
    
    def getTables(self, group_size):
        # Return optimal seating for group_size
        best = BestCandidate(group_size)
        for candidate in self.genCandidates():
            best.update(candidate)
        return best.candidate

    def seat(self, group_size):
        seating = self.getTables(group_size)
        if seating:
            self.tsc.sub(seating)
            self.seatings.append(seating)
        return seating
    
    def unseat(self, seating):
        if seating in self.seatings:
            self.seatings.remove(seating)
            self.tsc.add(seating)

    def dump(self):
        print
        for size, count in self.tsc:
            print("size = {}, count = {}".format(size, count))

        print
        print("# tables = {}".format(self.tsc.tableCount()))
        print("# seats = {}".format(self.tsc.seatCount()))
        print("# seatings = {}".format(self.seatingCount()))

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
