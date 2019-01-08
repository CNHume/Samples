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

    def add(self, size_count_pairs):
        for size, count in size_count_pairs:
            self.counter[size] = self.counter[size] + count if size in self.counter else count

    def sub(self, size_count_pairs):
        for size, count in size_count_pairs:
            self.counter[size] = self.counter[size] - count if size in self.counter and self.counter[size] > count else 0

    def seatCount(self):
        return TableSizeCounter.seatCount1(self.counter.items())

    @staticmethod
    def seatCount1(size_count_pairs):
        return sum([size * count for size, count in size_count_pairs])

    def tableCount(self):
        return TableSizeCounter.tableCount1(self.counter.items())

    @staticmethod
    def tableCount1(size_count_pairs):
        return sum([count for size, count in size_count_pairs])

class BestCandidate:
    """
    Maintains the best among a series of candidate seatings.
    A candidate consists of a list of table size, count pairs.
    """
    def __init__(self):
        self.candidate = None
        self.seat_count = 0
        self.table_count = 0

    def compare(self, group_size, candidate):
        seat_count = TableSizeCounter.seatCount1(candidate)
        if seat_count < group_size:
            return

        table_count = TableSizeCounter.tableCount1(candidate)

        # The following relation controls the optimization policy:
        update = not self.candidate or table_count < self.table_count or table_count == self.table_count and seat_count < self.seat_count

        if update:
            self.candidate = candidate
            self.seat_count = seat_count
            self.table_count = table_count

class RestaurantManager:
    """
    Manages a restaurant with tables of different sizes that can seat 2, 4 or 6 people.
    Tables can be grouped together to form larger tables in any configuration,
    i.e., you can put two 4 seat tables together to get a 8 seat table.
    """
    def __init__(self, size_count_pairs):
        self.tsc = TableSizeCounter(size_count_pairs)
        self.seatings = []

    def freeSeatCount(self):
        return self.tsc.seatCount()

    def freeTableCount(self):
        return self.tsc.tableCount()

    def seatingCount(self):
        return len(self.seatings)
    
    def unseat(self, seating):
        if seating in self.seatings:
            self.seatings.remove(seating)
            self.tsc.add(seating)

    def seat(self, group_size):
        seating = self.getTables(group_size)
        self.seatings.append(seating)
        return seating

    def getSizes(self):
        return [size for size in self.tsc.counter]

    def getCountLimits(self, offset = 1):
        return [count + offset for size, count in self.tsc.counter.items()]

    def getCandidateCount(self, counts):
        return reduce(mul, counts, 1)

    def getSizeCounts(self, counts, n):
        size_count_pairs = []
        for count in counts:
            n, size_count = divmod(n, count)
            size_count_pairs.append(size_count)
        return size_count_pairs
    
    def getCandidates(self):
        candidates = []
        sizes = self.getSizes()
        count_limits = self.getCountLimits()
        for n in range(self.getCandidateCount(count_limits)):
            counts = self.getSizeCounts(count_limits, n)
            candidate = zip(sizes, counts)
            candidates.append(candidate)
        return candidates
    
    def getTables(self, group_size):
        #should return the optiminal table(s) for this group size
        best = BestCandidate()
        for candidate in self.getCandidates():
            best.compare(group_size, candidate)

        if best.candidate:
            self.tsc.sub(best.candidate)
        return best.candidate

    def dump(self):
        print
        # print("TableSizeCounter = {}".format(self.tsc.counter.items()))
        for size, count in self.tsc.counter.items():
            print("size = {}, count = {}".format(size, count))

        print
        print("# tables = {}".format(self.freeTableCount()))
        print("# seats = {}".format(self.freeSeatCount()))
        print("# seatings = {}".format(self.seatingCount()))

    def test1(self):
        print("sizes = {}".format(self.getSizes()))
        # for candidate in self.getCandidates():
        #     print("candidate = {}".format(candidate))
        self.dump()
        print
        s1 = self.seat(5)
        print("s1 = {}".format(s1))
        s2 = self.seat(6)
        print("s2 = {}".format(s2))
        self.dump()

        self.unseat(s1)
        self.unseat(s2)
        self.dump()

def main():
    size_count_pairs = [ (2, 4), (4, 2), (6, 1) ]
    rm = RestaurantManager(size_count_pairs)
    rm.test1()

main()
