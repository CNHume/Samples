# -*- coding: utf-8 -*-
#
# Explorations into Python Sorting Protocols
#
# 2018-04-01  CNHume  Created File
#
def customSort(input):
    counts = {}
    for value in input:
        counts[value] = counts[value] + 1 if value in counts else 1

    #
    # Each pair is (k: input value, v: count)
    #
    pairs = [(k, v) for k, v in counts.items()]
    insertionSort(pairs)

    return [pair[0] for pair in pairs]

def insertionSort(entries):
    insertionSortSlice(entries, 0, len(entries) - 1)

def insertionSortSlice(entries, first, last):
    for index in range(first + 1, last + 1):
        insertionSortEntry(entries, first, index)

def insertionSortEntry(entries, first, index):
    entry = entries[index]
    while index > first and compare(entries[index - 1], entry) > 0:
        entries[index] = entries[index - 1]
        index -= 1
    entries[index] = entry

# Compound Comparison
def compare(entry1, entry2):
    # Compare count first
    if entry1[1] < entry2[1]:
        return -1
    elif entry1[1] > entry2[1]:
        return 1
    # Compare input value
    elif entry1[0] < entry2[0]:
        return -1
    elif entry1[0] > entry2[0]:
        return 1
    else:
        return 0

def main():
    input = [1, 2, 3, 2, 3, 4, 5, 4, 4]
    output = customSort(input)
    print(output)

main()
