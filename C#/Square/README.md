# Application Notes for the Square Chess Engine

by Christopher N. Hume, updated 2022-10-12

## Source Code

Square is a Universal Chess Interface (UCI) Engine.  Please see [Description of UCI Protocol](https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/) by Stefan Meyer-Kahlen.

Source code for the [Square Chess Engine](https://github.com/CNHume/Samples/tree/master/C%23/Square) can be found on GitHub.  The code is available under terms of the [GNU General Public License (GPL)](https://www.gnu.org/licenses/gpl-3.0.html).

## Copyright and Licensing

Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.

This program comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to redistribute it under certain conditions.  See **COPYING.txt** and **COPYING_LESSER.txt** for details.

## Solution and Project Structure

The Square .NET solution consists of the following projects:

* Square - Console Application Startup Project
* HeapSort - Heap Sort Library Project
* HeapTest - Heap Sort Library Test Project

### Target Framework

All projects in the solution currently target the **.NET 6.0 SDK**.

## Principal Components of the Square Engine

Methods for the Square Engine fall into the following categories:

### Input Methods

* Command Parser
* Command Dispatch Loop

### Output Methods

* Move Notations
* Board Display
* Logger

### Move Lookup Tables

* Orthogonal & Diagonal Ray Moves
* King & Knight Moves

### Bitboard Methods

* Trailing Zero Count (TZC)
* Rotated Bitboards vs. Magic Methods

### Move Generation

* Generator Methods
* Check Evasions
* Pin Restrictions
* Move Ordering
* Killer Move Heuristic

### Move Execution

* Board Representation
* Piece Moves & Captures
* Pawn Moves & Captures, including Promotion & En Passant
* Castling and Chess 960 Support

### Search Methods

* Iterative Deepening
* Principal Variation Search (PVS)
* Backward Pruning Optimizations
* Forward Pruning Heuristics
* Quiescent Search
* Static Exchange (Swap) Evaluation
* Move Path Enumeration, for Perft

### Position Evaluation

* Eval Methods
* Reward vs. Punishment Heuristics
* Pawn Features
* Shortest Mate
* Draw Detection

### Cache Methods

* Transposition Tables
* Material Balance Memoization
* Hash Functions

### Diagnostics & Statistics

* Counters & Histograms
* Test Positions (Tabiya)
* Timer Tests

## References

### The Programming Chess Wiki
* [Chess Programming Wiki Main Page](https://www.chessprogramming.org/Main_Page)
