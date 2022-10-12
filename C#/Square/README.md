# Application Notes for the Square Chess Engine

by Christopher N. Hume, updated 2022-10-12

## Source Code

Square is a Universal Chess Interface (UCI) Engine.  Please see [Description of UCI Protocol](https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/) by Stefan Meyer-Kahlen.

Source code for the [Square Chess Engine](https://github.com/CNHume/Samples/tree/master/C%23/Square) can be found on GitHub.  This code is available under the terms of the [GNU General Public License (GPL)](https://www.gnu.org/licenses/gpl-3.0.html).

## Copyright and Licensing

Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.

This program comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to redistribute it under certain conditions.  See **COPYING.txt** and **COPYING_LESSER.txt** for details.

## Solution and Project Structure

The Square .NET solution consists of the following projects:

* Square - The Main Console Application Startup Project
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

* Move Notation
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
* Move Sorting
* Killer Heuristic

### Move Execution

* Piece Moves & Captures
* Pawn Moves & Captures (including En Passant & Promotion)
* Castling (including Chess 960 support)

### Search Methods

* Iterative Deepening
* Principal Variation Search (PVS)
* Backward Pruning Optimizations
* Forward Pruning Heuristics
* Quiescence Search
* Static Exchange Evaluation (SEE)
* Move Path Enumeration (Perft)

### Evaluation

* Eval Methods
* Draw Detection

### Cacheing

* Transposition Tables
* Eval Memoization
* Hash Functions

## References

### The Programming Chess Wiki
* [Chess Programming Wiki Main Page](https://www.chessprogramming.org/Main_Page)
