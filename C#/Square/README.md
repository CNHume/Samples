# Application Notes for the Square Chess Engine

by Christopher N. Hume, updated 2022-10-16

## Source Code

Square is a Universal Chess Interface (UCI) Engine.  See [Description of UCI Protocol](https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/) by Stefan Meyer-Kahlen.  An April 2004 version of this documentation can be found at [Description of the Universal Chess Interface](http://wbec-ridderkerk.nl/html/UCIProtocol.html).

Source code for the [Square Chess Engine](https://github.com/CNHume/Samples/tree/master/C%23/Square) can be found on GitHub.  This code is available under terms of the [GNU General Public License (GPL)](https://www.gnu.org/licenses/gpl-3.0.html).

## Copyright and License

Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
This program comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to redistribute it under certain conditions.  See **COPYING.txt** and **COPYING_LESSER.txt** for details.

## Solution and Project Structure

The Square .NET solution consists of the following projects:

* Square - Console Application Startup Project
* HeapSort - Heap Sort Library Project
* HeapTest - Heap Sort Library Test Project

### Target Framework

All projects in the solution currently target the [.NET 8.0 SDK](https://dotnet.microsoft.com/download/dotnet/8.0).

## Principal Components of the Square Engine

Methods for the Square Engine fall into the following categories:

### Input Methods

* Position Deserialization from FEN or EPD
* Command Parser
* Command Dispatch Loop

### Output Methods

* Position Serialization to FEN or EPD
* Move Notation
* Board Display
* Histogram Display
* Logger

### Move Lookup Tables

* Orthogonal & Diagonal Ray Moves
* King & Knight Moves

### Bitboard Methods

* de Bruijn Sequences vs. Trailing Zero Count (TZC)
* Magic Methods vs. Rotated Bitboards

### Move Generation

* Generator Methods
* Check Evasion
* Pin Restriction
* Move Ordering
* Killer Move Heuristic

### Move Execution

* Board Representation
* Piece Moves & Captures
* Pawn Moves & Captures, including Promotion & En Passant
* Castling and Chess 960 Support

### Search Methods

* Iterative Deepening
* Alpha-Beta Backward Pruning
* Principal Variation Search (PVS)
* Forward Pruning, e.g., Null Move Heuristic
* Quiescent Search
* Static Exchange (Swap) Evaluation
* Move Path Enumeration (Perft)
* Background Task for Search or Perft

### Position Evaluation

* Eval Methods
* Reward and Punishment Heuristics
* Pawn Features
* Shortest Mate
* Draw Detection

### Cache Methods

* Transposition Tables
* Material Balance Memoization
* Hash Functions

### Diagnostics & Statistics

* Counters
* Test Positions (Tabiya)
* Timer Tests

## References

### Chess Programming Wiki
* [Chess Programming Wiki Main Page](https://www.chessprogramming.org/Main_Page)
