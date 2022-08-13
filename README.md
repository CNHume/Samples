# README

Code Samples written by Christopher N. Hume

### What is this repository for?

These samples are intended to be illustrative of my coding style. In addition to games, the other folders include implementations of Sorting and Searching and examples of more interesting algorithms as well.

The C++/LCS folder contains three implementations of my solution to The Longest Common Subsequence (LCS) Problem, which I have offered as the C++ implementation on Rosetta Code. LCS is perhaps best known as the algorithm used to identify the changes to made to a source file in a source control system.

I have held long-standing interest in Functional Programming. In the Lisp folder I have included my Allegro Common Lisp (ACL) implementation of a “graph-reducing combinator-compiler” for a head-first (a.k.a., lazy-evaluation) functional programming language that I refer to as SK.

This language is a more Lisp-like version SASL, a purely functional programming language developed by David Turner at the University of St Andrews in 1972.  The implementation thereby skirts the need for clever parsing, and makes use instead of the Lisp reader and macros (and the backquote operator).

The semantics closely follow those described in "A New Implementation Technique for Applicative Languages" by David A. Turner, a copy of which is provided in the SK sub-folder.

I originally wrote this code for a Symbolics Lisp Machine. It was then ported to Allegro Common Lisp: first running on Mac OS, then as run under Windows. The ANSI Common Lisp standard was still under development then. So; the code may not be strictly ANSI compliant.

While working in Lisp, I became interested in understanding the RSA public-key encryption algorithm developed by Rivest, Shamir and Adleman. I include my Lisp implementation in the RSA sub-folder.

The C# folder has a Square sub-folder which contains implementation of a Chess Engine written in C#.

The Sort sub-folder contains implementations of the standard Sorting Algorithms.

There is also a Java folder which contains a Bit-Board based Othello player.

### How do I get set up?

Most of the Python samples assume a Python 2.7 environment.

C# Samples are written in C# 9.0 and target the NET5.0 Framework.

The Othello player was written in Java 1.8 using a NetBeans 8.0 IDE.

The allegro-projects can be run in the Free Express Edition of Allegro Common Lisp 10, available at https://franz.com/downloads.

### Who do I talk to?

Send questions or comments to cnhume-software@yahoo.com
