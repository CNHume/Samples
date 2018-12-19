# README

Code Samples written by Christopher N. Hume

### What is this repository for?

These samples are intended to be illustrative of my coding style. In addition to games, the other folders include implementations of Sorting and Searching and examples of more interesting algorithms as well.

The LCS folder contains three implementations of my solution to The Longest Common Subsequence (LCS) Problem, which I have offered as the C++ implementation on Rosetta Code. LCS is perhaps best known as the algorithm used to identify the changes to made to a source file in a source control system.

I have maintained a long-standing interest in Functional Programming. In the allegro-projects folder I have included my implementation of a “graph-reducing combinator-compiler” for a head-first (a.k.a., lazy-evaluation) functional programming language which I designed and refer to as SK.

This language is a less prosaic, more Lisp-like version of SASL. This approach skirted the need for clever parsing, by leveraging use of the Lisp reader and macros (which are based on the backquote operator.) The semantics closely follow "A New Implementation Technique for Applicative Languages" by David A. Turner, a copy of which is provided under the SK sub-folder.

I originally wrote this code for a Symbolics Lisp Machine. It was then ported to Allegro Common Lisp: first running on Mac OS, then as run under Windows. The ANSI Common Lisp standard was still under development then. So; the code may not be strictly ANSI compliant.

When I was working in Lisp, I also wanted to better understand the RSA public-key encryption algorithm developed by Rivest, Shamir and Adleman. I include my Lisp implementation in the RSA sub-folder.

Other folders include implementation of the major Sorting Algorithms written in C# and a Bit-Board based Othello player written in Java.

### How do I get set up?

Most of the Python samples assume a Python 2.7 environment.

The C# Samples assume C# 6.0 and .NET 4.6.1 available in Visual Studio Community 2017.

The Othello player was written in Java 1.8 using a NetBeans 8.0 IDE.

The allegro-projects can be run in the Free Express Edition of Allegro Common Lisp 10, available at https://franz.com/downloads.

### Who do I talk to?

Send questions or comments to cnhume-software@yahoo.com
