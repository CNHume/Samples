# README
### Code Samples by Christopher N. Hume
### updated 2025-04-13

## What is this repository for?

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

## How do I get set up?

The Python samples assume a Python 3.13 environment.

Some of the C# projects may require C# 12 features provided by Visual Studio 2022, and the .NET
8.0 Framework.

See the following Microsoft Download link to obtain the [Free Visual Studio 2022 Community Edition](https://visualstudio.microsoft.com/vs/), currently at version 17.10 Preview 3.  Installation includes a [current release of the .NET SDK](https://dotnet.microsoft.com/en-us/download).


To **Open the PMC** via *Tools > NuGet Package Manager > Package Manager Console*.

Here are some useful PMC commands:

```
dotnet tool list -g
dotnet tool update dotnet-ef -g

dotnet --list-runtimes
dotnet --list-sdks
dotnet --info [To list both sdks and runtimes]
```

The Othello player was written in Java 1.8 using a NetBeans 8.0 IDE.

The allegro-projects can be run in the Free Express Edition of Allegro Common Lisp 10, available at https://franz.com/downloads.

## Who do I talk to?

Please send any comments or suggestions to cnhume-software@yahoo.com
