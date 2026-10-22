### [The Longest Common Subsequence Problem](http://en.wikipedia.org/wiki/Longest_common_subsequence_problem)
by Christopher Hume, updated 2023-01-29

Define a *subsequence* to be any output string obtained by deleting zero or more symbols from an input string.

The Longest Common Subsequence or LCS is a subsequence of maximum length common to two or more strings.

Let *A* &equiv; *A*[0]&hellip; *A*[m - 1] and *B* &equiv; *B*[0]&hellip; *B*[n - 1], m &lt; n be strings drawn from an alphabet &Sigma; of size s, containing every distinct symbol in A + B.

An ordered pair (i, j) will be called a match if *A*[i] = *B*[j], where 0 &le; i &lt; m and 0 &le; j &lt; n.

The set of matches **M** defines a relation over matches: **M**[i, j] &hArr; (i, j) &isin; **M**.

Define a *non-strict* [product-order](https://en.wikipedia.org/wiki/Product_order) (&le;) over ordered pairs, such that (i1, j1) &le; (i2, j2) &hArr; i1 &le; i2 and j1 &le; j2.  We define (&ge;) similarly.

We say ordered pairs p1 and p2 are *comparable* if either p1 &le; p2 or p1 &ge; p2 holds.  If i1 &lt; i2 and j2 &lt; j1 (or i2 &lt; i1 and j1 &lt; j2) then neither p1 &le; p2 nor p1 &ge; p2 are possible, and we say p1 and p2 are *incomparable*.

Define the *strict* product-order (&lt;), such that (i1, j1) &lt; (i2, j2) &hArr; i1 &lt; i2 and j1 &lt; j2.  We define (&gt;) similarly.

A chain **C** is a subset of **M** consisting of at least one element; and where either m1 &lt; m2 or m1 &gt; m2 for any pair of distinct elements m1 and m2.  An antichain **D** is any subset of **M** in which every pair of distinct elements m1 and m2 are incomparable.

A chain can be visualized as a strictly increasing curve that passes through matches (i, j) in the m\*n coordinate space of **M**[i, j].

Every Common Sequence of length *q* corresponds to a chain of cardinality *q*, over the set of matches **M**.  Thus, finding an LCS can be restated as the problem of finding a chain of maximum cardinality *p*.

According to [Dilworth 1950][^1], this cardinality *p* is equal to the minimum number of disjoint antichains into which **M** can be decomposed.  Note that such a decomposition of **M** into the minimum number of disjoint antichains may not be unique.

## History

The chain/antichain viewpoint is the thread connecting the practical LCS
algorithms.  [Dilworth 1950][^1] supplied the poset foundation above, though
it is not specific to LCS.  Its application to LCS:

- [Hirschberg 1975][^3] gave the linear-space divide-and-conquer (still
  `O(mn)` time).
- [Hunt & McIlroy 1976][^4] and [Hunt & Szymanski 1977][^5] gave the
  match-list and threshold algorithms.
- [Hirschberg 1977][^7] first computed the match classes rank by rank — the
  *contours* (defined in the "with Contours" overview).
- [Apostolico & Guerra 1985][^8] and [Apostolico, Browne & Guerra 1992][^9]
  made the contours efficient via *dominant* matches, in linear space.
- [Rick 1995][^10] and [Rick 2000][^6] packaged these into the
  `O(ns + min{pm, p(n−p)})` algorithms; [Goeman & Clausen 2002][^2] re-derived
  the bound from the dual antichain viewpoint.

## Background

Where the number of symbols appearing in matches is small relative to the length of the input strings, reuse of the symbols increases; and the number of matches will tend towards O(*m\*n*) quadratic growth.  This occurs, for example, in the Bioinformatics application of nucleotide and protein sequencing.

The divide-and-conquer approach of [Hirschberg 1975][^3] limits the space required to O(*n*).  However, this approach requires O(*m\*n*) time even in the best case.

This quadratic time dependency may become prohibitive, given very long input strings.  Thus, heuristics are often favored over optimal Dynamic Programming solutions.

In the application of comparing file revisions, records from the input files form a large symbol space; and the number of symbols approaches the length of the LCS.  In this case the number of matches reduces to O(*n*) linear growth.

A binary search optimization due to [Hunt and Szymanski 1977][^5] can be applied to the basic Dynamic Programming approach, resulting in an expected performance of O(*n log m*).  Performance can degrade to O(*m\*n log m*) time in the worst case, as the number of matches grows to O(*m\*n*).

## Note

[Rick 2000][^6] describes a linear-space algorithm with a time bound of O(*n\*s + p\*min(m, n - p)*).

## Legend

    A, B are input strings of lengths m, n respectively
    p is the length of the LCS
    M is the set of matches (i, j) such that A[i] = B[j]
    r is the magnitude of M
    s is the magnitude of the alphabet Σ of distinct symbols in A + B

## References

[^1]: ["A decomposition theorem for partially ordered sets"](https://doi.org/10.2307/1969503)
by Robert P. Dilworth, published January 1950,
Annals of Mathematics [Volume 51, Number 1, *pp.* 161-166]

[^2]: ["A New Practical Linear Space Algorithm for the Longest Common Subsequence Problem"](https://www.kybernetika.cz/content/2002/1/45)
by Heiko Goeman and Michael Clausen, published 2002,
Kybernetika [Volume 38, Issue 1, *pp.* 45-66]

[^3]: ["A linear space algorithm for computing maximal common subsequences"](https://doi.org/10.1145/360825.360861)
by Daniel S. Hirschberg, published June 1975
Communications of the ACM [Volume 18, Number 6, *pp.* 341-343]

[^4]: ["An Algorithm for Differential File Comparison"](https://www.cs.dartmouth.edu/~doug/diff.pdf)
by James W. Hunt and M. Douglas McIlroy, June 1976
Computing Science Technical Report, Bell Laboratories 41

[^5]: ["A Fast Algorithm for Computing Longest Common Subsequences"](https://doi.org/10.1145/359581.359603)
by James W. Hunt and Thomas G. Szymanski, published May 1977
Communications of the ACM [Volume 20, Number 5, *pp.* 350-353]

[^6]: ["Simple and fast linear space computation of longest common subsequences"](https://doi.org/10.1016/S0020-0190(00)00100-1)
by Claus Rick, received 17 March 2000, Information Processing Letters,
Elsevier Science [Volume 75, *pp.* 275–281]

[^7]: ["Algorithms for the longest common subsequence problem"](https://doi.org/10.1145/322033.322044)
by Daniel S. Hirschberg, published October 1977
Journal of the ACM [Volume 24, Number 4, *pp.* 664-675]

[^8]: ["A fast linear space algorithm for computing longest common subsequences"](https://www.semanticscholar.org/paper/A-Fast-Linear-Space-Algorithm-for-Computing-Longest-Apostolio-Guerra/7d380f831959f869af2dcacad264c95468822109)
by Alberto Apostolico and Concettina Guerra, published 1985
Proceedings of the 23rd Allerton Conference on Communication, Control, and Computing, *pp.* 76-84

[^9]: ["Fast linear-space computations of longest common subsequences"](https://www.sciencedirect.com/science/article/pii/030439759290132Y)
by Alberto Apostolico, Stephen Browne, and Concettina Guerra, published 1992
Theoretical Computer Science [Volume 92, *pp.* 3-17]

[^10]: ["A new flexible algorithm for the longest common subsequence problem"](https://theory.cs.uni-bonn.de/blum/Mitarbeiter/rick/welcome-english.html)
by Claus Rick, published 1995
Proceedings of the 6th Annual Symposium on Combinatorial Pattern Matching (CPM '95),
Lecture Notes in Computer Science 937, Springer, *pp.* 340-351.  [DOI](https://doi.org/10.1007/3-540-60044-2_53)
