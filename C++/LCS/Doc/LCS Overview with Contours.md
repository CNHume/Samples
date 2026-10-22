### [The Longest Common Subsequence Problem](http://en.wikipedia.org/wiki/Longest_common_subsequence_problem)
by Christopher Hume, updated 2023-01-31

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

## Contours

Forward Contours FC[*k*] of *class k* are defined inductively, as follows:

FC[1] consists of those elements m1 for which there exists no element m2 strictly less than m1.

FC[*k*] consists of those elements m1 for which there exists no element m2 strictly less than m1, for m1 and m2 not already members of FC[*l*] where *l* &lt; *k*.

Reverse Contours RC[*k*] of *class k* are defined similarly, for elements m1 where there exists no m2 strictly greater than m1.

**Lemma 1a:** A chain including any member (i, j) of FC[*k*] corresponds to a *common prefix* of length *k* which ends at (i, j).

**Lemma 1b:** A chain including any member (i, j) of RC[*k*] corresponds to a *common suffix* of length *k* which begins at (i, j).

**Proof** *by Induction on length k*

*Initial Case, where length k = 1:*

FC[1] contains matches having no predecessors, and RC[1] contains matches having no successors.

These matches correspond to a prefix of length 1 ending at one of the matches in FC[1], and to a suffix of length 1 beginning at one of the matches in RC[1], respectively.

*Induction from the Case of length k to the Case of length k + 1:*

For each match m2 in FC[k + 1] there exists some match m1 in FC[k] such that m1 &lt; m2; otherwise m2 would be a member of FC[k] rather than FC[k + 1].

It will therefore be possible to construct a prefix of length k + 1 ending at m2 by appending the element corresponding to any of the matches m2 to the prefix ending at m1.

For each match m2 in RC[k + 1] there exists some match m1 in RC[k] such that m1 &gt; m2; otherwise m2 would be a member of RC[k] rather than RC[k + 1].

It will therefore be possible to construct a suffix of length k + 1 beginning at m2 by prepending the element corresponding to any of the matches m2 to the suffix beginning at m1.

**Lemma 2:** An LCS of length *p* has a common prefix of length *k* and a common suffix of length *p* - *k* + 1 for every *k* &isin; [1, *p*].

## Dominant Matches

There may exist distinct members m1 and m2 of a Forward Contour such that m2 &le; m1.

Dominant Matches of a Forward Contour are those m1 for which there exists no other member m2 such that m2 &le; m1.  Dominant Matches are the *Infimum*, or Meet (&and;) of the Forward Contour, with respect to the *non-strict* product-order (&le;).

There may exist distinct members m1 and m2 of a Reverse Contour such that m2 &ge; m1.

Dominant Matches of a Reverse Contour are those m1 for which there exists no other member m2 such that m2 &ge; m1.  Dominant Matches are the *Supremum*, or  Join (&or;) of the Reverse Contour, with respect to the *non-strict* product-order (&le;).

Where multiple Dominant Matches exist within a Meet (or Join, respectively) the Dominant Matches will be incomparable to each other.

## History

The chain/antichain viewpoint is the thread connecting the practical LCS
algorithms.  Its lineage:

- [Dilworth 1950][^1] supplied the poset foundation: the maximum size of an
  antichain equals the minimum number of chains in a chain decomposition (and
  dually, Mirsky's theorem).  A **chain** is a common subsequence; an
  **antichain** is a set of pairwise-incomparable matches.  This is the source
  of the chain concept, but it is not specific to LCS.
- [Hirschberg 1977][^7] first applied the chain idea to LCS as *contours*:
  computing the match classes (the matches of a given rank) rank by rank.
- [Apostolico & Guerra 1985][^8] made each contour cheap to walk by
  concentrating on its **dominant** (minimal) matches — the antichain that
  suffices to build the next contour — giving the first fast linear-space LCS
  beyond the `O(mn)` DP.
- [Apostolico, Browne & Guerra 1992][^9] sharpened this to `O(pm)` /
  `O(n(m−p))` time in linear space.
- [Rick 1995][^10] gave the "flexible algorithm" with
  `O(ns + min{pm, p(n−p)})`; [Rick 2000][^6] added the linear-space
  forward/backward construction.
- [Goeman & Clausen 2002][^2] re-derived the same bound from the dual
  antichain viewpoint.

Rick (2000) credits the split: computing contours rank by rank was "first
suggested by Hirschberg [6]", with "efficiency … later gained by concentrating
on the computation of dominant matches [3]" (Apostolico et al. 1992).

### The DP frontier

The classic LCS dynamic program fills an `(m+1)×(n+1)` table of prefix LCS
lengths — `O(mn)` cells, nearly all of them irrelevant.  Its **frontier** is
the sparse boundary of that computation: the matches lying on the edge of the
region already settled.  The frontier of rank `k` is exactly the contour
`FC[k]`, and it can be represented by its dominant (minimal) matches alone.
The whole DP thus collapses to a walk over the contours, each step described by
a handful of dominant matches instead of `O(mn)` cells — the compression that
turns `O(mn)` into Hirschberg's `O(pn)` and Rick's
`O(ns + min{pm, p(n−p)})`.

| DP view | Sparse view |
|---|---|
| fill `O(mn)` cells | walk only the frontier |
| the frontier at length `k` | the contour `FC[k]` |
| all rank-`k` matches on it | only its dominant (minimal) matches |

## Background

Where the number of symbols appearing in matches is small relative to the length of the input strings, reuse of the symbols increases; and the number of matches will tend towards O(*m\*n*) quadratic growth.  This occurs, for example, in the Bioinformatics application of nucleotide and protein sequencing.

The divide-and-conquer approach of [Hirschberg 1975][^3] limits the space required to O(*n*).  However, this approach requires O(*m\*n*) time even in the best case.

This quadratic time dependency may become prohibitive, given very long input strings.  Thus, heuristics are often favored over optimal Dynamic Programming solutions.

In the application of comparing file revisions, records from the input files form a large symbol space; and the number of symbols approaches the length of the LCS.  In this case the number of matches reduces to O(*n*) linear growth.

A binary search optimization due to [Hunt and Szymanski 1977][^5] can be applied to the basic Dynamic Programming approach, resulting in an expected performance of O(*n log m*).  Performance can degrade to O(*m\*n log m*) time in the worst case, as the number of matches grows to O(*m\*n*).

## Note

[Rick 2000][^6] describes a linear-space algorithm with a time bound of O(*n\*s + p\*min(m, n - p)*).

## Legend

    A, B are input strings of lengths m, n respectively (m ≤ n)
    p is the length of the LCS
    M is the set of matches (i, j) such that A[i] = B[j]
    r is the magnitude of M
    s is the magnitude of the alphabet Σ of distinct symbols in A + B
    d is the number of dominant matches
    n − p is the gap: the number of positions in B not on the LCS

The Rick bound `O(ns + min{pm, p(n−p)})` is three terms: `ns`, the occurrence
table build (one next-occurrence table per symbol, over the longer string, paid
once); `pm`, the *match side* — `p` ranks, each contour an antichain of at most
`m` matches; and `p(n−p)`, the *gap side* — `p` ranks times the `n−p`
off-diagonal positions.  The `min` is `p·min{m, n−p}`: the smaller of the two
sides.

On near-identical inputs the gap side dominates: each edit shifts the path off
the diagonal for every rank above it, so `d ≈ p(n−p)/2`.  The dominant-match
count is therefore `d = O(min{pm, p(n−p)} + p)` — the `+p` carrying the
on-diagonal matches when the strings are identical — and the sweep adds `O(m)`,
making the honest time `O(ns + m + min{pm, p(n−p)})`.

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
