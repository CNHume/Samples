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

Every Common Sequence of length *q* corresponds to a chain of cardinality *q*, over the set of matches **M**.  Thus, finding an LCS can be restated as the problem of finding a chain of maximum cardinality *p*.

A chain can be visualized as a strictly increasing curve that passes through matches (i, j) in the m\*n coordinate space of **M**[i, j].

According to [Dilworth 1950][^1], this cardinality *p* is equal to the minimum number of disjoint antichains into which **M** can be decomposed.  Note that such a decomposition of **M** into the minimum number of disjoint antichains may not be unique.

## Contours

Forward Contours FC[*k*] of *class k* are defined inductively, as follows:

FC[1] consists of those elements m1 for which there exists no element m2 strictly less than m1.

FC[*k*] consists of those elements m1 for which there exists no element m2 strictly less than m1, for m1 and m2 not already members of FC[*l*] where *l* &lt; *k*.

Reverse Contours RC[*k*] of *class k* are defined similarly.

A chain including any member (i, j) of FC[*k*] corresponds to a *common prefix* of length *k* which ends at (i, j).

A chain including any member (i, j) of RC[*k*] corresponds to a *common suffix* of length *k* which begins at (i, j).

**Lemma:** An LCS of length *p* has a common prefix of length *k* and a common suffix of length *p* - *k* + 1 for every *k* &isin; [1, *p*].

## Dominant Matches

There may exist distinct members m1 and m2 of a Forward Contour such that m2 &le; m1.

Dominant Matches of a Forward Contour are those m1 for which there exists no other member m2 such that m2 &le; m1.  Dominant Matches are the *Infimum*, or Meet (&and;) of the Forward Contour, with respect to the *non-strict* product-order (&le;).

There may exist distinct members m1 and m2 of a Reverse Contour such that m2 &ge; m1.

Dominant Matches of a Reverse Contour are those m1 for which there exists no other member m2 such that m2 &ge; m1.  Dominant Matches are the *Supremum*, or  Join (&or;) of the Reverse Contour, with respect to the *non-strict* product-order (&le;).

Where multiple Dominant Matches exist within a Meet (or Join, respectively) the Dominant Matches will be incomparable to each other.

## Background

Where the number of symbols appearing in matches is small relative to the length of the input strings, reuse of the symbols increases; and the number of matches will tend towards O(*m\*n*) quadratic growth.  This occurs, for example, in the Bioinformatics application of nucleotide and protein sequencing.

The divide-and-conquer approach of [Hirschberg 1975][^3] limits the space required to O(*n*).  However, this approach requires O(*m\*n*) time even in the best case.

This quadratic time dependency may become prohibitive, given very long input strings.  Thus, heuristics are often favored over optimal Dynamic Programming solutions.

In the application of comparing file revisions, records from the input files form a large symbol space; and the number of symbols approaches the length of the LCS.  In this case the number of matches reduces to linear, O(*n*) growth.

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

[^1]: "A decomposition theorem for partially ordered sets"
by Robert P. Dilworth, published January 1950,
Annals of Mathematics [Volume 51, Number 1, *pp.* 161-166]

[^2]: "A New Practical Linear Space Algorithm for the Longest Common
Subsequence Problem" by Heiko Goeman and Michael Clausen,
published 2002, Kybernetika [Volume 38, Issue 1, *pp.* 45-66]

[^3]: "A linear space algorithm for computing maximal common subsequences"
by Daniel S. Hirschberg, published June 1975
Communications of the ACM [Volume 18, Number 6, *pp.* 341-343]

[^4]: "An Algorithm for Differential File Comparison"
by James W. Hunt and M. Douglas McIlroy, June 1976
Computing Science Technical Report, Bell Laboratories 41

[^5]: "[A Fast Algorithm for Computing Longest Common Subsequences](http:www.cs.bgu.ac.il/~dpaa111/wiki.files/HuntSzymanski.pdf)"
by James W. Hunt and Thomas G. Szymanski, published May 1977
Communications of the ACM [Volume 20, Number 5, *pp.* 350-353]

[^6]: "Simple and fast linear space computation of longest common subsequences"
by Claus Rick, received 17 March 2000, Information Processing Letters,
Elsevier Science [Volume 75, *pp.* 275–281]
