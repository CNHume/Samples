# LCS Algorithm Selection

`nabla` ships four LCS algorithms, each with a regime where it is fastest
(see `LCS Algorithms.md`).  The `-Aa` switch (Automatic, the default) picks
among them at run time.  This documents the dispatch.

## The three metrics

Three independent quantities decide which algorithm wins:

| Metric | Definition | Favors |
|---|---|---|
| Edit distance `D` | `m + n − 2p` | Myers (`O(ND)`) when `D` is small |
| Match count `r` | matched position pairs | Hunt–Szymanski (`O((r+n) log n)`) when `r` is sparse |
| Alphabet size `s` | distinct symbols | RickTime's dense tables (`O(ns)`) and recording budget |

`r` is a function of `s`, the lengths, and the symbol *distribution*: uniform
symbols give `r ≈ n²/s`, but a skewed distribution keeps `r` high even at large
`s`.  The three metrics are therefore genuinely independent.  The normalized
form is the match density `ρ = r/(mn)`, in the closed interval `[0,1]`: `ρ → 0`
is sparse, `ρ → Θ(1)` is dense.

## The dispatch

`-Aa` runs a cheap pre-analysis and routes the workload, in order:

1. **Myers probe** — run Myers' length-only D-path sweep capped at
   `Analysis::EditThreshold` (= 2000).  If the sweep reaches the end within the
   cap (`D ≤ 2000`), use Myers.  This is an *exact* test of the near-identical
   regime, not an estimate; the earlier composition bound
   (`m + n − 2·Σ min(count1, count2)`) was abandoned because it collapses to ~0
   on balanced alphabets like DNA and never let the dispatcher leave Myers.

2. **Sparse check** — if the match count is low (`r ≤ Analysis::SparseMatchCount
   · min(m, n)`, where `SparseMatchCount` = 100), use Hunt–Szymanski.

3. **Dense, by phase** — otherwise the matches are dense and the Rick engines
   apply.  The choice depends on whether reconstruction is requested:

   - **Length-only** (`-l`): RickTime — its single diagonal sweep is the
     fastest find.  The dispatch runs it speculatively and falls back to
     RickSpace if its recording exceeds `Analysis::RecordingBudget`.
   - **Interval reconstruction**: RickSpace — RickTime's `O(d log d)`
     dominant-match sort is more expensive than RickSpace's divide-and-conquer
     once the sequences diverge, so reconstruction goes straight to linear
     space.

The Myers and Hunt–Szymanski branches are decided up front from the metrics;
the RickTime/RickSpace branch is the only part that depends on the output phase.

## Thresholds

`lib/algorithms/Analysis.h` names the knobs:

```
EditThreshold   = 2000   // Myers' fast range (the probe cap)
SparseMatchCount = 100  // r / min(m,n) below which Hunt-Szymanski is sparse
RecordingBudget(m, n, s) // RickTime's dominant-match budget, RAM-derived
```

`RecordingBudget` is `(0.75 · available RAM − 4·s·(m+n+4) − rank headers) / 16`
— the number of dominant matches whose `byRank` recording fits within a safety
margin of available memory.  This is the alphabet-size term: on a small alphabet
the dense tables are cheap and the budget is wide, so RickTime fits easily.

## Status

`-A` selects the algorithm (`-Aa` Automatic, `-Ad` Myers, `-Am` Hunt–Szymanski,
`-As` RickSpace, `-At` RickTime).  `-Aa` implements the tree above; `-v`
reports the choice and its reason.  An explicit `-At` that overflows its
recording budget aborts with a diagnostic pointing at `-As`; under `-Aa` the
same overflow is caught and falls back to RickSpace automatically.

For the measured crossover points and the two output phases, see
`LCS Performance.md`.
