# LCS Algorithm Selection

`compare` ships four LCS algorithms, each with a regime where it is fastest.
This documents those regimes and the heuristic that selects among them.
Measurements come from `test perf region` on the same host as
`LCSPerformance.md` (Intel Core i7-9700K, 8 cores, 3.60 GHz, 15 GB RAM, WSL2);
times are single runs and are approximate, but the regime boundaries are
robust.

## The three metrics

Three independent quantities decide which algorithm wins:

| Metric | Definition | Owner |
|---|---|---|
| **EditDistance** `D` | insertions + deletions in the minimal edit script (`D = M+N−2p`) | Myers |
| **Redundancy** `ρ` | average matches per position (`ρ = r/n`) | Hunt-Szymanski |
| **AlphabetSize** `s` | number of distinct symbols | RickSpace/RickTime (table build) |

`ρ` is a function of `s`, the input length `M+N`, and the symbol *distribution*:
uniform symbols give `ρ = n/s`, but a skewed (Zipf) distribution keeps `ρ` high
even at large `s`.  The three metrics are therefore genuinely independent —
uniform random strings only look two-dimensional because they couple `s` and
`ρ`.

## Measurement

```
test perf region [subs] [skew] [dissimilar]
```

- `subs` sets the near-identical edit distance `D = 2·subs` (substitution-only).
- `skew` shapes the symbol distribution (`0` = uniform, `>0` = Zipf-like).
- `dissimilar` (nonzero) generates the second string independently, giving
  `D ≈ M+N` and a short LCS.

Each cell times all four algorithms with full reconstruction; `N/A` means the
algorithm aborted on its budget (Myers' `D` budget, RickTime's recording
budget).

## Near-identical regime

Winner at `InputLength = 200000` (seconds; the winner's time in each cell):

| `s` (`ρ`) | `D=200` | `D=2000` | `D=10000` | `D=20000` |
|---|---|---|---|---|
| 4 (25000) | **Myers** 0.007 | **Myers** 0.019 | **Myers** 0.477 | **Myers** 3.31 |
| 20 (5000) | **Myers** 0.006 | **Myers** 0.014 | **Myers** 0.407 | **RickTime** 1.50 |
| 100 (1000) | **Myers** 0.009 | **Myers** 0.013 | **RickTime** 0.203 | **RickTime** 0.399 |
| 1000 (100) | **Myers** 0.006 | **Myers** 0.013 | **RickTime** 0.608 | **RickTime** 0.543 |

- **Myers** wins while `D` is small, independent of redundancy — its cost is
  the `O(D²)` reconstruction history alone.
- As `D` grows, the crossover to **RickTime** moves with redundancy: at high
  redundancy (small `s`) RickTime's own dominant-match recording is expensive,
  so Myers holds on longer.
- **Hunt-Szymanski** never wins here: at `s=1000` it is 0.93 s vs RickTime's
  0.54 s, and at `s=4` it is 224 s.
- **RickSpace** never wins: its `O(ns)` table build makes it slowest at large
  alphabet (2.78 s at `s=1000`).

## Dissimilar regime

Winner by input length and alphabet size (seconds):

| `n` | `s=4` | `s=20` | `s=100` | `s=1000` |
|---|---|---|---|---|
| 1K | **RickTime** 0.003 | **RickTime** 0.001 | **RickTime** 0.001 | **H-S** 0.000 |
| 10K | **RickTime** 0.244 | **RickTime** 0.136 | **RickTime** 0.054 | **H-S** 0.010 |
| 100K | **RickSpace** 23.1 | **RickTime** 15.9 | **RickTime** 6.47 | **H-S** 1.16 |

- **Myers** is unavailable once `D` exceeds its budget (`N/A` at `n=100K`).
- **Hunt-Szymanski** wins at low redundancy (sparse matches) — its intended
  regime.
- **RickTime** wins at high redundancy, but at `(100K, s=4)` its recording
  exceeds the budget, leaving **RickSpace** as the only algorithm that
  completes (linear memory, never OOMs).

## Selection heuristic

The decision tree a future `-ma` (automatic) selector would implement, in
order:

1. `D` small → **Myers**.
2. else if redundancy low (sparse matches) → **Hunt-Szymanski**.
3. else if RickTime's recording fits → **RickTime**.
4. else → **RickSpace**.

Each step reuses an existing cheap signal: the composition lower bound on `D`
(`LCSMyers::DistanceLowerBound`), the match count (`Diagnostic::WarnMatchCount`),
and the recording budget (`Diagnostic::RecordingBudget`).

## Status

`-a` selects the algorithm explicitly (`d|m|s|t`): `d` = EditDistance (Myers),
`m` = Matches (Hunt-Szymanski), `s` = RickSpace, `t` = RickTime.  An automatic
`-a a` heuristic dispatch is **not yet implemented**; the decision tree above
is its specification.
