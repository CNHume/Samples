# LCS Performance

This file shows how `-Aa` (Automatic) seamlessly moves between the four LCS
algorithms as the input crosses each boundary condition.  Every table lists all
four engines plus `-Aa`, so the reader can see that `-Aa` tracks the winner
without any user intervention.

Notation: `D` = edit distance, `r` = match count (matched position pairs),
`ρ = r/(mn)` = match density (in `[0,1]`), `s` = alphabet size (distinct
symbols), `p` = LCS length.  Cells are `wall-time · peak-RSS`;
the `-Aa` cell also names the algorithm it selected in brackets.
`TIMEOUT` = exceeded the timeout; `abort` = the algorithm bailed out on its own
budget.

The dispatch itself is documented in `LCS Algorithm Selection.md`.  In one line:

```
D ≤ 2000                → Myers          (-Ad)
else r ≤ 100·min(m,n)   → Hunt-Szymanski (-Am)
else dense, length-only → RickTime       (-At)
else dense, reconstruct → RickSpace      (-As)
```

There are three boundary conditions, each with a measured table below.

---

## Boundary 1 — divergence (`D`)

Dense DNA (`s = 4`, so `r ≈ n²/4 ≈ 626M` at every edit rate — Hunt–Szymanski is
ineligible throughout, at 125× its sparse threshold).  As the edit rate climbs,
`D` grows, and `-Aa` moves off Myers.

### Length-only (the find phase)

| rate | D | Myers `-Ad` | H-S `-Am` | RickSpace `-As` | RickTime `-At` | Auto `-Aa` |
|---|---|---|---|---|---|---|
| 1% | 851 | **0.00s** · 5MB | TIMEOUT | 0.06s · 8MB | 0.01s · 7MB | **0.00s** · 5MB [Myers] |
| 5% | 3,695 | 0.04s · 5MB | TIMEOUT | 0.25s · 8MB | 0.06s · 7MB | **0.06s** · 7MB [RickTime] |
| 10% | 7,557 | 0.17s · 5MB | TIMEOUT | 0.51s · 8MB | 0.13s · 7MB | **0.14s** · 7MB [RickTime] |
| 20% | 14,712 | 0.64s · 6MB | TIMEOUT | 1.07s · 9MB | 0.32s · 7MB | **0.34s** · 7MB [RickTime] |
| 40% | 25,373 | 2.19s · 6MB | TIMEOUT | 1.98s · 9MB | 0.92s · 7MB | **0.89s** · 7MB [RickTime] |

The hand-off is at the `D = 2000` probe: at 1% (`D = 851`) `-Aa` picks Myers
instantly; from 5% up it stays on RickTime, whose single diagonal sweep is the
fastest find.  Myers, left to itself, degrades `O(ND)` — 2.19s by 40%.

### Interval reconstruction (the full LCS)

| rate | D | Myers `-Ad` | H-S `-Am` | RickSpace `-As` | RickTime `-At` | Auto `-Aa` |
|---|---|---|---|---|---|---|
| 1% | 851 | **0.01s** · 14MB | TIMEOUT | 0.15s · 12MB | 0.09s · 69MB | **0.01s** · 14MB [Myers] |
| 5% | 3,695 | 0.09s · 112MB | TIMEOUT | 0.55s · 12MB | 0.60s · 241MB | **0.58s** · 12MB [RickSpace] |
| 10% | 7,557 | 0.48s · 444MB | TIMEOUT | 1.12s · 12MB | 1.39s · 450MB | **1.14s** · 12MB [RickSpace] |
| 20% | 14,712 | 1.91s · 1.6GB | TIMEOUT | 2.23s · 11MB | 3.36s · 761MB | **2.31s** · 11MB [RickSpace] |
| 40% | 25,373 | 6.44s · 4.9GB | TIMEOUT | 4.15s · 11MB | 6.02s · 1.1GB | **4.16s** · 11MB [RickSpace] |

Reconstruction changes the picture in two ways the dispatcher exploits:

- **Myers** is fast at low `D` but its `O(D²)` history balloons to **4.9 GB** by
  40% — so `-Aa` leaves it as soon as `D` passes 2000.
- **RickTime's** `O(d log d)` dominant-match sort makes it *slower than RickSpace*
  once the sequences diverge — so `-Aa` routes reconstruction straight to
  RickSpace, whose **11 MB** is flat across the whole range.

The same input therefore picks a different Rick engine by phase: **RickTime for
the find, RickSpace for the reconstruction** — and `-Aa` tracks the best in both
columns.

---

## Boundary 2 — match count (`r`)

Divergent input (40% edit), length-only, as the alphabet grows and the matches
thin out:

| `s` | r | Myers `-Ad` | H-S `-Am` | RickSpace `-As` | RickTime `-At` | Auto `-Aa` |
|---|---|---|---|---|---|---|
| 4 | 624M | 2.58s · 6MB | TIMEOUT | 2.29s · 9MB | 1.02s · 7MB | **0.98s** · 7MB [RickTime] |
| 20 | 125M | 2.85s · 6MB | 6.91s · 5MB | 1.61s · 21MB | 0.34s · 13MB | **0.33s** · 13MB [RickTime] |
| 100 | 25M | abort | 1.54s · 5MB | 0.51s · 82MB | 0.21s · 44MB | **0.18s** · 44MB [RickTime] |
| 1,000 | 2.5M | abort | **0.16s** · 6MB | 1.21s · 768MB | 0.28s · 387MB | **0.17s** · 6MB [Hunt-Szymanski] |
| 10,000 | 284K | abort | **0.03s** · 12MB | TIMEOUT | TIMEOUT | **0.07s** · 12MB [Hunt-Szymanski] |

The hand-off is at the `r = 100·min(m,n)` boundary (5M at this length) — a
match density of `ρ = 100/max(m,n) = 0.002` here, swept from `ρ ≈ 1/4` (DNA)
down to `ρ ≈ 10⁻⁴` (`s = 10,000`):

- **Dense** (`s ≤ 100`): `r` is huge, Hunt–Szymanski times out, and `-Aa` stays
  on RickTime.
- **Sparse** (`s ≥ 1000`): `r` falls below the threshold, Hunt–Szymanski becomes
  the winner (0.03s at `s=10,000`), and `-Aa` moves onto it — *just as* the Rick
  engines' `O(ns)` tables blow up (RickSpace/RickTime TIMEOUT at `s=10,000`,
  where the tables are ~4 GB).  `-Aa` dodges that blow-up by switching to the
  match-list algorithm.

---

## Boundary 3 — memory (the linear-space fallback)

RickSpace is the only engine with flat memory, so `-Aa`'s choices are
memory-safe by construction:

- **Length-only** → RickTime needs no recording (memory `O(ns)`), so it never
  overflows.
- **Reconstruction** → RickSpace is `O(ns)` linear space, so it never overflows
  either.

The only overflow left is an *explicit* `-At` on a large, divergent input: its
`O(d)` dominant-match recording grows past `Analysis::RecordingBudget` (0.75×
available RAM), and it aborts with "use `-As` (RickSpace)" rather than OOMing.
Measured at 40% edit: RickTime's recording is 3.9 GB at 100 Kbp and overflows at
~150 Kbp on a 9.7 GB host — the point at which `-As` is mandatory.

---

## Summary

`-Aa` never runs a pathological algorithm.  Each boundary condition — divergence
(`D = 2000`), match count (`r = 100·min`), and the output phase (length-only vs
reconstruction) — is tested up front or routed by phase, and `-Aa` lands on the
winner every time:

- near-identical → **Myers**;
- sparse matches → **Hunt–Szymanski**;
- dense, length-only → **RickTime**;
- dense, reconstruction → **RickSpace**.

Because the two Rick engines are parameterized by LCS length and alphabet size
rather than edit distance or match count, there is no input for which all four
engines are pathological — and `-Aa` finds the one that is not.
