# LCS Performance — RickSpace vs RickTime

A comparison of the two Rick algorithms implemented in `compare`:

- **RickSpace** (`-as`, `LCSRickSpace`) — Rick (2000) linear-space LCS:
  forward/backward contours + Hirschberg.  Time `O(ns + min{pm, p(n−p)})`,
  space `O(ns)`.
- **RickTime** (`-at`, `LCSRickTime`) — Rick (1995) diagonal sweep.  Time
  `O(ns + min{pm, p(n−p)})`, space `O(ns + d)`, where `d` is the number of
  dominant matches it records for reconstruction.

Both are run on the character (`-t`) path.  Time is wall-clock; peak RSS is the
maximum resident set size reported by `/usr/bin/time -v`.  The host has 15 GB
of RAM, so any run whose recording grows past ~14 GB is killed (shown as OOM).

## Data

| Label | Bases | Source | Alphabet | N content |
|---|---|---|---|---|
| 1M | 1,000,000 | *E. coli* K-12 MG1655 (slice) | A C G T | 0% |
| 4.6M | 4,641,653 | *E. coli* K-12 MG1655 (full) | A C G T | 0% |
| 16M | 16,000,000 | human chr21 (slice) | A C G T N | 41% |
| 46.7M | 46,709,983 | human chr21 (full) | A C G T N | 14% |

The `_100subs` / `_1000subs` / `_10000subs` mutants differ from the reference at
a fixed number of randomly chosen positions (each changed to a different
symbol).  The `_999permil` mutant applies a 0.1% substitution *rate* (one
position per thousand), so its substitution count scales with the input size.

## Results

### 1M (E. coli)

| Mutations | LCS length | RickSpace time | RickSpace RSS | RickTime time | RickTime RSS |
|---|---:|---:|---:|---:|---:|
| 100 | 999,900 | 2.07 s | 0.15 GB | 0.88 s | 0.43 GB |
| 1,000 | 999,000 | 9.62 s | 0.15 GB | 6.48 s | 2.71 GB |
| 10,000 | 990,065 | 144 s | 0.14 GB | — | OOM (13.8 GB) |

### 4.6M (E. coli)

| Mutations | LCS length | RickSpace time | RickSpace RSS | RickTime time | RickTime RSS |
|---|---:|---:|---:|---:|---:|
| 100 | 4,641,552 | 9.45 s | 0.66 GB | 4.24 s | 2.00 GB |
| 1,000 | 4,640,652 | 54.4 s | 0.66 GB | 40.1 s | 13.1 GB |
| 10,000 | 4,631,660 | 750 s | 0.66 GB | — | OOM (13.9 GB) |
| 0.1% rate (4,642) | 4,637,010 | 314 s | 0.66 GB | — | OOM (14.0 GB) |

### 16M (chr21)

| Mutations | LCS length | RickSpace time | RickSpace RSS | RickTime time | RickTime RSS |
|---|---:|---:|---:|---:|---:|
| 100 | 15,999,900 | 20.6 s | 2.51 GB | 6.09 s | 4.19 GB |
| 1,000 | 15,999,000 | 91.0 s | 2.51 GB | 25.4 s | 13.2 GB |

### 46.7M (chr21)

| Mutations | LCS length | RickSpace time | RickSpace RSS | RickTime time | RickTime RSS |
|---|---:|---:|---:|---:|---:|
| 100 | 46,709,883 | 65.4 s | 7.31 GB | 25.1 s | 13.8 GB |
| 1,000 | 46,708,983 | 318 s | 7.31 GB | — | OOM (14.0 GB) |

## Observations

- **Space is the binding constraint.**  RickSpace's RSS is constant *for a given
  size* — it never changes as the mutation count grows (0.15 GB at 1M,
  0.66 GB at 4.6M, 2.51 GB at 16M, 7.31 GB at 46.7M), scaling linearly with
  the input.  RickTime's RSS grows with the mutation count because `byRank`
  records every dominant match (`d`), so 1,000 substitutions already take
  13 GB at 46.7M and 10,000 substitutions exhaust memory even at 1M.
- **RickTime is faster while it fits.**  On the 100-substitution cases RickTime
  is roughly 2.5–3.4× faster than RickSpace (single pass versus the ×2
  Hirschberg recomputation), at the cost of ~2–3× the memory.
- **Fixed count vs. fixed rate.**  A fixed *count* keeps the running time
  `O(n·count)` and RickSpace's memory `O(n)`.  A fixed *rate* (0.1% here) makes
  the count scale with `n`, so both time and RickTime's `d` grow quadratically —
  the `_999permil` case is already out of reach for RickTime at 4.6M.
- **Where each wins.**  For very long, near-identical sequences RickTime gives
  the best wall-clock when memory allows; RickSpace is the safe default because
  its memory stays linear and it never exhausts the host, which is what makes
  46.7M tractable in 7.3 GB (versus RickTime's 13.8 GB, already near the limit).
