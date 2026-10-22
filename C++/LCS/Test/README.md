# LCS Test Application

A single `test` binary that links the LCS library sources and unifies
the LCS project's testing into one application.  It dispatches on its first
argument:

| Command | Purpose | Notes |
|---|---|---|
| `make test` | **regression** (default) | fast; the CI gate |
| `./build/test stress [N] [seed]` | randomized stress | default 200,000 trials |
| `./build/test perf rick <space\|time> <file1> <file2>` | RickSpace/RickTime timing | reports `p` and `time_ms` |
| `./build/test perf measure [N] [K]` | RickSpace contour work | near-diagonal input |
| `./build/test perf filterpairs [n] [s]` | FILTER_PAIRS benefit | requires `s \| n` |

## Regression

doctest (vendored under `doctest/`, v2.5.3) runs the correctness suite:

- **Reconstruction oracle** (`test_lcs.cpp`) — every algorithm's reported LCS
  length must match an independent O(m·n) DP length for small inputs, and the
  reconstructed match trace must be a genuine LCS (ordered runs, equal symbols,
  lengths summing to the reported length).  This is the check that caught the
  Pair-destructor reconstruction bug.
- **Encoding round-trip** (`test_encoding.cpp`) — the `TankNew`/`TankOld`
  fixtures decode identically across UTF-8/16/32.

Run a subset with standard doctest filters, e.g.
`./build/test --test-case="reconstruction*"`.

## Stress

`stress` absorbs `scratch/fuzz.cpp`: many small, mostly low-alphabet random
inputs, cross-checked against the DP reference across all three algorithms.
Build under AddressSanitizer for memory checking:

```
make BUILD=build-asan CXXFLAGS="-std=c++20 -O1 -g -fsanitize=address,undefined -fno-omit-frame-pointer"
./build-asan/test stress 50000
```

## Perf

`perf` absorbs `scratch/bench.cpp` (`rick`), `scratch/measure.cpp` (`measure`),
and `scratch/hsbench.cpp` (`filterpairs`).  These are informational timing and
counter reports, not pass/fail assertions; they run the same measurements that
populate `Doc/LCSPerformance.md`.
