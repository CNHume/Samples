# LCS Namespace Plan

The library's flat `namespace lcs` with `LCS*`-prefixed classes hides a three-way
split.  This documents the plan to organize it into sub-namespaces.

## Taxonomy

The library has three orthogonal dimensions, plus a shared layer:

| Dimension | Classes |
|---|---|
| Algorithms | `LCSMyers`, `LCSHuntSzymanski`, `LCSRickSpace`, `LCSRickTime` |
| Modes (granularity) | `LCSText`, `LCSRecord` |
| Formatters | `LCSFormat`, `LCSSequential`, `LCSParallel`, `LCSCigar`, `LCSEdit`, `LCSFormatRecord`, `LCSFormatText` |
| Shared | `Delta`, `Pair`, `Statistics`, `Diagnostic`, `Duration`, `Stopwatch`, `Environment`, `LCSEncoding`, `LCSNormal`, `LCSOptions`, `LCSFile` |

`LCSFormatRecord` and `LCSFormatText` embody the formatter protocol — they build
`SideSpan`s from content and drive a formatter — so they belong with the
formatters rather than the modes.

## Target namespace layout

```
namespace lcs {
  enum class Algorithm { Automatic, EditDistance, Matches, RickSpace, RickTime };
  enum class Format { CIGAR, Edit, Parallel, Sequential };

  class Delta; class Pair;               // data structures
  class Statistics; class Diagnostic;    // metrics / heuristics
  class Duration; class Stopwatch;
  class Environment;
  class Encoding;                        // was LCSEncoding
  class Normal;                          // was LCSNormal
  class Options;                         // was LCSOptions
  class File;                            // was LCSFile (entry point)

  namespace algorithms {
    class Myers;                         // was LCSMyers
    class HuntSzymanski;                 // was LCSHuntSzymanski
    class RickSpace;                     // was LCSRickSpace
    class RickTime;                      // was LCSRickTime
  }

  namespace modes {                      // granularity
    class Text;                          // was LCSText
    class Record;                        // was LCSRecord
  }

  namespace formatters {
    class Formatter;                     // was LCSFormat (base)
    class Sequential;                    // was LCSSequential
    class Parallel;                      // was LCSParallel
    class Cigar;                         // was LCSCigar
    class Edit;                          // was LCSEdit
    class RecordDriver;                  // was LCSFormatRecord (name TBD)
    class TextDriver;                    // was LCSFormatText   (name TBD)
  }
}
```

## Directory layout

```
lib/
├── algorithms/
│   └── Myers, HuntSzymanski, RickSpace, RickTime
├── modes/
│   └── Text, Record
├── formatters/
│   └── Formatter, Sequential, Parallel, Cigar, Edit,
│       RecordDriver, TextDriver
└── (top level)
    └── LCS, Delta, Pair, Statistics, Diagnostic, Duration, Stopwatch,
        Environment, Encoding, Normal, Options, File, join, SuspectDash
```

## Decisions

- Namespace names are lowercase (C++ idiom); class names stay PascalCase.
- The `LCS` prefix is dropped inside the sub-namespaces, since the namespace
  supplies the context (`lcs::formatters::Edit`, not `LCSEdit`).
- The `LCS` base class dissolves: `Algorithm`/`Format` become `lcs::`-level
  enums, and the `PAIRS`/`INDEXES`/`STRING_TO_INDEXES_MAP`/`MATCHES` typedefs
  stay in `lcs::` (shared by the modes and the algorithms).
- The formatter base is `formatters::Formatter` (not `Format`), so it does not
  collide with the `lcs::Format` enum.
- `Encoding` and `Normal` stay in `lcs::` — they are content *support*, not a
  mode.  `Encoding`'s nested enum is `Kind` (was `Encoding`) and `Normal`'s
  normalize method is `Normalize` (was `Normal`), to avoid colliding with the
  class names.
- The driver names `RecordDriver`/`TextDriver` avoid colliding with
  `modes::Record`/`modes::Text`.

## Stages

1. Physically reorganize the files into the subdirectories above (header and
   source together), updating `#include` paths and the build files, with no
   namespace changes.
2. Implement the namespaces and rename the classes per the target layout.
3. Rename the files to match the classes they contain.
