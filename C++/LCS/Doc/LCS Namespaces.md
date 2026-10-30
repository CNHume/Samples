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
    class Format;                        // was LCSFormat (base)
    class Sequential;                    // was LCSSequential
    class Parallel;                      // was LCSParallel
    class Cigar;                         // was LCSCigar
    class Edit;                          // was LCSEdit
    class RecordDriver;                  // was LCSFormatRecord (name TBD)
    class TextDriver;                    // was LCSFormatText   (name TBD)
  }
}
```

## Directory layout (stage 1)

```
lib/
├── algorithms/
│   └── LCSMyers, LCSHuntSzymanski, LCSRickSpace, LCSRickTime
├── modes/
│   └── LCSText, LCSRecord
├── formatters/
│   └── LCSFormat, LCSSequential, LCSParallel, LCSCigar, LCSEdit,
│       LCSFormatRecord, LCSFormatText
└── (top level)
    └── LCS, Delta, Pair, Statistics, Diagnostic, Duration, Stopwatch,
        Environment, LCSEncoding, LCSNormal, LCSOptions, LCSFile, join,
        LCSSuspectDash
```

## Decisions

- Namespace names are lowercase (C++ idiom); class names stay PascalCase.
- The `LCS` prefix is dropped inside the sub-namespaces, since the namespace
  supplies the context (`lcs::formatters::Edit`, not `LCSEdit`).
- The `LCS` base class dissolves: `Algorithm`/`Format` move to `lcs::` as
  namespace-level enums; the `PAIRS`/`INDEXES`/`MATCHES` typedefs move to
  `lcs::algorithms` where they are used.
- `Encoding` and `Normal` stay in `lcs::` — they are content *support*, not a
  mode.
- The driver names `RecordDriver`/`TextDriver` avoid colliding with
  `modes::Record`/`modes::Text`; the exact names are still open.

## Stages

1. Physically reorganize the files into the subdirectories above (header and
   source together), updating `#include` paths and the build files, with no
   namespace changes.
2. Implement the namespaces and rename the classes per the target layout above.
