# LCS Library

How the reusable C++ library in `lib/` is organized, and how it relates to the
`nabla` command-line tool.

## Goal

Separate the LCS algorithms and the file/record/string processing into a common
library (`lib/`), leaving the tool with only its command-line parsing and a thin
dispatch.  The library does not depend on any tool's switch set.

## Namespace layout

All library types live under `namespace lcs`, split into sub-namespaces by
concern (one `lib/` subdirectory per sub-namespace):

```
namespace lcs {
  enum class Algorithm { Automatic, EditDistance, Matches, RickSpace, RickTime };
  class Pair; class Delta;                     // data structures
  template <typename T> std::string join(...)  // helper
  // typedefs: PAIRS, INDEXES, STRING_TO_INDEXES_MAP, MATCHES

  namespace files {        // lib/files/
    class File;                                 // entry point
    class Encoding; class Normal;               // content support
    class Options;                               // comparison parameters
    class Host;                                  // host-machine report
  }

  namespace algorithms {   // lib/algorithms/
    class Analysis;                              // dispatch + warnings
    class Myers; class HuntSzymanski;
    class RickSpace; class RickTime;
  }

  namespace modes {        // lib/modes/  (granularity)
    class Text;            // code points
    class Record;          // lines / words
  }

  namespace formatters {   // lib/formatters/
    enum class Format { CIGAR, Edit, Parallel, Sequential };
    class Formatter;                             // base
    class Sequential; class Parallel;
    class Edit; class Cigar;
    class RecordDriver; class TextDriver;        // driver glue
  }

  namespace metrics {      // lib/metrics/
    class Statistics; class Progress; class ProgressBar;
    class Duration; class Stopwatch;
  }
}
```

The tool (`nabla/`) sits outside the library, in the global namespace:
`Command` (derives from `lcs::files::Options`), `Version`, and the `SuspectDash`
helper.

## Naming decisions

- Namespace names are lowercase (C++ idiom); class names stay PascalCase.
- The `LCS` prefix is dropped inside the sub-namespaces, since the namespace
  supplies the context (`lcs::formatters::Edit`, not `LCSEdit`).  `Algorithm`
  and `Format` are `lcs::`-level enums; the `PAIRS`/`INDEXES`/
  `STRING_TO_INDEXES_MAP`/`MATCHES` typedefs stay in `lcs::` (shared by the
  modes and the algorithms).
- The formatter base is `formatters::Formatter` (not `Format`), so it does not
  collide with the `lcs::formatters::Format` enum.
- `Encoding` and `Normal` are content *support*, not a mode.  `Encoding`'s
  nested enum is `Kind`; `Normal`'s method is `Normalize`, so neither collides
  with the class names.
- `RecordDriver`/`TextDriver` avoid colliding with `modes::Record`/`modes::Text`.
- `Analysis` is the algorithm advisor (was `Diagnostic`): it selects among the
  algorithms and warns on pathological cases.

## Entry point

`files::File` exposes the two top-level operations, taking the comparison
parameters as a plain `files::Options` struct:

```
static uint32_t Correspondence(std::shared_ptr<Delta>* intervals,
                               metrics::Statistics* statistics,
                               metrics::Progress* progress,
                               const Options& options);
static uint32_t Difference(std::shared_ptr<Delta>* intervals,
                           metrics::Statistics* statistics,
                           metrics::Progress* progress,
                           const Options& options);
```

`File` unpacks `options` and dispatches to `modes::Text` (text) or
`modes::Record` (record).  A null `intervals` pointer is the length-only
protocol: the algorithm computes the LCS length but skips reconstruction.

`Options` carries only the comparison-relevant parameters (the two files, the
algorithm, the format, and the comparison flags); tool-specific switches are
declared in `Command`, which derives from it.

## Header hygiene

- No `using namespace std;` in headers; std types are qualified.
- `using namespace std;` and the relevant `using namespace lcs::…;` are
  permitted in `.cpp` files, for brevity.
- The `FILTER_PAIRS` (and commented `SHOW_*`) build toggles stay in `lcs/LCS.h`,
  above the namespace.

## ABI

Source-library model for now (consumers compile from source).  If prebuilt
binaries are shipped later:

- Treat `Options`'s field set as versioned (or freeze its layout); adding or
  reordering fields is an ABI break.
- Export symbols explicitly (hidden-by-default with an export macro).
- A binary is tied to a compiler/standard-library family, so binaries are
  published per toolchain.

## Build

Each tool's `Makefile` compiles its own `*.cpp` plus `../lib/*.cpp` and
`../lib/*/*.cpp` (the sub-namespace source directories), with `-I../lib`.
`Test/Makefile` links the same library sources with the doctest cases.  The
Visual Studio `.vcxproj`/`.filters` reference the same sources as `..\lib\…`,
grouped by the namespace filters (`algorithms`, `files`, `formatters`, `metrics`,
`modes`, `lcs`).
