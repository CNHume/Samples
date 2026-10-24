# LCS Library Design

How the reusable C++ library in `lib/` was factored out of the `compare` and
`delta` command-line tools.

## Goal

Separate the LCS algorithms and the file/record/string processing into a common
library (`lib/`), leaving each tool with only its command-line parsing and a thin
dispatch.  The library does not depend on any tool's switch set.

## Namespace and naming

All library types live in `namespace lcs` (no author/project moniker).

Class names keep their `LCS` prefix.  The namespace provides scoping; the prefix
keeps the names self-describing; and library users can `using namespace lcs;` to
write `LCSRecord`, `LCSText`, ... without the `lcs::` qualifier, so the
`lcs::LCS…` redundancy appears only when fully qualifying.

Two renames were applied:

- `LCSString` → `LCSText` — it is the text-file comparison engine, not a string
  type; `Text` matches the `istext` dispatch flag and avoids confusion with
  `std::string`.  `LCSFormatString` → `LCSFormatText` likewise.
- `LCS` was **not** renamed.  It remains the base class holding the `Algorithm`
  enum and the shared typedefs (`PAIRS`/`INDEXES`/`STRING_TO_INDEXES_MAP`/
  `MATCHES`).

## Agreed design

### LCSOptions

A plain data struct carrying the comparison-relevant command-line parameters.
`LCSFile` takes `const LCSOptions&`, so its interface is switch-agnostic.

Fields, in `Command`'s original relative order, with in-class defaults:

```
algorithm    LCS::Algorithm = LCS::Algorithm::HuntSzymanski
file1        std::string
file2        std::string
ignorecase   bool = false
ignorespace  bool = false
isfasta      bool = false
isjoin       bool = false
isquote      bool = false
istext       bool = false
isverbose    bool = false
isword       bool = false
join         uint32_t = 0
prefix       uint32_t = 0
suffix       uint32_t = 0
```

Excluded (CLI concerns, handled in `main.cpp`):

- `isscorrespondence` — selects Correspondence vs. Difference.
- `islength` — expressed as `intervals == nullptr` (the null-pointer protocol).
- `isenvironment`, `ishelp`, `isversion` — handled in `Command::Parse`/`main.cpp`.

### Command : LCSOptions

`Command` derives from `LCSOptions` and declares only the CLI-only switches plus
`Parse()`:

```
class Command : public lcs::LCSOptions {
public:
  bool isscorrespondence = false;   // compare only
  bool isenvironment = false;
  bool ishelp = false;
  bool islength = false;
  bool isversion = false;

  void Parse(int argc, char* argv[]);
  // protected helpers: arg_msg, parse_uint32
};
```

`compare`'s `Command` parses the full switch set; `delta`'s parses its subset
(no `-a`, `-c`, `-f`, `-q`, `-t`), leaving the inherited `algorithm`, `istext`,
`isfasta`, and `isquote` at their defaults — which is how delta stays
Hunt-Szymanski and record-only.

### LCSFile interface

```
static uint32_t Correspondence(std::shared_ptr<Delta>* intervals,
                               Statistics* statistics,
                               const LCSOptions& options);
static uint32_t Difference(std::shared_ptr<Delta>* intervals,
                           Statistics* statistics,
                           const LCSOptions& options);
```

`LCSFile` unpacks `options` and dispatches to `LCSText` (text) or `LCSRecord`
(record).  Those keep their explicit-parameter signatures, with the `algorithm`
parameter unified everywhere (delta passes `HuntSzymanski` by default).

## Header hygiene

- No `using namespace std;` in headers; std types are qualified (`std::string`,
  `std::uint32_t`, `std::shared_ptr`, ...).
- `using namespace std;` and `using namespace lcs;` are permitted in `.cpp`
  files, for brevity.  The one free function (`Pair`'s `operator<<` friend) is
  defined inside `namespace lcs`, since the member-function `using` idiom does
  not cover free functions.
- The `FILTER_PAIRS` (and commented `SHOW_*`) build toggles stay in the `LCS`
  header, above the namespace.

## Not doing

- `[[nodiscard]]` on the length-returning methods.  Some callers care only about
  the intervals, so discarding the length is legitimate.

## ABI

Source-library model for now (consumers compile from source).  If prebuilt
binaries are shipped later:

- Treat `LCSOptions`'s field set as versioned (or freeze its layout); adding or
  reordering fields is an ABI break.
- Export symbols explicitly (hidden-by-default with an export macro).
- Consider PIMPL only for classes whose internals must stay hidden; `LCSOptions`
  is pure data and does not need it.
- A binary is tied to a compiler/standard-library family (MSVC vs. GCC/Clang;
  libstdc++ vs. libc++), so binaries are published per toolchain.

## Library vs. tool boundary

**Library (`lib/`)** — moved from `compare/` and wrapped in `namespace lcs`:

- `Pair`, `Delta`
- `LCS` (typedefs + `Algorithm`), `LCSHuntSzymanski`, `LCSRickSpace`,
  `LCSRickTime`
- `LCSRecord`, `LCSText`, `LCSEncoding`, `LCSNormal`
- `LCSFile`, `LCSOptions`
- `LCSFormatRecord`, `LCSFormatText`, `Statistics`
- `Duration`, `Stopwatch`, `Diagnostic`, `Environment`, `join`

`delta`'s `LCS.cpp`/`LCS.h` were absorbed by the library's
`LCSHuntSzymanski`/`LCS` (they had already been aligned to be textually
identical apart from the class name).

**Tools (`compare/`, `delta/`)** — remain tool-specific:

- `main.cpp` — maps `Command` → `LCSOptions`, picks `Correspondence` vs.
  `Difference`, handles `-e`, `-h`, `-V`, and the null-interval length-only case.
- `Command` — derives from `LCSOptions`; parses the tool's switch set.
- `Version`

## Build

Each tool's `Makefile` compiles its own `*.cpp` plus `../lib/*.cpp` (the source
library), with `-I../lib`.  `Test/Makefile` links `../lib/*.cpp` with the
doctest cases.  The Visual Studio `.vcxproj`/`.filters` reference the same
sources as `..\lib\`.
