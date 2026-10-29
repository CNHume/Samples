# LCSEdit — GNU unified diff output

`compare -u` emits a GNU unified diff — a patch — of the two files.  It is the
`Format::Edit` output format, rendered by the `LCSEdit` formatter (an
`LCSFormat` subclass).  Context is taken from the existing `-p`/`-s` switches,
defaulting to 3.

## Switch

`-u` is a flag (no argument).  It sets `format = Format::Edit` and applies a
default context to whichever of `prefix`/`suffix` the user did not set:

```
constexpr uint32_t edit_context_affix = 3;
```

- `-u` → prefix = suffix = 3
- `-u -p 5` → prefix = 5, suffix = 3
- `-u -s 2` → prefix = 3, suffix = 2
- `-u -p 5 -s 2` → prefix = 5, suffix = 2

The default is applied after parsing, so it is independent of switch order.

Conflicts, rejected at parse time:

- `-y` — another output format.
- `-c` — correspondence; a patch is inherently differences.
- `-l` — length only; no patch.
- `-q`, `-f`, `-T` — each implies `istext` (code-point mode, not line-oriented).
- `-w` — word mode, not line-oriented.
- `-j` — edit mode does its own interval merging.

`-b` (whitespace) and `-i` (case) remain valid; they only change which records
compare equal.

## Output format

GNU unified diff:

```
--- file1
+++ file2
@@ -l1,c1 +l2,c2 @@
 context line
-removed line
+added line
 context line
```

- `---`/`+++` headers carry the file paths; timestamps are deferred.
- Each interval begins `@@ -l1,c1 +l2,c2 @@`; `l` is the 1-based start line, `c`
  the number of lines shown from that file (context plus changes).  A count of
  `1` is omitted; a count of `0` means the interval has no lines from that file
  and its start is the line just before the block (`0` at the beginning of the
  file).
- Body lines are prefixed `␣` (context, present in both), `-` (removed, old
  only), or `+` (added, new only).
- `\ No newline at end of file` is deferred (see below).

## Design decisions

### Equality stays in the algorithm

The formatter never compares line content.  Under `-i` (or `-b`, `-f`), the
algorithm treats `Foo` and `foo` as equal, while a raw content comparison would
not; the formatter must therefore take the match/change classification from the
algorithm, by index, never by re-comparing text.

### No flattening

The existing `Delta::Context()` / `Delta::Coalesce()` machinery merges context
into the difference deltas, discarding the match/change structure and forcing
the formatter to re-derive equality.  The edit path therefore does not use it.
Instead it consumes the match runs (`Compare()` output) and the difference runs
(`Delta::Complement()` output) directly and groups them into intervals by index
arithmetic.

### A unified Layout

`Format::Edit` is selected by a `format == Edit` branch only for *delta
production* (tagged runs rather than flattened differences).  The formatter
contract is shared: `LCSEdit` is an `LCSFormat` subclass whose `Layout` reads
`EditKind`, while the layout formatters ignore it.

### Two kinds suffice

`EditKind` carries only what the interval's shape cannot: whether the two sides
are **equal** (`Match`) or **differ** (`Change`).  Delete, insert, and
substitution are all derivable from which side of a `Change` is empty, so the
formatter needs no further distinction.

### No new data types

The edit path adds no structs.  A run is an existing `Delta` carrying an
`EditKind`; its text is an existing `SideSpan`.  An interval is just a group of
runs, described by the existing semi-open `begin`/`end` intervals (counts
derived where needed).

## Naming

`Edit` (Format::Edit), `EditKind`, `LCSEdit`, `edit_context_affix`.

Future: append the morpheme `Format` to the formatter class names
(`LCSCigarFormat`, `LCSEditFormat`, `LCSParallelFormat`, `LCSSequentialFormat`).

## Interface

### Format enum

```
enum class Format {
  CIGAR,        // CIGAR alignment string
  Edit,         // GNU unified diff patch (-u)
  Parallel,     // two columns side-by-side
  Sequential,   // one side after the other
};
```

### EditKind (Delta.h, namespace scope)

```
enum class EditKind { None, Match, Change };
```

`Delta` gains `EditKind kind = EditKind::None;`.  `Match` marks an LCS match
run (both sides equal); `Change` marks a difference run (the sides differ —
delete, insert, or substitution).  `LCSFormat.h` includes `Delta.h` to use
`EditKind`.

### LCSFormat (the formatter protocol)

```
virtual void BeginResult(const std::string&, const std::string&,
                         std::uint32_t, std::uint32_t) {}
virtual void BeginInterval(std::uint32_t begin1, std::uint32_t end1,
                           std::uint32_t begin2, std::uint32_t end2) {}
virtual void Layout(std::uint32_t counter, const SideSpan& side1, const SideSpan& side2,
                    EditKind kind = EditKind::None) = 0;
virtual void EndResult() {}
```

One `Layout` for every formatter; `kind` defaults to `None`, which the layout
formatters ignore.  `BeginInterval` is part of the protocol for all formats,
but only `LCSEdit` implements it (the rest leave the default no-op).  An
`EndInterval` is a likely future addition; it is not needed now.

### Where the calls are made

The driver — not the formatter — makes these calls.  `LCSFile::Difference`
branches on `format == Edit` and hands the `SideSpan` construction and the
method calls to a driver; the formatter only implements the methods:

```
LCSFormat — one file-level result
┌───────────────────────────────────────────────────────────────┐
│ BeginResult(label1, label2, …)  file header                   │
│                                 └ Edit:  "--- f1" / "+++ f2"  │
│                                                               │
│ ┌─ interval (Edit only) ────────────────────────────────────┐ │
│ │ BeginInterval(b1,e1,b2,e2)    interval header             │ │
│ │                               └ Edit:  "@@ -l1,c1 +l2,c2 @@"│
│ │ Layout(counter,s1,s2,kind)    body                        │ │
│ │                               └ "␣ ctx" / "- del" / "+ ins"│
│ └───────────────────────────────────────────────────────────┘ │
│                                                               │
│ EndResult()                     footer (usually nothing)       │
└───────────────────────────────────────────────────────────────┘
```

`Sequential` is the minimal case: `BeginResult`/`EndResult` are no-ops and only
`Layout` runs — no headers at all.  (Its per-side marker `<<<<< n …` is written
by a private `Head` helper *inside* `Layout`, not by a protocol method.)

### LCSEdit

```
class LCSEdit : public LCSFormat {
public:
  void BeginResult(const std::string&, const std::string&,
                   std::uint32_t, std::uint32_t) override;
  void BeginInterval(std::uint32_t begin1, std::uint32_t end1,
                     std::uint32_t begin2, std::uint32_t end2) override;
  void Layout(std::uint32_t, const SideSpan& side1, const SideSpan& side2,
              EditKind kind) override;
  void EndResult() override;
};
```

`BeginResult` emits `---`/`+++`; `BeginInterval` derives `l = begin + 1`,
`c = end − begin` (with the count-0 convention) and emits `@@ -l1,c1 +l2,c2 @@`;
`Layout` emits `side1`/`side2` prefixed by `kind`.

## Interval construction (index arithmetic)

Given the match runs and difference runs (by index) and the context values
(`prefix`/`suffix`):

1. Walk the runs in order.  An interval is a difference run plus up to `prefix`
   matching records before and `suffix` matching records after.
2. Merge a consecutive difference into the current interval when the matching
   records between them number ≤ `prefix + suffix` (their contexts touch).
3. Tag each delta by provenance: match run → `Match`, difference run →
   `Change`.
4. An interval is a list of tagged runs; the intervals form a list of lists
   (`std::vector<std::vector<std::shared_ptr<Delta>>>`).

No line content is compared anywhere in this step.

## Rendering

- `BeginResult` → emit `--- label1` and `+++ label2`.
- `BeginInterval(begin1, end1, begin2, end2)` → emit `@@ -l1,c1 +l2,c2 @@`.
- `Layout(kind, side1, side2)` → `Match` emits `␣` side1; `Change` emits `-`
  side1 (if non-empty) then `+` side2 (if non-empty).
- `EndResult` → no-op.

The driver builds a `SideSpan` for each side with the existing `MakeSpan`, then
drives `BeginResult`/`BeginInterval`/`Layout`/`EndResult`, passing `delta->kind`.

## Deferred

- `\ No newline at end of file` marker (record mode does not track
  trailing-newline absence).
- Header timestamps.
- Section headings (the trailing `@@ function-name`).

## Hooks

- `LCSOptions`: `format` carries the mode (no new boolean).
- `Command.cpp`: parse `-u` (flag), set `format`; apply `edit_context_affix`
  to `prefix`/`suffix` unless `-p`/`-s` set them; reject the conflicts above.
- `LCSFile::Difference`: branch on `format == Edit` and drive the edit driver.
- `LCSEdit` (an `LCSFormat` subclass) and the interval builder (index arithmetic).
- `man/compare.1`: document `-u` and the conflicts.
