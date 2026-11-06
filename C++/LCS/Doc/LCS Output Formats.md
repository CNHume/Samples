# LCS Output Formats

After an algorithm computes the LCS, a **formatter** renders the result.  `nabla`
offers four formats, selected at run time:

| Format | Switch | What it emits |
|---|---|---|
| `Sequential` | (default) | one side after the other, tagged |
| `Parallel` | `-y` | two side-by-side columns |
| `Edit` | `-u` | a GNU unified diff (patch) |
| `Cigar` | `-G` | a SAM CIGAR alignment string |

The formats share one protocol (below); they differ only in how `Layout`
renders each interval.

## The format enum and switches

```
enum class Format { CIGAR, Edit, Parallel, Sequential };
```

`-G`, `-u`, and `-y` set the format; the default is `Sequential`.  `-y` takes
`-W` (width) and `-t` (tab size).  `-u` applies a default context (3 lines) to
whichever of `prefix`/`suffix` the user did not set with `-p`/`-s`.

## The formatter protocol

`formatters::Formatter` is the dispatch base.  A **driver** — not the formatter
— builds a `SideSpan` (one side of a delta, materialized for rendering) and
drives four calls:

```
BeginResult(label1, label2, size1, size2)   file-level header
BeginInterval(begin1, end1, begin2, end2)   interval header
Layout(counter, side1, side2, kind)         the body
EndResult()                                 footer
```

`BeginInterval` and `EndResult` default to no-ops; only `Edit` implements
`BeginInterval`.  `kind` is `EditKind::None` for the layout formats, which
ignore it; `Edit` reads it to prefix each line.

Two drivers delegate to the formatter: `RecordDriver` (line/word path) and
`TextDriver` (code-point path).  Each normalizes its side and calls the
protocol, so the formatter never re-reads the source.

## Sequential (default)

Lays out each delta's two sides one after the other, with a per-side marker
(`<<<<< n …`) written inside `Layout`.  `BeginResult`/`EndResult` are no-ops —
the minimal case.

## Parallel (`-y`)

Lays out each delta's two sides in side-by-side columns, constrained to `-W`
characters per side with `-t`-space tab stops.  This is the two-column
side-by-side view of `diff -y`.

## Edit (`-u`) — GNU unified diff

`nabla -u` emits a GNU unified diff — a patch — of the two files:

```
--- file1
+++ file2
@@ -l1,c1 +l2,c2 @@
 context line
-removed line
+added line
```

- `---`/`+++` headers carry the file paths.
- Each interval begins `@@ -l1,c1 +l2,c2 @@`; `l` is the 1-based start line, `c`
  the number of lines shown from that file.  A count of `1` is omitted; a count
  of `0` means the interval has no lines from that file and its start is the
  line just before the block.
- Body lines are prefixed `␣` (context), `-` (removed), or `+` (added).

`-u` is rejected at parse time when combined with `-y` (another format), `-c`
(correspondence — a patch is inherently differences), `-l` (length only),
`-q`/`-f`/`-T` (code-point mode, not line-oriented), `-w` (word mode), or `-j`
(edit mode does its own interval merging).  `-b` (whitespace) and `-i` (case)
remain valid — they only change which records compare equal.

Two design rules keep the edit path honest:

1. **Equality stays in the algorithm.**  The formatter never compares line
   content; under `-i`/`-b` the algorithm treats `Foo` and `foo` as equal, so
   the formatter takes the match/change classification *by index*, never by
   re-comparing text.
2. **No flattening.**  The edit path consumes the match runs and difference
   runs directly (grouping them into intervals by index arithmetic) rather than
   the flattened `Delta::Context`/`Coalesce` output, which discards the
   match/change structure.

`EditKind` carries only `Match` (both sides equal) or `Change` (sides differ);
delete, insert, and substitution are all derivable from which side of a
`Change` is empty, so no further distinction is needed.  A run is an existing
`Delta` carrying an `EditKind`; an interval is a list of tagged runs.

## Cigar (`-G`) — SAM alignment

`Cigar` emits the SAM CIGAR alignment trace of one sequence against the other —
the run-length encoding (`M`/`I`/`D` operations) that SAM/BAM alignment records
use.  Selecting `-G` turns the LCS match runs into `M` blocks and the edit runs
into `I`/`D` blocks, producing a standard CIGAR string for direct consumption
by downstream alignment tooling (`samtools`, etc.).

## Deferred

- `\ No newline at end of file` marker (record mode does not track
  trailing-newline absence).
- `---`/`+++` header timestamps.
- Section headings (the trailing `@@ function-name` of GNU diff).
