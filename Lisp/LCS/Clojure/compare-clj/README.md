# Compare — a ZetaLisp → Clojure refactor

This is an idiomatic Clojure port of the **Symbolics "Compare" facility** (a
Sequence Comparison Utility written in ZetaLisp / Lisp Machine Lisp). The
original sources live in `../Zetalisp/Compare` (`.lisp` files).

The facility computes the **Longest Common Subsequence** of two sequences
using the Hunt–Szymanski algorithm, and layers four interfaces over it (file,
stream, sequence, and position-interval), plus five human-readable output
formatters (`:delta`, `:merge`, `:parallel`, `:review`, `:series`), a delta
applicator, and a self-test harness.

## Run the tests

```bash
cd compare-clj
clojure -M:test
```

The suite mirrors `compare-test.lisp`: `compare.test/diagnose-compare` returns
`nil` iff every case in the original test file passes.

## Layout

| ZetaLisp source                  | Clojure namespace          | Notes |
|----------------------------------|----------------------------|-------|
| `Compare/compare-body.lisp`      | `compare.body`             | Hunt–Szymanski LCS, `list-matches`, `binary-position` |
| `Compare/compare-interval.lisp`  | `compare.interval`         | interval operators (`subseq`, `fasten`, `affix`, `complement`, …) |
| `Compare/compare-scan.lisp`      | `compare.scan`             | the windowed "scan" algorithm |
| `Compare/compare-face.lisp`      | `compare.face`             | public interfaces + `wrap-interfaces` wrappers |
| `Compare/compare-io.lisp`        | `compare.io`               | hashing, record read/write, source descriptors |
| `Compare/compare-format.lisp`    | `compare.format`           | tab expansion, centering, fill strings, formatter dispatch |
| `Compare/compare-delta.lisp`     | `compare.delta`            | delta formatter |
| `Compare/compare-merge.lisp`     | `compare.merge`            | merge formatter |
| `Compare/compare-parallel.lisp`  | `compare.parallel`         | parallel formatter |
| `Compare/compare-review.lisp`    | `compare.review`           | review formatter |
| `Compare/compare-series.lisp`    | `compare.series`           | series formatter |
| `Compare/compare-edit.lisp`      | `compare.edit`             | delta applicator (`edit-file`/`edit-stream`) |
| `Compare/compare-test.lisp`      | `compare.test`             | `diagnose-compare` + permutation helpers |
| `Compare/lab/compare-dynamic.lisp` | `compare.lab.dynamic`    | O(mn) dynamic-programming LCS |
| `Compare/lab/compare-space.lisp`   | `compare.lab.space`      | Hirschberg linear-space LCS |
| `Compare/lab/compare-hybrid.lisp`  | `compare.lab.hybrid`     | hybrid threshold `common-length` |
| `Compare/lab/compare-form.lisp`    | `compare.lab.form`       | form tokenizer |
| `Compare/lab/exerciser.lisp`       | `compare.lab.exerciser`  | performance exerciser / symmetry tester |
| (shared specials)                | `compare.core`             | dynamic vars + record/sequence primitives |

## Deliberately skipped

These files are Lisp-Machine-only integration and have no portable meaning, so
they were not ported:

* `Compare/System/cp-patch.lisp` — SCL presentation types, Zwei `defcom`
  commands, and Flavors `send` (`.new-version`, `.translate-wild-pathname`).
* `Compare/System/srccom-patch.lisp` — `cl:defstruct` for SRCCOM file objects,
  Zwei buffers/editor pointers, presentations.
* `Compare/System/compare.lisp` — Genera `defpackage`/`defsystem`/`defsubsystem`.
* `Compare/System/compare.system`, `Compare/System/compare.translations` —
  logical-pathname host setup (`fs:`, `sct:`).
* `Compare/load/compare-compile.lisp`, `Compare/load/compare-load.lisp` —
  Allegro CL build/load iteration scripts (they walk `Mesa:Allegro:Compare:`
  and `*.fasl`); not part of the portable core, and not needed by the Clojure
  port.
* `Compare/Text/*` (usage/history/algorithm notes) and `Compare/Temp/*`
  (sample XML data) — documentation/data.

The `#+genera` / `#+symbolics` reader-conditional bodies in the ported files
(Zwei buffer pointers, `tv:noting-progress`, presentation output, `scl:`
functions) are likewise dropped in favour of the portable `#-symbolics` paths.

## ZetaLisp → Clojure mapping

* **Multiple values** (`values` / `multiple-value-bind`) → plain maps with
  descriptive keys (`:pairs`, `:length-pair`, `:intervals`, `:lcs-length`,
  `:pair-count`, `:work-count`, `:work-done`, `:hash`/`:version`, …). Clojure
  has no multiple-value protocol, and maps destructure cleanly.

* **Special variables** (`defparameter`, `defvar`, `(declare (special …))`) →
  `^:dynamic` Vars collected in `compare.core`. A function-valued hook such as
  `*compare-normalizer*` defaults to the fully-qualified symbol of its
  implementation and is resolved lazily by `compare.core/invoke`; this both
  breaks the load-order cycles that a one-package CL program never had, and
  still lets callers `binding`-override a hook with a plain function.

* **`&rest keys &key … &allow-other-keys`** → variadic `& {:keys […] :as opts}`.
  Internal functions pass the options map through directly.

* **Destructive list splicing** (`push`/`nreverse`, `nsplice`) → persistent
  vectors/lists built with `conj`, `into`, `concat`. The Hunt–Szymanski
  threshold/log arrays are Clojure vectors (`assoc`/`nth`).

* **`format` directive strings** (`~D`, `~@{…~}`, `~@[…]`, …) → direct `str` /
  `format` helpers (`compare.format/label-string`, `interval-to-string`).
  Clojure's `cl-format` was not used because the original relied on a lenient
  SCL `format` that accepted strings in `~D`.

* **CL `subseq`** → `compare.core/subseq`, which returns the same collection
  type as its input (strings stay strings, vectors stay vectors).

* **`coerce`/`concatenate`/`intern`/`pathname` restoration** →
  `compare.core/coerce-record`, `empty-of-type`, `concat-of-type`.

* **Characters** (`#\Tab`, `#\Return`, `#\Rubout`, …) → Clojure `\tab`,
  `\return`, `\u007f`, etc.; `char-code` → `int`.

* **`defmacro`** (`wrap-interfaces`, `wrap-pure-string`, `delta-file`,
  `delta-edit`, `noting-progress`) → ordinary `defn`s and one trivial
  `defmacro` (`noting-progress`). The correspondence/difference wrappers and
  pure-string makers are generated directly as functions.

* **`block`/`return-from`** (in `scan-common-pairs`) → `loop`/`recur` with an
  explicit terminal result branch.

## Notes and limitations

* The `diagnose-compare` **improbable test** is probabilistic in the original
  (a random short sequence vs. a much longer random sequence).  The port makes
  it deterministic: `short` is a fixed distinct-element sequence, and `long` is
  built by `compare.test/supersequence` so that it *provably* embeds `short`,
  padded from a disjoint alphabet.  The LCS is therefore exactly `short` by
  construction, with no reliance on the RNG.

* Records are strings, or `[:truncated "line"]` for a final line with no
  trailing newline (the analogue of CL's `(TRUNCATED . line)`).

* Clojure has no `nil`-is-empty-list pun, so a missing interval is `nil`, never
  `[]`, and `nonep` is `empty?`.

* The delta applicator (`compare.edit`) reimplements the destructive `nsplice`
  as a functional vector splice (edits are ascending and non-overlapping, so
  each edit replaces the original `[start, end)` range).
