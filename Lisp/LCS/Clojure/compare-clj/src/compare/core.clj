(ns compare.core
  "Shared primitives and dynamic (\"special\") variables for the Compare port.

  In ZetaLisp/Common Lisp the Compare facility lives in a single package
  `COMPARE`, so every `defparameter`/`defvar` is one global special variable
  visible everywhere, and `(declare (special ...))` merely re-states that a
  function reads the global binding.  A function-valued hook is referenced by
  name and invoked via `(apply *compare-normalizer* ...)`.

  Clojure dynamic Vars are per-namespace, and namespace references form a
  strict DAG (no cycles).  We therefore:

    * collect the shared specials here in a leaf namespace that depends on
      nothing else in the library;
    * keep the function-valued hooks as fully-qualified symbols and resolve
      them lazily with [[invoke]], which both avoids load-order cycles and
      still lets a caller `binding`-override a hook with a plain function.

  A \"record\" is either a String (a line) or a vector `[:truncated \"line\"]`
  marking a final line that had no trailing newline (cf. `compare.io/read-records`)."
  (:refer-clojure :exclude [subseq]))

;; ---------------------------------------------------------------------------
;; Shared dynamic (special) variables
;;
;; The function-valued hooks default to the fully-qualified name of their
;; implementation; see `invoke`.
;; ---------------------------------------------------------------------------

;; compare-body.lisp
(def ^:dynamic *compare-normalizer*
  "The Standard Sequence Element Normalizer"
  'compare.body/normalize-element)

(def ^:dynamic *space-characters*
  "The characters which may be ignored as space"
  [\space \tab])

(def ^:dynamic *redundancy-ratio*
  "The Redundancy Heuristic Overhead Threshold"
  6)

(def ^:dynamic *default-compare-method*
  "The Default Compare Method (declared in compare-face.lisp)"
  :any)

;; compare-face.lisp
(def ^:dynamic *record-reader*
  "The Standard Record Reader"
  'compare.io/read-records)

;; compare-io.lisp
(def ^:dynamic *default-width*
  "The Default (Compare Formatter) Width"
  80)

(def ^:dynamic *default-hash-version*
  "The Default Hash Code Version"
  0)

;; compare-format.lisp
(def ^:dynamic *compare-formatter*
  "The Standard Compare Formatter"
  'compare.format/compare-formatter)

(def ^:dynamic *default-compare-format*
  "The Default Compare Format"
  :parallel)

(def ^:dynamic *compare-framer*
  "The Standard Record Framer"
  'compare.format/frame-record)

(def ^:dynamic *default-tab-spaces*
  "The Default (Relative) Tab Stops -- in CL '#0=(8 . #0#), an infinite cycle of 8s."
  (repeat 8))

;; These two mirror the CL directive strings but are retained only as
;; documentation; the formatting itself is implemented directly (idiomatic
;; Clojure) in compare.format, since the CL strings were only ever interpreted
;; by a lenient SCL `format`.
(def ^:dynamic *label-format*
  "The Standard Label Format Directive String (informational)"
  " ~@{~@[~D ~]~}")

(def ^:dynamic *interval-format*
  "The Standard Interval Format Directive String (informational)"
  "[~D~@[:~D~]]")

;; compare-delta.lisp
(def ^:dynamic *delta-formatter*
  "The Formatter used to Pretty Print a Delta (XP branch dropped in the port)"
  nil)

;; compare-parallel.lisp
(def ^:dynamic *maximum-default-parallel-width*
  "The Maximum Default (Parallel Format) Width"
  256)

;; compare-edit.lisp
(def ^:dynamic *edit-record-writer*
  "The Standard Edit Record Writer"
  'compare.io/write-records)

;; ---------------------------------------------------------------------------
;; Record / sequence primitives shared across the whole facility
;; ---------------------------------------------------------------------------

(defn truncated?
  "Is this a truncated record?  (CL `TRUNCATED-P`.)
  A truncated record is the vector [:truncated line ...]."
  [record]
  (and (sequential? record)
       (= :truncated (first record))))

(defn nonep
  "Determine whether the sequence contains no elements.  (CL `NONEP`.)
  Works for nil, strings, lists, vectors, and any other seq."
  [sequence]
  (empty? sequence))

(defn sequence-type-of
  "Return the sequence's (essential) type.  (CL `SEQUENCE-TYPE-OF`.)
  Clojure has no distinct string/vector/list subtype lattice like Common
  Lisp, so simple-string/simple-vector/bit-vector all collapse to :string
  and :vector respectively."
  [sequence]
  (cond
    (nil? sequence)   :null
    (string? sequence) :string
    (vector? sequence) :vector
    (list? sequence)   :list
    (seq? sequence)    :list
    :else (throw (ex-info (str "Unknown sequence type for " (pr-str sequence))
                          {:sequence sequence}))))

(defn subseq
  "Extract a contiguous subsequence of `coll` between `start` (inclusive) and
  `end` (exclusive; defaults to the end).  Strings return strings, vectors
  return vectors, and other seqables return a lazy seq.  (CL `subseq`.)"
  ([coll start] (subseq coll start (count coll)))
  ([coll start end]
   (cond
     (string? coll) (subs coll start end)
     (vector? coll) (subvec coll start end)
     :else (take (- end start) (drop start coll)))))

(defn empty-of-type
  "The empty sequence of the given (essential) type.  (CL `(coerce nil type)`.)"
  [sequence-type]
  (case sequence-type
    :string ""
    :vector []
    :list '()
    :null nil
    ""))

(defn concat-of-type
  "Concatenate seqs producing a value of the given (essential) type.
  (CL `concatenate`.)"
  [sequence-type & seqs]
  (case sequence-type
    :string (apply str seqs)
    :vector (vec (apply concat seqs))
    :null nil
    (apply concat seqs)))

(defn coerce-record
  "Coerce a (string) record back to the requested type, mirroring the
  `coerce`/`intern`/`pathname` restoration in CL `NORMALIZE-ELEMENT` and
  `FRAME-RECORD`.  The port treats records as strings (or :truncated
  vectors); symbols/keywords are re-interned, chars are re-made, everything
  else round-trips as a string."
  [line line-type]
  (if (string? line-type)
    line
    (cond
      (symbol? line-type) (symbol line)
      (keyword? line-type) (keyword line)
      :else
      (case line-type
        :string line
        :list (seq line)
        :vector (vec line)
        :char (first line)
        line))))

(defn string-capitalize
  "CL `string-capitalize`: upcase the first letter of each word, downcase the
  rest (words are delimited by non-alphanumeric characters)."
  [s]
  (let [s (str s)
        n (count s)]
    (apply str
           (map-indexed
            (fn [i ^Character c]
              (let [prev (when (pos? i) (.charAt ^String s (dec i)))
                    alnum? #(Character/isLetterOrDigit ^char %)
                    start? (or (nil? prev) (not (alnum? prev)))]
                (cond
                  (not (alnum? c)) c
                  start? (Character/toUpperCase c)
                  :else (Character/toLowerCase c))))
            s))))

(defn char-code
  "CL `char-code`: the numeric code of a character."
  [c]
  (int c))

(defn invoke
  "Invoke a function-valued hook, resolving a fully-qualified symbol lazily.
  This is the analogue of CL `(apply *some-special* args)` where the special
  holds a function name.  Callers may `binding`-override the hook with an
  actual function, in which case it is called directly."
  [hook & args]
  (let [f (if (symbol? hook) (requiring-resolve hook) hook)]
    (apply f args)))
