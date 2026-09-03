(ns compare.test
  "Test the Sequence Comparison Utility.

  Port of `compare-test.lisp`.  `diagnose-compare` returns nil when every test
  succeeds, and a non-nil (failed test name) keyword otherwise."
  (:require [compare.core :refer [sequence-type-of]]
            [compare.face :refer [common-pairs common-sequence
                                  correspondence-sequences difference-sequences]]))

(defn- warn! [msg]
  (binding [*out* *err*] (println msg)))

(defn make-random-sequence
  "Make a random sequence of the specified type (over a finite alphabet.)"
  [type size & [number]]
  (let [number (or number 26)
        elems (mapv (fn [_] (rand-int number)) (range size))]
    (case type
      :vector elems
      :string (apply str (map char elems))
      (seq elems))))

(defn supersequence
  "Deterministically build a sequence of length `long-length` that contains
  every element of `short` (in order) as a subsequence, filling the remaining
  positions from `padding`.

  `padding` must be disjoint from `short`'s elements, so that
  `LCS(short, result)` is exactly `short` — the common subsequence is
  determined by construction rather than by chance."
  [short long-length padding]
  (let [n (count short)
        pad-total (- long-length n)
        per-gap (quot pad-total (inc n))
        extra (mod pad-total (inc n))
        pad-stream (cycle padding)
        gap (fn [start len] (take len (drop start pad-stream)))]
    (loop [out []
           elems (seq short)
           consumed 0]
      (if elems
        (recur (into out (concat (gap consumed per-gap) [(first elems)]))
               (next elems)
               (+ consumed per-gap))
        (into out (gap consumed (+ per-gap extra)))))))

(defn permute-list
  "Produce all permutations of the list of (distinct) elements."
  [elements]
  (if (empty? elements)
    '(())
    (mapcat (fn [element]
              (map (fn [permutation] (cons element permutation))
                   (permute-list (remove #(= % element) elements))))
            elements)))

(defn permutations
  "Produce all permutations of the sequence of elements."
  [sequence]
  (let [t (sequence-type-of sequence)]
    (map (fn [permutation]
           (case t
             :string (apply str permutation)
             :vector (vec permutation)
             (seq permutation)))
         (permute-list (seq sequence)))))

(defn permutation-lcs-lengths
  "Find Longest Common Sequence lengths for each permutation of a sequence."
  [sequence & {:as opts}]
  (map (fn [permutation]
         (count (:pairs (apply common-pairs sequence permutation (mapcat identity opts)))))
       (permutations sequence)))

(defn diagnose-compare
  "Diagnose the Sequence Comparison Utility."
  [& {:as keys}]
  (let [flat (mapcat identity keys)
        null-pair (:pairs (apply common-pairs '() '() flat))
        null-left (:pairs (apply common-pairs '(left) '() :symmetry nil flat))
        null-right (:pairs (apply common-pairs '() '(right) :symmetry nil flat))
        a-example "abcbdda"
        b-example "badbabd"
        forward-example (apply common-sequence a-example b-example :symmetry nil flat)
        reverse-example (apply common-sequence b-example a-example :symmetry nil flat)
        desired-example "abbd"
        a-symmetric "aooboocoo dooeoo"
        b-symmetric "foogoo hooioo"
        forward-symmetric (apply common-sequence a-symmetric b-symmetric flat)
        reverse-symmetric (apply common-sequence b-symmetric a-symmetric flat)
        desired-symmetric "oooo oooo"
        a-normal '("zero" "ONE" " two" " THREE ")
        b-normal '("zero" "one" "two " "Three")
        n0 (apply common-sequence a-normal b-normal
                  :ignore-whitespace nil :ignore-case-and-style nil flat)
        n1 (apply common-sequence a-normal b-normal
                  :ignore-whitespace nil :ignore-case-and-style true flat)
        n2 (apply common-sequence a-normal b-normal
                  :ignore-whitespace true :ignore-case-and-style nil flat)
        n3 (apply common-sequence a-normal b-normal
                  :ignore-whitespace true :ignore-case-and-style true flat)
        desired-0 '("zero")
        desired-1 '("zero" "one")
        desired-2 '("zero" "two ")
        desired-3 '("zero" "one" "two " "Three")
        perm-lcs-lengths (apply permutation-lcs-lengths '(a b c d) flat)
        desired-lcs-lengths '(4 3 3 3 3 2 3 2 3 3 2 2 3 2 2 2 2 2 3 2 2 2 2 1)
        a-quadratic "0123456701234567012345670123456701234567012345670123456701234567"
        b-quadratic (apply str (reverse a-quadratic))
        forward-quadratic (apply common-sequence a-quadratic b-quadratic :symmetry nil flat)
        reverse-quadratic (apply common-sequence b-quadratic a-quadratic :symmetry nil flat)
        desired-quadratic-length 15
        a-affix '(m a d e i n)
        b-affix '(t a i w a n)
        correspondence-pairs (apply correspondence-sequences a-affix b-affix
                                     :prefix -1 :suffix 1 flat)
        a-correspondences (mapv first correspondence-pairs)
        b-correspondences (mapv second correspondence-pairs)
        desired-a-correspondences '((d) () ())
        desired-b-correspondences '(() (w) ())
        difference-pairs (apply difference-sequences a-affix b-affix
                                :prefix -1 :suffix 1 flat)
        a-differences (mapv first difference-pairs)
        b-differences (mapv second difference-pairs)
        desired-a-differences '((a) (e i) (n))
        desired-b-differences '((a) (i) (a n))
        alphabet-number 20
        improbability 2
        short-improbable-length 20
        long-improbable-length (* alphabet-number improbability short-improbable-length)
        ;; Deterministic "improbable" case: `short` is a fixed, distinct-element
        ;; sequence; `long` provably embeds it (padded from a disjoint
        ;; alphabet), so the LCS must be exactly `short`.  No RNG is involved.
        short-improbable (vec (range short-improbable-length))
        long-improbable (supersequence short-improbable long-improbable-length
                                       (range 100 (+ 100 alphabet-number)))
        forward-improbable (apply common-sequence short-improbable long-improbable
                                  :symmetry nil flat)
        reverse-improbable (apply common-sequence long-improbable short-improbable
                                  :symmetry nil flat)]
    (cond
      (or null-pair null-left null-right) :null-sequence
      (not= (count desired-example) (count forward-example)) :forward-example-length
      (not= (count desired-example) (count reverse-example)) :reverse-example-length
      (not= desired-example forward-example) :forward-example-result
      (not= desired-example reverse-example) :reverse-example-result
      (not= (count desired-symmetric) (count forward-symmetric)) :forward-symmetric-length
      (not= (count desired-symmetric) (count reverse-symmetric)) :reverse-symmetric-length
      (not= desired-symmetric forward-symmetric) :forward-symmetric-result
      (not= desired-symmetric reverse-symmetric) :reverse-symmetric-result
      (not= desired-0 n0) :0-normal-result
      (not= desired-1 n1) :1-normal-result
      (not= desired-2 n2) :2-normal-result
      (not= desired-3 n3) :3-normal-result
      (not= desired-lcs-lengths perm-lcs-lengths) :permutation-lcs-lengths
      (not= desired-quadratic-length (count forward-quadratic)) :forward-quadratic-length
      (not= desired-quadratic-length (count reverse-quadratic)) :reverse-quadratic-length
      (not= desired-a-correspondences a-correspondences) :a-correspondences-result
      (not= desired-b-correspondences b-correspondences) :b-correspondences-result
      (not= desired-a-differences a-differences) :a-differences-result
      (not= desired-b-differences b-differences) :b-differences-result
      (not= (count forward-improbable) (count reverse-improbable)) :improbable-length
      (not= short-improbable-length (count forward-improbable))
      (do (warn! (format "Shorter sequence only partly (~D of ~D elements) absorbed."
                         (count forward-improbable) short-improbable-length))
          :short-improbable-length)
      (not= short-improbable forward-improbable) :short-improbable-result)))
