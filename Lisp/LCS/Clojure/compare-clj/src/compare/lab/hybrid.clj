(ns compare.lab.hybrid
  "Upgrade of `compare-space.lisp` to a Hybrid LCS algorithm.

  Port of `lab/compare-hybrid.lisp`.  Provides only `common-length`, which
  fills (and returns) the distance vector using the Hunt-Szymanski threshold."
  (:refer-clojure :exclude [subseq])
  (:require [compare.body :refer [list-matches binary-position]]))

(defn common-length
  "Obtain the lengths of any longest common subsequence, quickly.
  Returns the filled distance vector."
  [a-sequence b-sequence & {:keys [length-pair ab-matches distance] :as opts}]
  (let [length-pair-sp? (contains? opts :length-pair)
        ab-matches-sp? (contains? opts :ab-matches)
        a-length (if length-pair-sp? (first length-pair) (count a-sequence))
        b-length (if length-pair-sp? (second length-pair) (count b-sequence))
        ab-matches (if ab-matches-sp?
                     (cons :too-bad ab-matches)
                     (:matches (list-matches a-sequence b-sequence
                                             (assoc opts :order-up false))))
        distance (or distance (vec (repeat b-length 0)))
        threshold (vec (repeat (inc a-length) b-length))]
    (let [threshold
          (loop [a-position 0
                 ab-common-remains (seq ab-matches)
                 threshold threshold]
            (if (= a-position a-length)
              threshold
              (let [threshold
                    (loop [b-common-remains (first ab-common-remains)
                           c-limit (inc a-position)
                           threshold threshold]
                      (if (empty? b-common-remains)
                        threshold
                        (let [b-position (first b-common-remains)
                              c-position (binary-position b-position threshold <= :end c-limit)
                              c-limit (inc c-position)]
                          (recur (next b-common-remains) c-limit
                                 (if (< b-position (nth threshold c-position))
                                   (assoc threshold c-position b-position)
                                   threshold)))))]
                (recur (inc a-position) (next ab-common-remains) threshold))))]
      ;; Convert the threshold into a distance vector.
      (loop [b-index 0 c-index 0 distance distance]
        (if (= b-index b-length)
          distance
          (let [b-threshold (nth threshold c-index)
                c-index (if (< b-index b-threshold) c-index (inc c-index))]
            (recur (inc b-index) c-index
                   (assoc distance b-index c-index))))))))
