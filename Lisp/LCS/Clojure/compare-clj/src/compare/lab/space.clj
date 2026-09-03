(ns compare.lab.space
  "The Linear Space LCS algorithm [D.S. Hirschberg].

  Port of `lab/compare-space.lisp`.  `common-length` fills (and returns) a
  distance vector; `basic-common-pairs` recurses on itself (in CL the recursion
  went through the shared `common-pairs`, which this namespace does not
  redefine)."
  (:refer-clojure :exclude [subseq])
  (:require [compare.core :refer [invoke subseq *compare-normalizer*]]
            [compare.body :refer [list-matches]]))

(defn- normalize [element opts]
  (invoke *compare-normalizer* element
          :ignore-case-and-style (:ignore-case-and-style opts)
          :ignore-whitespace (:ignore-whitespace opts)))

(defn common-length
  "Obtain the lengths of any longest common subsequence, simply.
  Returns the filled distance vector (its last element is the LCS length)."
  [a-sequence b-sequence & {:keys [length-pair ab-matches distance] :as opts}]
  (let [length-pair-sp? (contains? opts :length-pair)
        ab-matches-sp? (contains? opts :ab-matches)
        a-length (if length-pair-sp? (first length-pair) (count a-sequence))
        b-length (if length-pair-sp? (second length-pair) (count b-sequence))
        ab-matches (if ab-matches-sp?
                     (cons :too-bad ab-matches)
                     (:matches (list-matches a-sequence b-sequence
                                             (assoc opts :order-up true))))
        distance (or distance (vec (repeat b-length 0)))]
    (loop [a-position 0
           ab-common-remains (seq ab-matches)
           distance distance]
      (if (= a-position a-length)
        distance
        (let [distance
              (loop [b-position 0
                     b-common-remains (first ab-common-remains)
                     diagonal-distance 0
                     vertical-distance 0
                     distance distance]
                (if (= b-position b-length)
                  distance
                  (let [horizontal-distance (nth distance b-position)
                        matched (and b-common-remains
                                     (= (first b-common-remains) b-position))
                        b-common-remains (if matched
                                           (next b-common-remains)
                                           b-common-remains)
                        distance (if matched
                                   (assoc distance b-position (inc diagonal-distance))
                                   (if (< horizontal-distance vertical-distance)
                                     (assoc distance b-position vertical-distance)
                                     distance))
                        diagonal-distance horizontal-distance
                        vertical-distance (nth distance b-position)]
                    (recur (inc b-position) b-common-remains
                           diagonal-distance vertical-distance distance))))]
          (recur (inc a-position) (next ab-common-remains) distance))))))

(defn basic-common-pairs
  "Match elements of a longest common subsequence, while conserving memory.
  Returns `{:pairs :length-pair :lcs-length :pair-count}`."
  [a-sequence b-sequence & {:keys [length-pair debug-log-entry] :as opts}]
  (let [length-pair-sp? (contains? opts :length-pair)
        a-length (if length-pair-sp? (first length-pair) (count a-sequence))
        b-length (if length-pair-sp? (second length-pair) (count b-sequence))]
    (cond
      (or (zero? a-length) (zero? b-length))
      {:pairs '() :length-pair [a-length b-length] :lcs-length 0 :pair-count 0}

      (= a-length 1)
      (let [a-normal (normalize (nth a-sequence 0) opts)
            b-match (first (keep-indexed (fn [i el]
                                           (when (= a-normal (normalize el opts)) i))
                                         b-sequence))
            match-pairs (when b-match [[0 b-match]])
            lcs-length (count match-pairs)]
        {:pairs match-pairs :length-pair [a-length b-length]
         :lcs-length lcs-length :pair-count lcs-length})

      (= b-length 1)
      (let [b-normal (normalize (nth b-sequence 0) opts)
            a-match (first (keep-indexed (fn [i el]
                                           (when (= b-normal (normalize el opts)) i))
                                         a-sequence))
            match-pairs (when a-match [[a-match 0]])
            lcs-length (count match-pairs)]
        {:pairs match-pairs :length-pair [a-length b-length]
         :lcs-length lcs-length :pair-count lcs-length})

      :else
      (let [mid-point (quot a-length 2)
            prefix-distance (common-length (subseq a-sequence 0 mid-point) b-sequence
                                           (assoc opts :length-pair [mid-point b-length]))
            suffix-distance (common-length (reverse (subseq a-sequence mid-point))
                                           (reverse b-sequence)
                                           (assoc opts :length-pair [(- a-length mid-point) b-length]))
            [max-point max-length]
            (loop [b-position -1 max-point nil max-length nil]
              (if (= b-position b-length)
                [max-point max-length]
                (let [next-length (+ (if (< b-position 0)
                                       0
                                       (nth prefix-distance b-position))
                                     (if (= b-position (dec b-length))
                                       0
                                       (nth suffix-distance (- b-length 2 b-position))))]
                  (if (or (nil? max-point) (< max-length next-length))
                    (recur (inc b-position) (inc b-position) next-length)
                    (recur (inc b-position) max-point max-length)))))]
        (when debug-log-entry
          (println (format "mid-point = %3d, max-point = %3d, max-length = %d"
                           mid-point max-point max-length)))
        (let [{prefix-pairs :pairs prefix-length-pair :length-pair}
              (basic-common-pairs (subseq a-sequence 0 mid-point)
                                  (subseq b-sequence 0 max-point) opts)
              {suffix-pairs :pairs}
              (basic-common-pairs (subseq a-sequence mid-point)
                                  (subseq b-sequence max-point) opts)
              offsetter (fn [offset-pair pairs]
                          (let [[a-offset b-offset] offset-pair]
                            (map (fn [[a-position b-position]]
                                   [(+ a-position a-offset) (+ b-position b-offset)])
                                 pairs)))
              match-pairs (concat prefix-pairs (offsetter prefix-length-pair suffix-pairs))
              lcs-length (+ (count prefix-pairs) (count suffix-pairs))]
          {:pairs match-pairs :length-pair [a-length b-length]
           :lcs-length lcs-length :pair-count lcs-length})))))
