(ns compare.lab.dynamic
  "The \"Standard\" Dynamic Programming (LCS) algorithm.

  Port of `lab/compare-dynamic.lisp`."
  (:require [compare.body :refer [list-matches]]))

(defn basic-common-pairs
  "Match elements in the longest subsequence common to two sequences, simply.
  Returns `{:pairs :length-pair :lcs-length :pair-count}`."
  [a-sequence b-sequence & {:keys [debug-log-entry] :as opts}]
  (let [a-length (count a-sequence)
        b-length (count b-sequence)
        {:keys [matches]} (list-matches a-sequence b-sequence (assoc opts :order-up true))]
    (loop [a-position 0
           ab-common-remains (seq matches)
           distance (vec (repeat b-length 0))
           pairs-back (vec (repeat b-length nil))
           pair-count 0]
      (if (= a-position a-length)
        {:pairs (when (pos? b-length)
                  (vec (reverse (nth pairs-back (dec b-length)))))
         :length-pair [a-length b-length]
         :lcs-length (if (zero? b-length) 0 (nth distance (dec b-length)))
         :pair-count pair-count}
        (let [[distance pairs-back pair-count]
              (loop [b-position 0
                     b-common-remains (first ab-common-remains)
                     diagonal-distance 0
                     vertical-distance 0
                     diagonal-pairs nil
                     vertical-pairs nil
                     distance distance
                     pairs-back pairs-back
                     pair-count pair-count]
                (if (= b-position b-length)
                  [distance pairs-back pair-count]
                  (let [horizontal-distance (nth distance b-position)
                        horizontal-pairs (nth pairs-back b-position)
                        matched (and b-common-remains
                                     (= (first b-common-remains) b-position))
                        [distance pairs-back pair-count b-common-remains]
                        (if matched
                          (let [next-pair [a-position b-position]]
                            [(assoc distance b-position (inc diagonal-distance))
                             (assoc pairs-back b-position (conj diagonal-pairs next-pair))
                             (inc pair-count)
                             (next b-common-remains)])
                          (if (< horizontal-distance vertical-distance)
                            [(assoc distance b-position vertical-distance)
                             (assoc pairs-back b-position vertical-pairs)
                             pair-count
                             b-common-remains]
                            [distance pairs-back pair-count b-common-remains]))
                        diagonal-distance horizontal-distance
                        vertical-distance (nth distance b-position)
                        diagonal-pairs horizontal-pairs
                        vertical-pairs (nth pairs-back b-position)]
                    (recur (inc b-position) b-common-remains
                           diagonal-distance vertical-distance diagonal-pairs vertical-pairs
                           distance pairs-back pair-count))))]
          (recur (inc a-position) (next ab-common-remains)
                 distance pairs-back pair-count))))))
