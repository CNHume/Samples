(ns compare.scan
  "The (traditional) windowed scan algorithm.

  Port of `compare-scan.lisp`.  `scan-common-pairs` returns the same
  `{:pairs :length-pair :lcs-length :pair-count :work-count :work-done}` map as
  `compare.body/basic-common-pairs` (its 5th value is the number of pairs
  *compared* rather than matched)."
  (:require [compare.core :refer [invoke *compare-normalizer*]]))

(defn- normalize
  "Normalize `element` with the options in `opts`."
  [element opts]
  (invoke *compare-normalizer* element
          :ignore-case-and-style (:ignore-case-and-style opts)
          :ignore-whitespace (:ignore-whitespace opts)))

(defn- match-window
  "Try to match `match-range` consecutive elements starting at [a-index b-index].
  Returns `{:matched? bool :check-count n}`."
  [a-sequence b-sequence a-list? b-list? a-rest b-rest a-index b-index
   match-range matcher check-count]
  (loop [mi 0
         a-rest (when a-list? a-rest)
         b-rest (when b-list? b-rest)
         check-count check-count]
    (if (= mi match-range)
      {:matched? true :check-count check-count}
      (let [a-element (if a-list? (first a-rest) (nth a-sequence (+ a-index mi)))
            b-element (if b-list? (first b-rest) (nth b-sequence (+ b-index mi)))
            check-count (inc check-count)]
        (if (matcher a-element b-element)
          (recur (inc mi)
                 (when a-list? (next a-rest))
                 (when b-list? (next b-rest))
                 check-count)
          {:matched? false :check-count check-count})))))

(defn handle-difference
  "Attempt to resynchronize, after encountering a difference.

  Returns `{:a-position :b-position :a-remains :b-remains :done? :check-count}`.
  (CL `HANDLE-DIFFERENCE`.)"
  [a-position b-position a-remains b-remains
   a-sequence b-sequence a-length b-length [a-list? b-list?]
   & {:keys [scan-window matcher check-count]
      :or {scan-window 1 matcher = check-count 0}}]
  (loop [a-cross (inc a-position) b-cross (inc b-position)
         a-tail (next a-remains) b-tail (next b-remains)
         check-count check-count]
    (if (and (>= a-cross a-length) (>= b-cross b-length))
      {:a-position a-position :b-position b-position
       :a-remains a-remains :b-remains b-remains
       :done? true :check-count check-count}
      (let [;; Scan for a window of the other sequence "cross" from it.
            res1 (when (< b-cross b-length)
                   (let [a-bound (min (inc a-cross) a-length)]
                     (loop [a-index a-position
                            a-scan a-remains
                            check-count check-count]
                       (if (>= a-index a-bound)
                         {:matched? false :check-count check-count}
                         (let [match-range (min scan-window
                                                (- a-length a-index)
                                                (- b-length b-cross))
                               {:keys [matched? check-count]}
                               (match-window a-sequence b-sequence a-list? b-list?
                                             a-scan b-tail a-index b-cross
                                             match-range matcher check-count)]
                           (if matched?
                             {:matched? true :a-position a-index :b-position b-cross
                              :a-remains a-scan :b-remains b-tail
                              :check-count check-count}
                             (recur (inc a-index) (next a-scan) check-count)))))))
            check-count (if res1 (:check-count res1) check-count)]
        (if (:matched? res1)
          (dissoc (assoc res1 :done? false) :matched?)
          (let [res2 (when (< a-cross a-length)
                       (let [b-bound (min (inc b-cross) b-length)]
                         (loop [b-index b-position
                                b-scan b-remains
                                check-count check-count]
                           (if (>= b-index b-bound)
                             {:matched? false :check-count check-count}
                             (let [match-range (min scan-window
                                                    (- a-length a-cross)
                                                    (- b-length b-index))
                                   {:keys [matched? check-count]}
                                   (match-window a-sequence b-sequence a-list? b-list?
                                                 a-tail b-scan a-cross b-index
                                                 match-range matcher check-count)]
                               (if matched?
                                 {:matched? true :a-position a-cross :b-position b-index
                                  :a-remains a-tail :b-remains b-scan
                                  :check-count check-count}
                                 (recur (inc b-index) (next b-scan) check-count)))))))
                check-count (if res2 (:check-count res2) check-count)]
            (if (:matched? res2)
              (dissoc (assoc res2 :done? false) :matched?)
              (recur (inc a-cross) (inc b-cross)
                     (next a-tail) (next b-tail)
                     check-count))))))))

(defn scan-common-pairs
  "Match elements common to two sequences, the old fashioned way.

  Returns `{:pairs matched-pairs :length-pair [a b] :lcs-length n
            :pair-count p :work-count c :work-done :checked}`."
  [a-sequence b-sequence & {:keys [transfix debug-method] :or {transfix 0} :as opts}]
  (letfn [(equivalent [a-element b-element]
            (= (normalize a-element opts) (normalize b-element opts)))]
    (let [a-length (count a-sequence)
          b-length (count b-sequence)
          length-pair [a-length b-length]
          a-list? (list? a-sequence)
          b-list? (list? b-sequence)
          scan-window (inc transfix)
          work-done :checked]
      (loop [a-position 0 b-position 0
             a-remains (when a-list? a-sequence)
             b-remains (when b-list? b-sequence)
             pair-stack [] pair-count 0 check-count 0]
        (if (or (>= a-position a-length) (>= b-position b-length))
          {:pairs pair-stack
           :length-pair length-pair
           :lcs-length (count pair-stack)
           :pair-count pair-count
           :work-count check-count
           :work-done work-done}
          (let [a-element (if a-list? (first a-remains) (nth a-sequence a-position))
                b-element (if b-list? (first b-remains) (nth b-sequence b-position))]
            (when debug-method
              (println (format "Comparing: %s and %s." (pr-str a-element) (pr-str b-element))))
            (if (equivalent a-element b-element)
              (recur (inc a-position) (inc b-position)
                     (when a-list? (next a-remains))
                     (when b-list? (next b-remains))
                     (conj pair-stack [a-position b-position])
                     (inc pair-count)
                     (inc check-count))
              (let [{:keys [a-position b-position a-remains b-remains done? check-count]}
                    (handle-difference a-position b-position a-remains b-remains
                                       a-sequence b-sequence a-length b-length
                                       [a-list? b-list?]
                                       :scan-window scan-window
                                       :matcher equivalent
                                       :check-count check-count)]
                (if done?
                  {:pairs pair-stack
                   :length-pair length-pair
                   :lcs-length (count pair-stack)
                   :pair-count pair-count
                   :work-count check-count
                   :work-done work-done}
                  (recur (inc a-position) (inc b-position)
                         (when a-list? (next a-remains))
                         (when b-list? (next b-remains))
                         (conj pair-stack [a-position b-position])
                         (inc pair-count)
                         (inc check-count)))))))))))
