(ns compare.body
  "The central Hunt–Szymanski longest-common-subsequence algorithm.

  Port of `compare-body.lisp`.  The Lisp-Machine progress interface
  (`tv:noting-progress` / `tv:note-progress`) is a no-op in the portable
  `#-symbolics` build, so [[note-progress]]/[[noting-progress]] are stubs.

  Multiple values are returned as maps (Clojure has no `values`), with keys:
  `:pairs` (matched [a b] position pairs), `:length-pair` ([a b] lengths),
  `:lcs-length`, `:pair-count`, `:match-count`, `:work-done`."
  (:require [clojure.string :as str]
            [compare.core :refer [truncated? nonep sequence-type-of coerce-record
                                  invoke *compare-normalizer* *space-characters*
                                  *redundancy-ratio* *default-compare-method*]]))

;; ---------------------------------------------------------------------------
;; Progress interfaces (portable no-ops; cf. tv:noting-progress)
;; ---------------------------------------------------------------------------

(defn note-progress
  "Note Progress, portably.  No-op outside the Lisp Machine."
  [& _args]
  nil)

(defmacro noting-progress
  "Wrap the Progress Noting forms, portably.  Expands to its body."
  [_label & body]
  `(do ~@body))

;; ---------------------------------------------------------------------------
;; Private helpers
;; ---------------------------------------------------------------------------

(defn- pathname? [x] (instance? java.io.File x))

(defn- string-trim
  "Trim the given characters from both ends of `s` (CL `string-trim`)."
  [char-bag s]
  (let [s (str s)
        n (count s)
        trims (set char-bag)
        start (loop [i 0]
                (if (and (< i n) (trims (nth s i))) (recur (inc i)) i))
        end (loop [i (dec n)]
              (if (and (>= i 0) (trims (nth s i))) (recur (dec i)) i))]
    (if (> start end) "" (subs s start (inc end)))))

;; ---------------------------------------------------------------------------
;; Element normalization
;; ---------------------------------------------------------------------------

(defn normalize-element
  "Normalize element as requested via ignore options.  (CL `NORMALIZE-ELEMENT`.)"
  [element & {:keys [ignore-case-and-style ignore-whitespace]}]
  (let [truncated (truncated? element)
        line (if truncated (second element) element)]
    (if (or ignore-case-and-style ignore-whitespace)
      (let [line-type (cond
                        (char? line) :char
                        :else (sequence-type-of line))
            normalization (if (pathname? line) (str line) (str line))
            normalization (if ignore-whitespace
                            (string-trim *space-characters* normalization)
                            normalization)
            normalization (if ignore-case-and-style
                            (str/upper-case normalization)
                            normalization)
            line (cond
                   (pathname? line) (java.io.File. normalization)
                   (symbol? line) (symbol normalization)
                   (keyword? line) (keyword normalization)
                   :else (coerce-record normalization line-type))]
        (if truncated
          (into [:truncated line] (drop 2 element))
          line))
      line)))

;; ---------------------------------------------------------------------------
;; list-matches
;; ---------------------------------------------------------------------------

(defn list-matches
  "Return (ordered) position sub-lists of B elements equal to each A.

  Returns `{:matches v :match-count n}` where `(v i)` is `nil` or a seq of the
  B-sequence positions that match A element `i`, in descending order by
  default (ascending when `:order-up` is truthy).  `:match-count` is the total
  number of matched element pairs.  (CL `LIST-MATCHES`.)"
  [a-sequence b-sequence & {:keys [order-up ignore-case-and-style
                                   ignore-whitespace]}]
  (let [a-length (count a-sequence)
        b-length (count b-sequence)]
    (if (pos? b-length)
      (let [;; key -> vector of b-indices, in ascending encounter order.
            b-table (reduce (fn [m [i el]]
                              (update m
                                      (invoke *compare-normalizer* el
                                              :ignore-case-and-style ignore-case-and-style
                                              :ignore-whitespace ignore-whitespace)
                                      (fnil conj []) i))
                            {}
                            (map-indexed vector b-sequence))
            ;; CL builds the lists by `push` (descending); :order-up nreverses.
            indices-for (fn [v]
                          (when v
                            (if order-up (seq v) (seq (rseq v)))))]
        (let [[matches match-count]
              (reduce (fn [[acc cnt] a-element]
                        (let [v (get b-table
                                     (invoke *compare-normalizer* a-element
                                             :ignore-case-and-style ignore-case-and-style
                                             :ignore-whitespace ignore-whitespace))]
                          [(conj acc (indices-for v))
                           (+ cnt (count v))]))
                      [[] 0]
                      a-sequence)]
          {:matches matches :match-count match-count}))
      {:matches (vec (repeat a-length nil)) :match-count 0})))

;; ---------------------------------------------------------------------------
;; binary-position
;; ---------------------------------------------------------------------------

(defn binary-position
  "Find the position for an item within a sorted array, efficiently.

  Assumes `array` is sorted such that `(predicate item (key array[i]))` is true
  for all positions strictly preceding the result and false from the result on.
  (CL `BINARY-POSITION`.)"
  [item array predicate & {:keys [key start end]}]
  (let [start (or start 0)
        end (or end (count array))]
    (if (and (<= start end) (<= end (count array)))
      (loop [start-position start
             end-position end
             item-position nil]
        (if (>= start-position end-position)
          item-position
          (let [array-position (quot (+ start-position end-position) 2)
                array-element (nth array array-position)
                array-item (if key (key array-element) array-element)]
            (if (predicate item array-item)
              ;; item strictly less than current: search toward the start
              (recur start-position array-position array-position)
              ;; item greater-or-equal: search toward the end
              (recur (inc array-position) end-position item-position)))))
      (throw (ex-info (str "Invalid bounds: start = " (pr-str start)
                           ", end = " (pr-str end)
                           "; for array " (pr-str array))
                      {:start start :end end :array array})))))

;; ---------------------------------------------------------------------------
;; basic-common-pairs -- the Hunt-Szymanski algorithm
;; ---------------------------------------------------------------------------

(defn basic-common-pairs
  "Match elements in the longest subsequence common to two sequences, quickly.

  Returns `{:pairs matched-pairs :length-pair [a b] :lcs-length n
            :pair-count p :match-count m :work-done w}`.
  (CL `BASIC-COMMON-PAIRS`.)"
  [a-sequence b-sequence & {:keys [method optimize-log-entry debug-method]
                            :or {method *default-compare-method*
                                 optimize-log-entry true}
                            :as opts}]
  (let [a-length (count a-sequence)
        b-length (count b-sequence)
        work-done :matched
        {:keys [matches match-count]}
        (list-matches a-sequence b-sequence (assoc opts :order-up false))]
    (letfn [(equivalent [a-element b-element]
              ;; Normalize and test an element pair for equality.
              (= (invoke *compare-normalizer* a-element
                         :ignore-case-and-style (:ignore-case-and-style opts)
                         :ignore-whitespace (:ignore-whitespace opts))
                 (invoke *compare-normalizer* b-element
                         :ignore-case-and-style (:ignore-case-and-style opts)
                         :ignore-whitespace (:ignore-whitespace opts))))]
      (if (and (= method :any)
               (= a-length b-length)
               (< (* b-length *redundancy-ratio*) match-count)
               (every? (fn [[a-el b-el]] (equivalent a-el b-el))
                       (map vector a-sequence b-sequence)))
        ;; Perfect match: matched pairs are (k . k) for k = b-length .. 1.
        {:pairs (mapv (fn [k] [k k]) (range b-length 0 -1))
         :length-pair [a-length b-length]
         :lcs-length b-length
         :pair-count b-length
         :work-count match-count
         :work-done work-done}
        ;; The actual algorithm:
        (let [threshold (vec (repeat (inc a-length) b-length))
              log-entry (vec (repeat a-length nil))]
          (loop [a-position 0
                 ab-common-remains (seq matches)
                 threshold threshold
                 log-entry log-entry
                 pair-count 0]
            (if (empty? ab-common-remains)
              (let [c-position (binary-position b-length threshold <=)]
                {:pairs (when (pos? c-position)
                          (vec (reverse (nth log-entry (dec c-position)))))
                 :length-pair [a-length b-length]
                 :lcs-length c-position
                 :pair-count pair-count
                 :work-count match-count
                 :work-done work-done})
              (let [[threshold log-entry pair-count]
                    (loop [b-common-remains (seq (first ab-common-remains))
                           c-limit (inc a-position)
                           threshold threshold
                           log-entry log-entry
                           pair-count pair-count]
                      (if (empty? b-common-remains)
                        [threshold log-entry pair-count]
                        (let [b-position (first b-common-remains)
                              b-lower (second b-common-remains)
                              c-position (binary-position b-position threshold
                                                          <= :end c-limit)
                              c-limit (inc c-position)
                              log-optimizable?
                              (not (and optimize-log-entry b-lower
                                        (or (not (pos? c-position))
                                            (> b-lower (nth threshold (dec c-position))))))
                              [threshold log-entry pair-count]
                              (if (and (< b-position (nth threshold c-position))
                                       log-optimizable?)
                                (let [last-entry (when (pos? c-position)
                                                   (nth log-entry (dec c-position)))
                                      next-pair [a-position b-position]]
                                  (when debug-method
                                    (println (format "i =%3d, j =%3d < THRESHOLD[k =%3d] =%3d"
                                                     a-position b-position c-position
                                                     (nth threshold c-position))))
                                  [(assoc threshold c-position b-position)
                                   (assoc log-entry c-position (conj last-entry next-pair))
                                   (inc pair-count)])
                                [threshold log-entry pair-count])]
                          (recur (next b-common-remains) c-limit
                                 threshold log-entry pair-count))))]
                (recur (inc a-position) (next ab-common-remains)
                       threshold log-entry pair-count)))))))))
