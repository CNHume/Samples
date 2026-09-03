(ns compare.interval
  "Support operators over \"Interval Lists\".

  Port of `compare-interval.lisp`.  An interval is `[start end]` (or `nil` for
  \"no interval\" -- Clojure has no NIL-as-empty-list, so a missing interval is
  always `nil`, never `[]`).  An interval pair is `[a-interval b-interval]`.
  Functions that return two values return `{:intervals ... :length-pair ...}`."
  (:refer-clojure :exclude [subseq])
  (:require [compare.core :refer [nonep subseq]]))

(defn subseq-intervals
  "Extract the subsequence pairs specified by the interval pairs.

  Returns a vector of `[a-subseq b-subseq]`; a nil interval yields `'()`.
  (The CL implementation maintains a list-cursor to avoid repeated traversal;
  Clojure's `subseq` handles all seq types, so we simply use it.)"
  [a-sequence b-sequence interval-pairs]
  (mapv (fn [[a-interval b-interval]]
          [(if a-interval
             (subseq a-sequence (first a-interval) (second a-interval))
             '())
           (if b-interval
             (subseq b-sequence (first b-interval) (second b-interval))
             '())])
        interval-pairs))

(defn fasten-intervals
  "Fasten adjacent intervals, if both pairs lie within the specified transfix."
  [interval-pairs length-pair & {:keys [transfix] :or {transfix 0}}]
  (if (and (seq interval-pairs) transfix)
    (let [a-length (first length-pair)
          b-length (second length-pair)
          terminal-pair [[a-length a-length] [b-length b-length]]
          remains (concat interval-pairs [terminal-pair])]
      (loop [remains remains
             last-a-start 0 last-b-start 0
             last-a-end 0 last-b-end 0
             intraval-pending false
             intraval-pairs []]
        (if (empty? remains)
          {:intervals intraval-pairs :length-pair length-pair}
          (let [[a-interval b-interval] (first remains)
                next-remains (rest remains)
                a-start (first a-interval) a-end (second a-interval)
                b-start (first b-interval) b-end (second b-interval)
                a-gap (- a-start last-a-end)
                b-gap (- b-start last-b-end)
                intraval-broken (or (> a-gap transfix) (> b-gap transfix))
                [last-a-end last-b-end intraval-pending]
                (if intraval-broken
                  [last-a-end last-b-end intraval-pending]
                  (let [intraval-pending (if (and (zero? last-a-end) (zero? last-b-end))
                                           true
                                           intraval-pending)]
                    (if intraval-pending
                      [a-end b-end intraval-pending]
                      [last-a-end last-b-end intraval-pending])))
                [intraval-pairs intraval-pending]
                (if (and intraval-pending (or intraval-broken (empty? next-remains)))
                  [(conj intraval-pairs
                         [[last-a-start last-a-end] [last-b-start last-b-end]])
                   false]
                  [intraval-pairs intraval-pending])
                [last-a-start last-b-start last-a-end last-b-end intraval-pending]
                (if intraval-pending
                  [last-a-start last-b-start last-a-end last-b-end intraval-pending]
                  [a-start b-start a-end b-end true])]
            (recur next-remains
                   last-a-start last-b-start last-a-end last-b-end
                   intraval-pending intraval-pairs)))))
    {:intervals interval-pairs :length-pair length-pair}))

(defn affix-intervals
  "Affix the specified number of records to each interval pair."
  [interval-pairs length-pair & {:keys [prefix suffix] :or {prefix 0 suffix 0}}]
  (let [a-length (first length-pair)
        b-length (second length-pair)]
    (loop [a-position 0 b-position 0
           last-a-end 0 last-b-end 0
           superval-pairs []
           remains (seq interval-pairs)]
      (if (empty? remains)
        {:intervals superval-pairs :length-pair length-pair}
        (let [[a-interval b-interval] (first remains)
              next-interval-pair (second remains)
              next-a-interval (first next-interval-pair)
              next-b-interval (second next-interval-pair)
              [a-superval last-a-end a-position]
              (if a-interval
                (let [a-start (first a-interval)
                      a-end (second a-interval)
                      next-a-start (first next-a-interval)
                      super-a-start (max (min (- a-start prefix) a-end)
                                         a-position last-a-end)
                      super-a-end (min (max (+ a-end suffix)
                                            super-a-start a-start)
                                       (or next-a-start a-length))]
                  [(when (<= super-a-start super-a-end)
                     [super-a-start super-a-end])
                   a-end
                   super-a-end])
                [nil last-a-end a-position])
              [b-superval last-b-end b-position]
              (if b-interval
                (let [b-start (first b-interval)
                      b-end (second b-interval)
                      next-b-start (first next-b-interval)
                      super-b-start (max (min (- b-start prefix) b-end)
                                         last-b-end b-position)
                      super-b-end (min (max (+ b-end suffix)
                                            super-b-start b-start)
                                       (or next-b-start b-length))]
                  [(when (<= super-b-start super-b-end)
                     [super-b-start super-b-end])
                   b-end
                   super-b-end])
                [nil last-b-end b-position])]
          (recur a-position b-position last-a-end last-b-end
                 (if (or a-superval b-superval)
                   (conj superval-pairs [a-superval b-superval])
                   superval-pairs)
                 (rest remains)))))))

(defn complement-intervals
  "Given an interval sequence, return its complementary interval sequence."
  [interval-pairs length-pair & _keys]
  (let [a-length (first length-pair)
        b-length (second length-pair)
        terminal-pair [[a-length a-length] [b-length b-length]]
        remains (concat interval-pairs [terminal-pair])]
    (loop [remains remains a-position 0 b-position 0 exterval-pairs []]
      (if (empty? remains)
        {:intervals exterval-pairs :length-pair length-pair}
        (let [[a-interval b-interval] (first remains)
              [interval-broken a-exterval a-position]
              (if a-interval
                (let [a-start (first a-interval) a-end (second a-interval)]
                  [(and (pos? a-start) (< a-position a-length) (<= a-position a-start))
                   [a-position a-start]
                   a-end])
                [false nil a-position])
              [interval-broken b-exterval b-position]
              (if b-interval
                (let [b-start (first b-interval) b-end (second b-interval)]
                  [(or interval-broken
                       (and (pos? b-start) (< b-position b-length) (<= b-position b-start)))
                   [b-position b-start]
                   b-end])
                [interval-broken nil b-position])]
          (recur (rest remains) a-position b-position
                 (if interval-broken
                   (conj exterval-pairs [a-exterval b-exterval])
                   exterval-pairs)))))))

(defn final-interval
  "Return the final interval pair (the interval AFTER the last)."
  [interval-pairs length-pair]
  (let [last-interval (last interval-pairs)
        a-start (if last-interval (second (first last-interval)) 0)
        b-start (if last-interval (second (second last-interval)) 0)
        a-end (first length-pair)
        b-end (second length-pair)]
    [[a-start a-end] [b-start b-end]]))

(defn interval-pairs
  "Convert a sequence of [a b] position pairs into a sequence of intervals."
  [pairs]
  (loop [a-start nil b-start nil
         pending false
         a-index 0 b-index 0
         out []
         remains (seq pairs)]
    (if (empty? remains)
      out
      (let [[a-position b-position] (first remains)
            next-remains (rest remains)
            [broken a-index b-index a-start b-start pending]
            (if pending
              [(not (and (= a-position a-index) (= b-position b-index)))
               a-index b-index a-start b-start pending]
              [false a-position b-position a-position b-position true])
            [out a-index b-index a-start b-start]
            (if broken
              [(conj out [[a-start a-index] [b-start b-index]])
               a-position b-position a-position b-position]
              [out a-index b-index a-start b-start])
            out (if (and (empty? next-remains) pending)
                  (conj out [[a-start (inc a-index)] [b-start (inc b-index)]])
                  out)]
        (recur a-start b-start pending (inc a-index) (inc b-index) out next-remains)))))

(defn interval-led?
  "Determine whether the interval leads the exterval."
  [interval-pair exterval-pair]
  (and interval-pair
       (or (nil? exterval-pair)
           (let [[a-interval b-interval] interval-pair
                 [a-exterval b-exterval] exterval-pair
                 a-interval-start (first a-interval)
                 b-interval-start (first b-interval)
                 a-exterval-start (first a-exterval)
                 b-exterval-start (first b-exterval)]
             (or (< b-interval-start b-exterval-start)
                 (and (= b-interval-start b-exterval-start)
                      (< a-interval-start a-exterval-start)))))))
