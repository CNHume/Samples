(ns compare.face
  "The primary interfaces of the Sequence Comparison utility.

  Port of `compare-face.lisp`.  Public functions take variadic keyword args;
  functions that return multiple values return maps (`:pairs`, `:length-pair`,
  `:intervals`, `:sequence-pairs`)."
  (:require [clojure.java.io :as jio]
            [compare.core :refer [invoke nonep sequence-type-of empty-of-type
                                  concat-of-type string-capitalize
                                  *compare-normalizer* *record-reader*
                                  *compare-formatter* *default-compare-method*]]
            [compare.body :refer [basic-common-pairs]]
            [compare.scan :refer [scan-common-pairs]]
            [compare.interval :refer [subseq-intervals complement-intervals
                                      fasten-intervals affix-intervals
                                      interval-pairs]]))

(declare compare-streams compare-sequences basic-compare-sequences
         compare-intervals common-intervals common-pairs)

(defn- normalize [element opts]
  (invoke *compare-normalizer* element
          :ignore-case-and-style (:ignore-case-and-style opts)
          :ignore-whitespace (:ignore-whitespace opts)))

(defn- warn! [msg]
  (binding [*out* *err*] (println msg)))

(defn reverse-cons
  "CONS both parts of another CONS, in reverse."
  [[a b]] [b a])

;; ---------------------------------------------------------------------------
;; compare-files / compare-streams
;; ---------------------------------------------------------------------------

(defn compare-files
  "Send the result of comparing two files to a third, output file."
  [differ-p a-file b-file & {:keys [output-file] :as opts}]
  (with-open [a-stream (jio/reader a-file)]
    (with-open [b-stream (jio/reader b-file)]
      (if output-file
        (with-open [output-stream (jio/writer output-file)]
          (apply compare-streams differ-p a-stream b-stream
                 (mapcat identity (assoc opts :output-stream output-stream))))
        (apply compare-streams differ-p a-stream b-stream
               (mapcat identity opts))))))

(defn compare-streams
  "Present the result of comparing two streams via a Compare Formatter."
  [differ-p a-stream b-stream & {:as opts}]
  (let [a-sequence (invoke *record-reader* a-stream)
        b-sequence (invoke *record-reader* b-stream)
        source-pair [a-stream b-stream]]
    (apply invoke *compare-formatter* differ-p a-sequence b-sequence
           (mapcat identity (assoc opts :source-pair source-pair)))))

;; ---------------------------------------------------------------------------
;; Generated correspondence/difference wrappers
;; ---------------------------------------------------------------------------

(defn correspondence-files [& args] (apply compare-files false args))
(defn difference-files [& args] (apply compare-files true args))
(defn correspondence-streams [& args] (apply compare-streams false args))
(defn difference-streams [& args] (apply compare-streams true args))
(defn correspondence-sequences [& args] (apply compare-sequences false args))
(defn difference-sequences [& args] (apply compare-sequences true args))
(defn correspondence-intervals [& args] (apply compare-intervals false args))
(defn difference-intervals [& args] (apply compare-intervals true args))

;; ---------------------------------------------------------------------------
;; common-sequence
;; ---------------------------------------------------------------------------

(defn common-sequence
  "Return the subsequence two sequences have in common.  (CL `COMMON-SEQUENCE`.)"
  [a-sequence b-sequence & {:keys [verify-matches] :as opts}]
  (let [{:keys [intervals]} (apply common-intervals a-sequence b-sequence
                                   (mapcat identity opts))
        subseq-pairs (subseq-intervals a-sequence b-sequence intervals)
        a-seq-type (sequence-type-of a-sequence)
        b-seq-type (sequence-type-of b-sequence)
        c-seq-type (if (= a-seq-type b-seq-type) b-seq-type :list)
        c-null (empty-of-type c-seq-type)]
    (reduce
     (fn [c-subseq-reduction subseq-pair]
       (let [[a-subseq b-subseq] subseq-pair
             c-subseq
             (if (or (not verify-matches)
                     (and (= (count a-subseq) (count b-subseq))
                          (every? (fn [[a-el b-el]]
                                    (= (normalize a-el opts) (normalize b-el opts)))
                                  (map vector a-subseq b-subseq))))
               b-subseq
               (do (warn! (format "WARNING: Erroneous Match: %s and %s"
                                  (pr-str a-subseq) (pr-str b-subseq)))
                   c-null))]
         (concat-of-type c-seq-type c-subseq c-subseq-reduction)))
     c-null
     (reverse subseq-pairs))))

;; ---------------------------------------------------------------------------
;; compare-sequences / basic-compare-sequences
;; ---------------------------------------------------------------------------

(defn compare-sequences
  "Return the result of comparing two sequences as a third, output sequence."
  [& args]
  (:sequence-pairs (apply basic-compare-sequences args)))

(defn basic-compare-sequences
  "Return the various results of comparing two sequences.
  Returns `{:sequence-pairs :interval-pairs :length-pair}`."
  [differ-p a-sequence b-sequence & {:as opts}]
  (let [{:keys [intervals length-pair]}
        (apply compare-intervals differ-p a-sequence b-sequence (mapcat identity opts))]
    {:sequence-pairs (subseq-intervals a-sequence b-sequence intervals)
     :interval-pairs intervals
     :length-pair length-pair}))

;; ---------------------------------------------------------------------------
;; compare-intervals / common-intervals
;; ---------------------------------------------------------------------------

(defn compare-intervals
  "Compare two sequences returning interval pairs and a length pair.
  Returns `{:intervals interval-pairs :length-pair [a b]}`."
  [differ-p a-sequence b-sequence
   & {:keys [affix transfix prefix suffix] :or {prefix 0 suffix 0} :as opts}]
  (let [affix-sp? (contains? opts :affix)
        transfix-sp? (contains? opts :transfix)
        prefix-sp? (contains? opts :prefix)
        suffix-sp? (contains? opts :suffix)
        {:keys [intervals length-pair]} (common-intervals a-sequence b-sequence opts)
        intervals (if differ-p
                    (:intervals (complement-intervals intervals length-pair))
                    intervals)
        [transfix prefix suffix]
        (if affix-sp?
          (if (integer? affix)
            [(if transfix-sp? transfix (max affix 0))
             (if prefix-sp? prefix (int (Math/ceil (/ affix 2.0))))
             (if suffix-sp? suffix (int (Math/floor (/ affix 2.0))))]
            (do (warn! (format "The :affix keyword specifies a non-integral value: %s."
                               (pr-str affix)))
                [transfix prefix suffix]))
          [transfix prefix suffix])
        {:keys [intervals]}
        (cond
          (and transfix (integer? transfix) (not (neg? transfix)))
          (fasten-intervals intervals length-pair :transfix transfix)
          transfix
          (do (warn! (format "The :transfix keyword specifies a %s value: %s."
                             (if (integer? transfix) "negative" "non-integral")
                             (pr-str transfix)))
              {:intervals intervals :length-pair length-pair})
          :else {:intervals intervals :length-pair length-pair})
        {:keys [intervals]}
        (if (and (integer? prefix) (integer? suffix))
          (if (or (not (zero? prefix)) (not (zero? suffix)))
            (affix-intervals intervals length-pair :prefix prefix :suffix suffix)
            {:intervals intervals :length-pair length-pair})
          (do (warn! (format "The :%s keyword specifies a non-integral value: %s."
                             (if (not (integer? prefix)) "prefix" "suffix")
                             (pr-str (if (not (integer? prefix)) prefix suffix))))
              {:intervals intervals :length-pair length-pair}))]
    {:intervals intervals :length-pair length-pair}))

(defn common-intervals
  "Return the common interval sequence, of corresponding subsequence pairs.
  Returns `{:intervals :length-pair}`."
  [a-sequence b-sequence & {:as opts}]
  (let [{:keys [pairs length-pair]}
        (apply common-pairs a-sequence b-sequence (mapcat identity opts))]
    {:intervals (interval-pairs pairs) :length-pair length-pair}))

;; ---------------------------------------------------------------------------
;; common-pairs
;; ---------------------------------------------------------------------------

(defn common-pairs
  "Match the element pairs for a Longest Common Subsequence, efficiently.
  Returns `{:pairs matched-pairs :length-pair [a b]}`."
  [a-sequence b-sequence
   & {:keys [method time-method analyze-method symmetry]
      :or {method *default-compare-method*} :as opts}]
  (let [comparitor (case method
                     (:any :lcs) basic-common-pairs
                     :scan scan-common-pairs
                     (throw (ex-info (str "No such Compare Method: " (pr-str method))
                                     {:method method})))
        common-pairs-1
        (fn [& args]
          (when analyze-method (println "[Logging Pairs.]"))
          (let [{:keys [pairs length-pair lcs-length pair-count work-count work-done]}
                (if time-method (time (apply comparitor args)) (apply comparitor args))]
            (when analyze-method
              (let [use-percent (if (zero? pair-count)
                                  100.0 (/ (* 100.0 lcs-length) pair-count))
                    [a-length b-length] length-pair
                    work-maximum (* a-length b-length)
                    work-percent (if (zero? work-maximum)
                                   100.0 (/ (* 100.0 work-count) work-maximum))]
                (println (format "[%s%d/%d (%.1f%%) of the Possible Pairs.]"
                                 (if work-done (str (string-capitalize (name work-done)) " ") "")
                                 work-count work-maximum work-percent))
                (println (format "[Common Subsequence used %d/%d (%.1f%%) of the Pairs Logged.]"
                                 lcs-length pair-count use-percent))))
            {:pairs pairs :length-pair length-pair}))]
    (let [a-length (count a-sequence)
          b-length (count b-sequence)
          flip (and symmetry (not (symmetry a-length b-length)))
          [i-sequence j-sequence] (if flip [b-sequence a-sequence] [a-sequence b-sequence])
          {:keys [pairs length-pair]}
          (apply common-pairs-1 i-sequence j-sequence (mapcat identity opts))]
      (if flip
        {:pairs (mapv reverse-cons pairs) :length-pair (reverse-cons length-pair)}
        {:pairs pairs :length-pair length-pair}))))
