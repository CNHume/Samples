(ns compare.series
  "The \"Series Compare Formatter\".

  Port of `compare-series.lisp` (portable build)."
  (:require [compare.core :refer [nonep]]
            [compare.face :refer [basic-compare-sequences]]
            [compare.interval :refer [complement-intervals subseq-intervals
                                      final-interval interval-led?]]
            [compare.io :refer [stream-name-pair stream-presentation stream-width
                                write-frame fresh-line!]]
            [compare.format :refer [diagnose-pair initial-header find-header]]
            [compare.merge :refer [write-half-head]]))

(defn write-pair-in-series
  "Format a matched record pair vertically and send it to the output stream."
  [in-seq-pair ex-seq-pair interval-pair exterval-pair interval-index diagnostic
   header-type-pair source-pair name-pair output-stream width object-pair type-pair]
  (let [a-index 0
        b-index 1
        [a-source b-source] source-pair
        [a-object b-object] object-pair
        [a-type b-type] type-pair
        [a-records b-records] in-seq-pair
        header-type-pair-sp? (some? header-type-pair)
        [a-initial-p b-initial-p]
        (when header-type-pair-sp? (initial-header header-type-pair in-seq-pair))
        head-width (if (some? width) width (stream-width output-stream))
        width-keys (when (some? width) [:width width])
        prognostic :header
        {:keys [headers headervals]}
        (when (and header-type-pair-sp? (not (and a-initial-p b-initial-p)))
          (find-header header-type-pair ex-seq-pair exterval-pair))
        a-headerval (if a-initial-p nil (first headervals))
        b-headerval (if b-initial-p nil (second headervals))]
    (when a-headerval
      (write-half-head a-index headervals interval-index name-pair prognostic
                       output-stream head-width a-object a-type)
      (apply write-frame (first headers) output-stream a-source a-headerval width-keys))
    (write-half-head a-index interval-pair interval-index name-pair diagnostic
                     output-stream head-width a-object a-type)
    (when-not (nonep a-records)
      (apply write-frame a-records output-stream a-source (first interval-pair) width-keys))
    (when b-headerval
      (write-half-head b-index headervals interval-index name-pair prognostic
                       output-stream head-width b-object b-type)
      (apply write-frame (second headers) output-stream b-source b-headerval width-keys))
    (write-half-head b-index interval-pair interval-index name-pair diagnostic
                     output-stream head-width b-object b-type)
    (when-not (nonep b-records)
      (apply write-frame b-records output-stream b-source (second interval-pair) width-keys))
    nil))

(defn compare-in-series
  "Perform a Formatted Compare in Series.  (CL `COMPARE-IN-SERIES`.)"
  [differ-p a-sequence b-sequence
   & {:keys [source-pair output-stream width header-type-pair]
      :or {output-stream *out*} :as opts}]
  (let [{:keys [sequence-pairs interval-pairs length-pair]}
        (apply basic-compare-sequences differ-p a-sequence b-sequence (mapcat identity opts))
        {:keys [intervals]} (complement-intervals interval-pairs length-pair)
        ex-seq-pairs (subseq-intervals a-sequence b-sequence intervals)]
    (when (seq interval-pairs)
      (let [name-pair (stream-name-pair source-pair)
            [a-source b-source] source-pair
            first-interval-pair (first interval-pairs)
            first-exterval-pair (first intervals)
            interval-led (interval-led? first-interval-pair first-exterval-pair)
            final-pair (final-interval interval-pairs length-pair)
            exterval-pairs (if interval-led (cons [[0 0] [0 0]] intervals) intervals)
            ex-seq-pairs (if interval-led (cons ['() '()] ex-seq-pairs) ex-seq-pairs)
            {a-object :object a-type :type} (stream-presentation a-source)
            {b-object :object b-type :type} (stream-presentation b-source)
            object-pair [a-object b-object]
            type-pair [a-type b-type]]
        (fresh-line! output-stream)
        (loop [interval-remains (seq interval-pairs)
               exterval-remains (seq exterval-pairs)
               in-seq-remains (seq sequence-pairs)
               ex-seq-remains (seq ex-seq-pairs)
               interval-index 0]
          (when (seq in-seq-remains)
            (let [interval-index (inc interval-index)
                  in-seq-pair (first in-seq-remains)
                  ex-seq-pair (first ex-seq-remains)
                  interval-pair (first interval-remains)
                  exterval-pair (first exterval-remains)
                  diagnostic (diagnose-pair differ-p in-seq-pair)]
              (write-pair-in-series in-seq-pair ex-seq-pair interval-pair exterval-pair
                                    interval-index diagnostic
                                    header-type-pair source-pair name-pair output-stream
                                    (when (some? width) width) object-pair type-pair)
              (recur (next interval-remains) (next exterval-remains)
                     (next in-seq-remains) (next ex-seq-remains)
                     interval-index))))
        ;; Finish line
        (write-pair-in-series nil ['() '()] final-pair final-pair
                              (inc (count sequence-pairs)) :finish
                              nil source-pair name-pair output-stream
                              (when (some? width) width) object-pair type-pair)))
    (boolean (seq interval-pairs))))
