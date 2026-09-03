(ns compare.merge
  "The \"Merge Compare Formatter\".

  Port of `compare-merge.lisp` (portable build; the Genera/ZWEI merge-record
  marks are dropped)."
  (:require [compare.core :refer [nonep string-capitalize]]
            [compare.face :refer [basic-compare-sequences]]
            [compare.interval :refer [complement-intervals subseq-intervals interval-led?]]
            [compare.io :refer [stream-name-pair stream-presentation stream-width
                                write-frame write-record-as-object write-string!
                                write-line! fresh-line!]]
            [compare.format :refer [diagnose-pair interval-to-string center-values
                                    label-string pure-less pure-greater]]))

(defn write-half-head
  "Write a head for the next half (of a pair), to the specified stream."
  [sequence-index interval-pair interval-index name-pair diagnostic
   output-stream width object type]
  (let [[interval name pure-fill]
        (if (zero? sequence-index)
          [(first interval-pair) (first name-pair) pure-less]
          [(second interval-pair) (second name-pair) pure-greater])
        label (label-string interval-index
                            (when diagnostic (string-capitalize (name diagnostic)))
                            (interval-to-string interval)
                            name)
        label-length (count label)
        head-width (if (some? width) width (stream-width output-stream))
        {:keys [fill-start fill-end crop-start crop-end]}
        (center-values label-length head-width)
        fill-finish (- head-width fill-end)
        fill-maximum (max fill-start fill-finish)
        fill (pure-fill fill-maximum)]
    (write-string! fill output-stream :end fill-start)
    (write-record-as-object label output-stream object type
                            :truncate true
                            :start (when (pos? crop-start) crop-start)
                            :end (when (< crop-end label-length) crop-end))
    (write-line! fill output-stream :end fill-finish)))

(defn write-half-foot
  "Write a foot for the next half (of a pair), to the specified stream."
  [sequence-index interval-pair interval-index name-pair diagnostic
   output-stream width object type]
  (write-half-head sequence-index nil interval-index nil diagnostic
                   output-stream width object type))

(defn write-pair-and-merge
  "Format a matched record pair vertically, and merge it with the later file."
  [differ-p sequence-pair interval-pair interval-index
   source-pair name-pair output-stream width object-pair type-pair]
  (let [diagnostic (diagnose-pair differ-p sequence-pair)
        a-index 0
        b-index 1
        a-source (first source-pair)
        b-source (second source-pair)
        a-object (first object-pair)
        b-object (second object-pair)
        a-type (first type-pair)
        b-type (second type-pair)
        a-records (first sequence-pair)
        b-records (second sequence-pair)
        head-width (if (some? width) width (stream-width output-stream))
        width-keys (when (some? width) [:width width])]
    (when differ-p
      (write-half-head a-index interval-pair interval-index name-pair diagnostic
                       output-stream head-width a-object a-type)
      (when-not (nonep a-records)
        (apply write-frame a-records output-stream a-source (first interval-pair)
               width-keys))
      (write-half-foot a-index interval-pair interval-index name-pair diagnostic
                       output-stream head-width a-object a-type)
      (write-half-head b-index interval-pair interval-index name-pair diagnostic
                       output-stream head-width b-object b-type))
    (when-not (nonep b-records)
      (apply write-frame b-records output-stream b-source (second interval-pair)
             width-keys))
    (when differ-p
      (write-half-foot b-index interval-pair interval-index name-pair diagnostic
                       output-stream head-width b-object b-type))
    nil))

(defn compare-and-merge
  "Perform a Formatted Compare and Merge.  (CL `COMPARE-AND-MERGE`.)"
  [differ-p a-sequence b-sequence
   & {:keys [source-pair output-stream width] :or {output-stream *out*} :as opts}]
  (let [{:keys [sequence-pairs interval-pairs length-pair]}
        (apply basic-compare-sequences differ-p a-sequence b-sequence (mapcat identity opts))
        {:keys [intervals]} (complement-intervals interval-pairs length-pair)
        ex-seq-pairs (subseq-intervals a-sequence b-sequence intervals)]
    (when (or (seq interval-pairs) (seq intervals))
      (let [name-pair (stream-name-pair source-pair)
            a-source (first source-pair)
            b-source (second source-pair)
            first-interval-pair (first interval-pairs)
            first-exterval-pair (first intervals)
            interval-led (interval-led? first-interval-pair first-exterval-pair)
            width-keys (when (some? width) [:width width])
            {a-object :object a-type :type} (stream-presentation a-source)
            {b-object :object b-type :type} (stream-presentation b-source)
            object-pair [a-object b-object]
            type-pair [a-type b-type]]
        (fresh-line! output-stream)
        (loop [interval-remains (seq interval-pairs)
               exterval-remains (seq intervals)
               in-seq-remains (seq sequence-pairs)
               ex-seq-remains (seq ex-seq-pairs)
               interval-index 0]
          (if (and (empty? in-seq-remains) (empty? ex-seq-remains))
            nil
            (let [interval-index (inc interval-index)
                  in-seq-pair (first in-seq-remains)
                  ex-seq-pair (first ex-seq-remains)
                  interval-pair (first interval-remains)
                  exterval-pair (first exterval-remains)]
              (doseq [this-is-differ-p [interval-led (not interval-led)]]
                (let [[this-differ-p this-interval-pair this-seq-pair]
                      (if this-is-differ-p
                        [differ-p interval-pair in-seq-pair]
                        [(not differ-p) exterval-pair ex-seq-pair])]
                  (when this-interval-pair
                    (write-pair-and-merge this-differ-p this-seq-pair
                                          this-interval-pair interval-index
                                          source-pair name-pair output-stream
                                          (when (some? width) width)
                                          object-pair type-pair))))
              (recur (next interval-remains) (next exterval-remains)
                     (next in-seq-remains) (next ex-seq-remains)
                     interval-index))))))
    (boolean (seq interval-pairs))))
