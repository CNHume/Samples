(ns compare.review
  "The \"Review Compare Formatter\".

  Port of `compare-review.lisp` (portable build)."
  (:require [compare.core :refer [nonep string-capitalize]]
            [compare.face :refer [basic-compare-sequences]]
            [compare.interval :refer [complement-intervals subseq-intervals interval-led?]]
            [compare.io :refer [stream-name-pair stream-presentation stream-width
                                write-frame write-record-as-object write-string!
                                write-line! fresh-line!]]
            [compare.format :refer [diagnose-pair center-values center-fill
                                    label-string pure-string]]))

(defn write-review-head
  "Write a head for the file under review, to the specified stream."
  [name output-stream width object type]
  (let [label (label-string name)
        head-width (if (some? width) width (stream-width output-stream))
        {:keys [string]} (center-fill label head-width \-)
        n (count string)
        head (cond
               (zero? n) string
               (= n 1) "+"
               :else (str "+" (subs string 1 (dec n)) "+"))]
    (write-record-as-object head output-stream object type)))

(defn write-trace
  "Write a trace (for a record interval), to the specified stream."
  [diagnostic fill-char output-stream width object type]
  (let [trace (label-string (when diagnostic (string-capitalize (name diagnostic))))
        trace-length (count trace)
        head-width (if (some? width) width (stream-width output-stream))
        {:keys [fill-start fill-end crop-start crop-end]}
        (center-values trace-length head-width)
        fill-finish (- head-width fill-end)
        fill-maximum (max fill-start fill-finish)
        fill (pure-string fill-maximum fill-char)
        crop-keys (concat (when (pos? crop-start) [:start crop-start])
                          (when (< crop-end trace-length) [:end crop-end]))]
    (write-string! fill output-stream :end fill-start)
    (apply write-record-as-object trace output-stream object type :truncate true crop-keys)
    (write-line! fill output-stream :end fill-finish)))

(defn write-pair-in-review
  "Mark each output record appropriately and send it to the output stream."
  [differ-p sequence-pair interval-pair interval-index
   source-pair output-stream width]
  (let [diagnostic (diagnose-pair differ-p sequence-pair)
        b-records (second sequence-pair)
        change-bar (case diagnostic
                     :modify "|" :delete "<" :insert ">" :voided "#"
                     :common (if (nonep b-records) "=" " ")
                     "?")
        head-width (if (some? width) width (stream-width output-stream))
        width-keys (when (some? width) [:width width])
        change-keys (concat [:change-bar change-bar] width-keys)]
    (if (nonep b-records)
      (let [{a-object :object a-type :type} (stream-presentation (first source-pair))]
        (write-trace diagnostic (first change-bar) output-stream head-width a-object a-type))
      (apply write-frame b-records output-stream (second source-pair)
             (second interval-pair) change-keys))))

(defn compare-in-review
  "Perform a Formatted Compare in Review.  (CL `COMPARE-IN-REVIEW`.)"
  [differ-p a-sequence b-sequence
   & {:keys [source-pair output-stream width] :or {output-stream *out*} :as opts}]
  (let [{:keys [sequence-pairs interval-pairs length-pair]}
        (apply basic-compare-sequences differ-p a-sequence b-sequence (mapcat identity opts))
        {:keys [intervals]} (complement-intervals interval-pairs length-pair)
        ex-seq-pairs (subseq-intervals a-sequence b-sequence intervals)]
    (when (or (seq interval-pairs) (seq intervals))
      (let [first-interval-pair (first interval-pairs)
            first-exterval-pair (first intervals)
            interval-led (interval-led? first-interval-pair first-exterval-pair)
            name-pair (stream-name-pair source-pair)
            b-name (second name-pair)
            head-width (if (some? width) width (stream-width output-stream))
            {b-object :object b-type :type} (stream-presentation (second source-pair))]
        (fresh-line! output-stream)
        (write-review-head b-name output-stream head-width b-object b-type)
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
                    (write-pair-in-review this-differ-p this-seq-pair
                                          this-interval-pair interval-index
                                          source-pair output-stream
                                          (when (some? width) width)))))
              (recur (next interval-remains) (next exterval-remains)
                     (next in-seq-remains) (next ex-seq-remains)
                     interval-index))))))
    (boolean (seq interval-pairs))))
