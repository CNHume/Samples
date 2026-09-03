(ns compare.parallel
  "The \"Parallel Compare Formatter\".

  Port of `compare-parallel.lisp` (portable build)."
  (:require [compare.core :refer [*maximum-default-parallel-width*]]
            [compare.face :refer [basic-compare-sequences]]
            [compare.io :refer [stream-name-pair stream-presentation stream-width
                                write-record-as-object write-string! write-line! fresh-line!]]
            [compare.format :refer [interval-to-string center-values label-string
                                    frame-records pure-minus pure-space pure-equal]]))

(defn write-side-head
  "Write a head for the next side, to the specified stream."
  [sequence-index interval-pair interval-index name-pair output-stream width object-pair type-pair]
  (let [[interval name object type]
        (mapv (if (zero? sequence-index) first second)
              [interval-pair name-pair object-pair type-pair])
        side-label (label-string (interval-to-string interval) name)
        side-label-length (count side-label)
        side-width (if (some? width) width (stream-width output-stream))
        side-start (first interval)
        side-end (second interval)
        minimum-keys (when (nil? interval) [:fill-minimum 1])
        pure-fill (cond
                    (nil? interval) pure-space
                    (< side-start side-end) pure-minus
                    :else pure-equal)]
    (let [{:keys [fill-start fill-end crop-start crop-end]}
          (apply center-values side-label-length side-width minimum-keys)
          fill-finish (- side-width fill-end)
          fill-maximum (max fill-start fill-finish)
          fill (pure-fill fill-maximum)
          crop-keys (concat (when (pos? crop-start) [:start crop-start])
                            (when (< crop-end side-label-length) [:end crop-end]))]
      (write-string! fill output-stream :end fill-start)
      (apply write-record-as-object side-label output-stream object type :truncate true crop-keys)
      ((if (zero? sequence-index) write-string! write-line!)
       fill output-stream :end fill-finish))))

(defn write-side-head-pair
  "Write a pair of interval headers, side by side, to the specified stream."
  [interval-pair interval-index name-pair divider output-stream width object-pair type-pair]
  (let [full-width (if (some? width)
                     width
                     (min (stream-width output-stream) *maximum-default-parallel-width*))
        side-width (quot (dec full-width) 2)]
    (write-side-head 0 interval-pair interval-index name-pair output-stream
                     side-width object-pair type-pair)
    (when (pos? full-width) (write-string! divider output-stream))
    (write-side-head 1 interval-pair interval-index name-pair output-stream
                     side-width object-pair type-pair)))

(defn write-record-pair
  "Present the pair of records, as the specified pair of objects (and types.)"
  [record-pair divider output-stream width object-pair type-pair]
  (let [[a-record b-record] record-pair
        [a-object b-object] object-pair
        [a-type b-type] type-pair
        full-width (if (some? width)
                     width
                     (min (stream-width output-stream) *maximum-default-parallel-width*))
        side-width (quot (dec full-width) 2)
        side-keys [:end side-width]
        a-width (if a-record
                  (let [a-string (str a-record)
                        a-length (count a-string)
                        end-keys (concat [:truncate true]
                                         (when (> a-length side-width) side-keys))]
                    (apply write-record-as-object a-string output-stream a-object a-type end-keys)
                    (min a-length side-width))
                  0)
        c-width (- side-width a-width)]
    (when (pos? c-width)
      (write-string! (pure-space c-width) output-stream :end c-width))
    (cond
      b-record
      (do (when (pos? full-width) (write-string! divider output-stream))
          (let [b-string (str b-record)
                b-length (count b-string)
                end-keys (when (> b-length side-width) side-keys)]
            (apply write-record-as-object b-string output-stream b-object b-type end-keys)))

      (pos? full-width)
      (write-line! divider output-stream)

      :else
      (write-line! "" output-stream))))

(defn write-pair-in-parallel
  "Format a matched record pair horizontally and send it to the output stream."
  [sequence-pair interval-pair interval-index finishing
   source-pair output-stream width object-pair type-pair]
  (let [[a-records b-records] sequence-pair
        full-width (if (some? width)
                     width
                     (min (stream-width output-stream) *maximum-default-parallel-width*))
        side-width (quot (dec full-width) 2)
        cross "+"
        foot (pure-minus side-width)
        a-length (count a-records)
        b-length (count b-records)
        length-max (max a-length b-length)
        line-limit (if (and finishing (pos? length-max)) (inc length-max) length-max)
        a-frames (frame-records a-records :width side-width)
        b-frames (frame-records b-records :width side-width)]
    (write-side-head-pair interval-pair interval-index nil cross output-stream
                          full-width object-pair type-pair)
    (dotimes [line-count line-limit]
      (let [a-ending (= line-count a-length)
            b-ending (= line-count b-length)
            a-footed (and a-ending (pos? a-length))
            b-footed (and b-ending (pos? b-length))
            a-element (when (< line-count a-length) (nth a-frames line-count))
            b-element (when (< line-count b-length) (nth b-frames line-count))
            a-framer (if a-footed foot a-element)
            b-framer (if b-footed foot b-element)
            record-pair [a-framer b-framer]
            divider (if (or a-footed b-footed) "+" "|")]
        (write-record-pair record-pair divider output-stream full-width object-pair type-pair)))
    nil))

(defn compare-in-parallel
  "Perform a Formatted Compare in Parallel.  (CL `COMPARE-IN-PARALLEL`.)"
  [differ-p a-sequence b-sequence
   & {:keys [source-pair output-stream width] :or {output-stream *out*} :as opts}]
  (let [{:keys [sequence-pairs interval-pairs]}
        (apply basic-compare-sequences differ-p a-sequence b-sequence (mapcat identity opts))]
    (when (seq interval-pairs)
      (let [name-pair (stream-name-pair source-pair)
            a-source (first source-pair)
            b-source (second source-pair)
            full-width (if (some? width)
                         width
                         (min (stream-width output-stream) *maximum-default-parallel-width*))
            {a-object :object a-type :type} (stream-presentation a-source)
            {b-object :object b-type :type} (stream-presentation b-source)
            object-pair [a-object b-object]
            type-pair [a-type b-type]]
        (fresh-line! output-stream)
        (write-side-head-pair nil nil name-pair " " output-stream full-width object-pair type-pair)
        (loop [interval-remains (seq interval-pairs)
               sequence-remains (seq sequence-pairs)
               interval-index 0]
          (when (seq sequence-remains)
            (let [interval-index (inc interval-index)
                  sequence-pair (first sequence-remains)
                  interval-pair (first interval-remains)
                  finishing (empty? (next sequence-remains))]
              (write-pair-in-parallel sequence-pair interval-pair interval-index finishing
                                      source-pair output-stream full-width object-pair type-pair)
              (recur (next interval-remains) (next sequence-remains) interval-index))))))
    (boolean (seq interval-pairs))))
