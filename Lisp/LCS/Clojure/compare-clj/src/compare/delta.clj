(ns compare.delta
  "The \"Delta Compare Formatter\".

  Port of `compare-delta.lisp`.  The XP pretty-printer is dropped (the portable
  `#-:xp` branch is just `prin1`, rendered with Clojure `pr`)."
  (:require [compare.core :refer [nonep *default-hash-version*]]
            [compare.face :refer [basic-compare-sequences]]
            [compare.io :refer [stream-path-pair hash-records]]))

(defn cons-pair-as-edit
  "Convert a matched record pair into an edit.  (CL `CONS-PAIR-AS-EDIT`.)"
  [differ-p sequence-pair & [interval-pair interval-index hash-version]]
  (let [hash-version (or hash-version *default-hash-version*)
        a-interval (first interval-pair)
        a-records (first sequence-pair)
        b-records (second sequence-pair)]
    (concat
     [(if differ-p :delta-edit :merge-edit)
      (apply list b-records)]
     (when a-interval
       (let [a-start (first a-interval) a-end (second a-interval)]
         (when a-start
           (concat [:start a-start]
                   (when (and a-end (<= a-start a-end)) [:end a-end])))))
     (when-not (nonep a-records)
       [:hash (:hash (hash-records a-records :version hash-version))])
     (when interval-index [:index interval-index]))))

(defn cons-delta
  "Convert a list of edit forms into a delta.  (CL `CONS-DELTA`.)"
  [differ-p a-sequence b-sequence edit-forms & [path-pair hash-version]]
  (let [hash-version (or hash-version *default-hash-version*)
        [a-path b-path] path-pair
        {:keys [hash version]} (hash-records b-sequence :version hash-version)]
    (concat
     [(if differ-p :delta-file :merge-file)
      edit-forms]
     (when a-path [:input-file a-path])
     (when b-path [:output-file b-path])
     (when (and differ-p b-sequence) [:output-hash hash])
     [:hash-version version])))

(defn print-delta
  "Print the Delta prettily, as would be pleased.  (CL `PRINT-DELTA`.)"
  [delta & [output-stream]]
  (binding [*out* (or output-stream *out*)]
    (pr delta)))

(defn compare-as-delta
  "Perform a Formatted Compare in Delta format.  (CL `COMPARE-AS-DELTA`.)"
  [differ-p a-sequence b-sequence
   & {:keys [source-pair output-stream hash-version]
      :or {output-stream *out* hash-version *default-hash-version*}
      :as opts}]
  (let [{:keys [sequence-pairs interval-pairs length-pair]}
        (apply basic-compare-sequences differ-p a-sequence b-sequence (mapcat identity opts))
        path-pair (stream-path-pair source-pair)
        edit-forms
        (reduce (fn [acc [sequence-pair interval-pair]]
                  (let [interval-index (inc (count acc))]
                    (conj acc (cons-pair-as-edit differ-p sequence-pair
                                                 interval-pair interval-index
                                                 hash-version))))
                []
                (map vector sequence-pairs interval-pairs))
        delta-form (cons-delta differ-p a-sequence b-sequence edit-forms
                               path-pair hash-version)]
    (print-delta delta-form output-stream)
    (boolean (seq interval-pairs))))
