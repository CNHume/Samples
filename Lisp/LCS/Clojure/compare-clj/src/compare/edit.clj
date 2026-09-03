(ns compare.edit
  "Automated Source Control \"Delta\" Applicator.

  Port of `compare-edit.lisp`.  The destructive `nsplice` is replaced by a
  functional vector splice: edits are non-overlapping and ascending, so each
  `[:delta-edit records :start s :end e :hash h :index i]` replaces the
  original records in `[s, e)` with `records`."
  (:require [clojure.java.io :as jio]
            [compare.core :refer [invoke *record-reader* *edit-record-writer*]]
            [compare.io :refer [hash-records]]))

(defn sequence-index-error-f
  "Generate a Range Error signaller, for Sequences."
  [sequence index]
  (throw (ex-info (str "The index " index " (" (pr-str index)
                       ") is not in range for the sequence " (pr-str sequence) ".")
                  {:sequence sequence :index index})))

(defn- warn! [msg]
  (binding [*out* *err*] (println msg)))

(defn edit-stream
  "Apply each Edit to the Input Stream, producing the Output Stream.
  (CL `EDIT-STREAM`.)"
  [delta-edits input-stream output-stream output-hash hash-version]
  (let [original (vec (invoke *record-reader* input-stream))
        n (count original)]
    (loop [pos 0
           result []
           new-index 0
           edits (seq delta-edits)]
      (if (empty? edits)
        (let [result-hash (when output-hash (:hash (hash-records result :version hash-version)))]
          (when (and output-hash (not= result-hash output-hash))
            (warn! (format "File result hash (%d) different than expected (%d)."
                           result-hash output-hash)))
          (invoke *edit-record-writer* result output-stream))
        (let [[_tag inserted & kvs] (first edits)
              {:keys [start end hash index]} (apply hash-map kvs)
              start (or start 0)
              end (or end start)
              _ (when (and end (neg? end)) (sequence-index-error-f original end))
              _ (when (or (neg? start) (and end (> start end)))
                  (sequence-index-error-f original start))
              new-index (inc new-index)
              new-index (if (and index (not= new-index index))
                          (do (warn! (format "Recorded edit index (%d) departs from count (%d)."
                                             index new-index))
                              index)
                          new-index)
              new-hash (when hash
                         (:hash (hash-records (subvec original start (min end n))
                                              :version hash-version)))]
          (if (or (nil? hash) (= new-hash hash))
            (recur end
                   (into result (concat (subvec original pos start) inserted))
                   new-index
                   (next edits))
            (do (warn! (format "Deleted record hash (%d) different than expected (%d)."
                               new-hash hash))
                (recur pos result new-index (next edits)))))))))

(defn edit-file
  "Apply each Edit to the Input File, producing the Output File.
  (CL `EDIT-FILE`.)"
  [delta-edits & {:keys [input-file output-file output-hash hash-version]}]
  (with-open [input-stream (jio/reader input-file)]
    (if output-file
      (with-open [output-stream (jio/writer output-file)]
        (edit-stream delta-edits input-stream output-stream output-hash hash-version))
      (edit-stream delta-edits input-stream *out* output-hash hash-version)))
  output-file)
