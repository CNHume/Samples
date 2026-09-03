(ns compare.lab.exerciser
  "Exercise the Sequence Comparison Utility, for performance.

  Port of `lab/exerciser.lisp`."
  (:require [compare.face :refer [common-sequence]]
            [compare.test :refer [make-random-sequence]]))

(defn exerciser
  "Exercise the Sequence Comparison Utility, for performance."
  [alphabet-length & {:keys [randomize function]
                      :or {function common-sequence} :as opts}]
  (let [test-length (* alphabet-length alphabet-length)
        a-sequence (mapv (fn [index]
                           (if randomize
                             (rand-int alphabet-length)
                             (mod index alphabet-length)))
                         (range test-length))
        b-sequence (vec (reverse a-sequence))]
    (apply function a-sequence b-sequence (mapcat identity opts))))

(defn symmetry
  "Test the Sequence Comparison Utility, for performance asymmetry.
  Returns `[forward-common reverse-common]`."
  [& {:keys [alphabet-length improbability short-length type function]
      :or {alphabet-length 26 improbability 16 short-length 52
           type 'list function common-sequence} :as opts}]
  (let [long-length (* alphabet-length improbability short-length)
        short-sequence (make-random-sequence type short-length alphabet-length)
        long-sequence (make-random-sequence type long-length alphabet-length)
        forward-common (apply function short-sequence long-sequence
                              :time-log-entry true (mapcat identity opts))
        reverse-common (apply function long-sequence short-sequence
                              :time-log-entry true (mapcat identity opts))]
    [forward-common reverse-common]))
