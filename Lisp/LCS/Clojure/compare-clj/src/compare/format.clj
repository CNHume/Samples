(ns compare.format
  "Formatting support for the various output formatters.

  Port of `compare-format.lisp`.  The CL `format` directive strings are
  implemented directly (see `interval-to-string` and the label helpers) rather
  than via a lenient SCL `format`."
  (:refer-clojure :exclude [subseq])
  (:require [compare.core :refer [truncated? nonep sequence-type-of coerce-record
                                  subseq invoke *compare-framer* *default-compare-format*
                                  *default-tab-spaces* *default-width*
                                  *label-format* *interval-format*]]))

;; ---------------------------------------------------------------------------
;; Pure (memoized) fill strings
;; ---------------------------------------------------------------------------

(def ^:private pure-cache (atom {}))

(defn pure-string
  "Get or Make a Read Only string containing the specified character."
  [minimum-length ch]
  (swap! pure-cache
         (fn [m]
           (let [existing (get m ch)
                 current (if (string? existing) (count existing) 0)]
             (if (> minimum-length current)
               (assoc m ch (apply str (repeat (max minimum-length *default-width*) ch)))
               m))))
  (get @pure-cache ch))

(defn pure-less [minimum-length] (pure-string minimum-length \<))
(defn pure-greater [minimum-length] (pure-string minimum-length \>))
(defn pure-equal [minimum-length] (pure-string minimum-length \=))
(defn pure-minus [minimum-length] (pure-string minimum-length \-))
(defn pure-space [minimum-length] (pure-string minimum-length \space))

;; ---------------------------------------------------------------------------
;; Formatter dispatch
;; ---------------------------------------------------------------------------

(defn compare-formatter
  "Dispatch to the appropriate Compare Formatter.  (CL `COMPARE-FORMATTER`.)"
  [differ-p a-sequence b-sequence & {:keys [format width]
                                     :or {format *default-compare-format*}
                                     :as opts}]
  (let [selected-formatter (case format
                             :delta (requiring-resolve 'compare.delta/compare-as-delta)
                             :merge (requiring-resolve 'compare.merge/compare-and-merge)
                             :parallel (requiring-resolve 'compare.parallel/compare-in-parallel)
                             :review (requiring-resolve 'compare.review/compare-in-review)
                             :series (requiring-resolve 'compare.series/compare-in-series)
                             (throw (ex-info (str "No such Compare Format: " (pr-str format))
                                             {:format format})))
        width-sp? (some? width)
        formatter-keys
        (if (and width-sp? (or (not (integer? width)) (neg? width)))
          (do (binding [*out* *err*]
                (println (format "The :width keyword specifies a %s value: %s."
                                 (if (integer? width) "negative" "non-integral")
                                 (pr-str width))))
              (dissoc opts :width))
          opts)]
    (apply selected-formatter differ-p a-sequence b-sequence
           (mapcat identity formatter-keys))))

;; ---------------------------------------------------------------------------
;; Header detection (Series format)
;; ---------------------------------------------------------------------------

(defn- typep
  "A small stand-in for CL `typep` over char header types."
  [object type]
  (cond
    (nil? type) false
    (fn? type) (boolean (type object))
    (set? type) (contains? type object)
    (class? type) (instance? type object)
    (coll? type) (boolean (some #(= object %) type))
    :else (= object type)))

(defn visible?
  "Is this record visible?  (CL `VISIBLE-P`.)"
  [header-type record]
  (or (typep \space header-type)
      (let [line (if (truncated? record) (second record) record)]
        (pos? (count line)))))

(defn header?
  "Does this record provide interesting context?  (CL `HEADER-P`.)"
  [header-type record]
  (let [line (if (truncated? record) (second record) record)]
    (when (pos? (count line))
      (typep (first line) header-type))))

(defn initial-header
  "Determine whether each subsequence presents its own headers.
  Returns `[a-header-p b-header-p]`.  (CL `INITIAL-HEADER`.)"
  [header-type-pair sequence-pair]
  (let [[a-header-type b-header-type] header-type-pair
        [a-records b-records] sequence-pair
        a-visible (some #(when (visible? a-header-type %) %) a-records)
        b-visible (some #(when (visible? b-header-type %) %) b-records)
        a-header-p (when a-visible (header? a-header-type a-visible))
        b-header-p (when b-visible (header? b-header-type b-visible))]
    [(if (nonep a-records) b-header-p a-header-p)
     (if (nonep b-records) a-header-p b-header-p)]))

(defn- last-index-of
  "The index of the last element of `coll` satisfying `pred`, or nil."
  [coll pred]
  (reduce-kv (fn [acc i x] (if (pred x) i acc)) nil (vec coll)))

(defn find-header
  "Return the most recent header(s) from both sides.
  Returns `{:headers [a-headers b-headers] :headervals [a-headerval b-headerval]}`."
  [header-type-pair sequence-pair interval-pair]
  (let [[a-context b-context] sequence-pair
        [a-interval b-interval] interval-pair
        a-offset (last-index-of a-context #(header? (first header-type-pair) %))
        b-offset (last-index-of b-context #(header? (second header-type-pair) %))
        [a-headerval a-headers]
        (if a-offset
          (let [a-position (+ (first a-interval) a-offset)]
            [[a-position (inc a-position)]
             (list (subseq a-context a-offset (inc a-offset)))])
          [nil '()])
        [b-headerval b-headers]
        (if b-offset
          (let [b-position (+ (first b-interval) b-offset)]
            [[b-position (inc b-position)]
             (list (subseq b-context b-offset (inc b-offset)))])
          [nil '()])]
    {:headers [a-headers b-headers]
     :headervals [a-headerval b-headerval]}))

;; ---------------------------------------------------------------------------
;; Diagnostics
;; ---------------------------------------------------------------------------

(defn diagnose-pair
  "Diagnose this pair, assuming a chronological sense.  (CL `DIAGNOSE-PAIR`.)"
  [differ-p sequence-pair]
  (let [a-nonep (nonep (first sequence-pair))
        b-nonep (nonep (second sequence-pair))]
    (if differ-p
      (if a-nonep
        (if b-nonep :voided :insert)
        (if b-nonep :delete :modify))
      :common)))

(defn interval-to-string
  "Convert an interval into a string.  (CL `INTERVAL-TO-STRING`.)"
  [interval]
  (when interval
    (let [[start end] interval]
      (str "[" start (when (< start end) (str ":" (dec end))) "]"))))

(defn label-string
  "Render the CL `*LABEL-FORMAT*` `\" ~@{~@[~D ~]~}\"`: a leading space, then
  each non-nil argument followed by a space."
  [& args]
  (str " " (apply str (for [a args :when a] (str a " ")))))

;; ---------------------------------------------------------------------------
;; Centering
;; ---------------------------------------------------------------------------

(defn center-values
  "Return the centering split points.  (CL `CENTER-VALUES`.)"
  [center-length fill-count & {:keys [fill-minimum right-crop] :or {fill-minimum 2}}]
  (let [fill-limit (min fill-count fill-minimum)
        center-limit (- fill-count fill-limit)
        center-width (min center-limit center-length)
        center-extra (- center-limit center-width)
        [left-minimum left-extra crop-start crop-end]
        (if right-crop
          [(quot fill-limit 2) (quot (inc center-extra) 2) 0 center-width]
          [(quot (inc fill-limit) 2) (quot center-extra 2)
           (- center-length center-width) center-length])
        fill-start (+ left-minimum left-extra)
        fill-end (+ fill-start center-width)]
    {:fill-start fill-start :fill-end fill-end
     :crop-start crop-start :crop-end crop-end}))

(defn center-fill
  "Return the sequence centered within a filled field, as a string.
  Returns `{:string s :start st :end en}`.  (CL `CENTER-FILL`.)"
  [center-sequence fill-count & {:keys [fill-item] :or {fill-item \space} :as keys}]
  (let [center-string (apply str center-sequence)
        center-length (count center-string)
        fill (apply str (repeat fill-count fill-item))
        {:keys [fill-start fill-end crop-start crop-end]}
        (center-values center-length fill-count keys)
        s (str (subs fill 0 fill-start)
               (subs center-string crop-start crop-end)
               (subs fill fill-end))]
    {:string s :start fill-start :end fill-end}))

;; ---------------------------------------------------------------------------
;; Tab expansion / framing
;; ---------------------------------------------------------------------------

(defn expand-tabs
  "Expand Tabs into Spaces, according to Tab Stops, for the Tabbed String."
  [tabbed-string & {:keys [tab-spaces width] :or {tab-spaces *default-tab-spaces*}}]
  (let [s (str tabbed-string)
        s-length (count s)
        width-sp? (some? width)]
    (loop [string-index 0
           column-index 0
           last-stop-remains (seq tab-spaces)
           last-stop-index 0
           sb nil
           tab-found false]
      (if (or (and width-sp? (>= column-index width)) (= string-index s-length))
        (if tab-found (str sb) s)
        (let [c (nth s string-index)]
          (if (= c \tab)
            (let [[last-stop-remains last-stop-index]
                  (loop [next-stop-remains last-stop-remains
                         next-stop-index last-stop-index]
                    (if (or (< column-index next-stop-index)
                            (and width-sp? (= next-stop-index width))
                            (empty? next-stop-remains))
                      [next-stop-remains next-stop-index]
                      (recur (next next-stop-remains)
                             (if width-sp?
                               (min (+ next-stop-index (first next-stop-remains)) width)
                               (+ next-stop-index (first next-stop-remains))))))
                  space-count (- last-stop-index column-index)
                  sb (or sb (StringBuilder. (subs s 0 string-index)))]
              (when (pos? space-count)
                (.append sb (apply str (repeat space-count \space))))
              (recur (inc string-index)
                     (+ column-index space-count)
                     last-stop-remains last-stop-index
                     sb true))
            (do (when sb (.append sb c))
                (recur (inc string-index) (inc column-index)
                       last-stop-remains last-stop-index sb tab-found))))))))

(defn frame-record
  "Expand Tabs for a record, within any specific width.  (CL `FRAME-RECORD`.)"
  [record & {:as opts}]
  (let [truncated (truncated? record)
        line (if truncated (second record) record)
        line-type (if (char? line) :char (sequence-type-of line))
        expansion (apply expand-tabs (str line) (mapcat identity opts))]
    (cond
      (instance? java.io.File line) (java.io.File. expansion)
      (symbol? line) (symbol expansion)
      (keyword? line) (keyword expansion)
      :else (coerce-record expansion line-type))))

(defn frame-records
  "Frame each record, within any specific width.  (CL `FRAME-RECORDS`.)"
  [records & {:keys [change-bar tab-spaces width]}]
  (let [change-bar-sp? (some? change-bar)
        tab-spaces-sp? (some? tab-spaces)
        width-sp? (some? width)]
    (cond
      (and change-bar-sp? width-sp?)
      (let [change-end (min width (count (str change-bar)))
            frame-keys (cond-> {:width (- width change-end)}
                         tab-spaces-sp? (assoc :tab-spaces tab-spaces))]
        (mapv #(apply invoke *compare-framer* % (mapcat identity frame-keys)) records))

      (or change-bar-sp? tab-spaces-sp? width-sp?)
      (let [frame-keys (cond-> {}
                         width-sp? (assoc :width width)
                         tab-spaces-sp? (assoc :tab-spaces tab-spaces))]
        (mapv #(apply invoke *compare-framer* % (mapcat identity frame-keys)) records))

      :else records)))
