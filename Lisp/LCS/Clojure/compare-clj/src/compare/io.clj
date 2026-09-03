(ns compare.io
  "I/O support: hashing, record reading/writing, and source descriptors.

  Port of `compare-io.lisp` (the portable `#-symbolics` build).  Output is
  written to a `java.io.Writer` (`*out*` by default); the Genera-specific
  presentation/editor-pointer paths are dropped."
  (:require [clojure.java.io :as jio]
            [compare.core :refer [truncated? nonep sequence-type-of
                                  *default-width* *default-hash-version*]]
            [compare.format :refer [frame-records]]))

;; ---------------------------------------------------------------------------
;; Low-level stream helpers
;; ---------------------------------------------------------------------------

(defn write-string!
  "Write (a substring of) `s` to `w`."
  [^CharSequence s ^java.io.Writer w & {:keys [start end]}]
  (.write w (.subSequence ^CharSequence s (int (or start 0)) (int (or end (.length s)))))
  nil)

(defn write-line!
  "Write (a substring of) `s` followed by a newline to `w`."
  [^CharSequence s ^java.io.Writer w & {:keys [start end]}]
  (write-string! s w :start start :end end)
  (.write w "\n")
  nil)

(defn fresh-line!
  "Ensure the last character written was a newline (CL `fresh-line`)."
  [^java.io.Writer w]
  ;; Approximate: a plain Writer does not track its own column; callers open
  ;; the stream fresh, so we simply write a newline to start cleanly.
  (.write w "\n")
  nil)

;; ---------------------------------------------------------------------------
;; Source descriptors (stream-name / stream-path / stream-presentation / width)
;; ---------------------------------------------------------------------------

(defn stream-name
  "Return something like a TRUENAME associated with the source."
  [source]
  (when source
    (if (instance? java.io.File source)
      (try (.getCanonicalPath ^java.io.File source)
           (catch Exception _ (str source)))
      (str source))))

(defn stream-path
  "Return any PATHNAME associated with the source specification."
  [source]
  (when source
    (if (instance? java.io.File source) source (jio/file (str source)))))

(defn stream-name-pair [source-pair]
  (let [[a b] source-pair] [(stream-name a) (stream-name b)]))

(defn stream-path-pair [source-pair]
  (let [[a b] source-pair] [(stream-path a) (stream-path b)]))

(defn stream-presentation
  "Return a Presentation Object (and Type) for the source specification.
  Returns `{:object o :type t}`."
  [source]
  (when source {:object source :type (class source)}))

(defn stream-width
  "Return the width of a stream, in characters (portable default)."
  [_stream]
  *default-width*)

;; ---------------------------------------------------------------------------
;; Hashing
;; ---------------------------------------------------------------------------

(def ^:private char-offsets
  "Special-character offsets for HASH-STRING (CL `char-code` of the case arms)."
  {\backspace 8 \tab 9 \newline 10 \formfeed 12 \return 13
   \u007f 127 \u0000 0 \u001b 27})

(defn hash-string
  "Form the system independent Hash Code for a string, in its context.

  Returns `{:hash h :version v}`.  The `#-:ccl` decomposition of
  `(mod (+ (* hash 257) offset) (2^31 - 1))` is expressed directly (JVM longs
  cannot overflow).  (CL `HASH-STRING`.)"
  [string & [context version]]
  (let [context (or context 0)
        modulus (- (bit-shift-left 1 31) 1)]
    (if (and version (not= version *default-hash-version*))
      (throw (ex-info (str "Hash Version " version " is not supported.")
                      {:version version}))
      (let [version (or version *default-hash-version*)
            s (str string)]
        (loop [hash (mod context modulus) i 0]
          (if (>= i (count s))
            {:hash hash :version version}
            (let [c (.charAt ^String s i)
                  offset (get char-offsets c (int c))]
              (recur (mod (+ (* hash 257) offset) modulus) (inc i)))))))))

(defn hash-records
  "Form the system independent Hash Code for some records, in their context.

  Returns `{:hash h :version v}`.  (CL `HASH-RECORDS`.)"
  [records & {:keys [context version start end] :or {context 0 start 0}}]
  (let [version (if version
                  (if (not= version *default-hash-version*)
                    (do (println (format "WARNING: Hash Version %d is not supported." version))
                        *default-hash-version*)
                    version)
                  *default-hash-version*)
        end (or end (count records))
        newline "\r"]
    (loop [i start hash context]
      (if (>= i end)
        {:hash hash :version version}
        (let [record (nth records i)
              truncated (truncated? record)
              line (if truncated (second record) record)
              hash (:hash (hash-string line hash version))
              hash (if truncated
                     hash
                     (:hash (hash-string newline hash version)))]
          (recur (inc i) hash))))))

;; ---------------------------------------------------------------------------
;; Record writing
;; ---------------------------------------------------------------------------

(defn write-record-as-object
  "Present the specified record as some object (the object/type are ignored
  portably).  Writes the change-bar (if any) then the record, with a newline
  unless truncating.  (CL `WRITE-RECORD-AS-OBJECT`.)"
  [record output-stream & {:keys [start end change-bar truncate]}]
  (let [start-sp? (some? start)
        end-sp? (some? end)
        change-bar-sp? (some? change-bar)
        bottom-end (when end-sp? (max end 0))
        change-string (str (or change-bar ""))
        change-length (count change-string)
        change-end (when (and change-bar-sp? end-sp?) (min bottom-end change-length))
        truncated (truncated? record)
        line (if truncated (second record) record)
        s (str line)
        s-length (count s)
        record-end (when end-sp? (if change-bar-sp? (- bottom-end change-end) bottom-end))
        sub-start (if (and start-sp? (pos? start)) start 0)
        sub-end (if (and end-sp? (< record-end s-length)) record-end s-length)]
    (when change-bar-sp?
      (write-string! change-string output-stream :start 0 :end (or change-end change-length)))
    (if (or truncate truncated)
      (write-string! s output-stream :start sub-start :end sub-end)
      (write-line! s output-stream :start sub-start :end sub-end))))

(defn write-records
  "Write the sequence of records to an output stream.
  Returns true when any records were written.  (CL `WRITE-RECORDS`.)"
  [records output-stream & opts]
  (doseq [record records]
    (apply write-record-as-object record output-stream opts))
  (not (nonep records)))

(defn write-frame
  "Frame and then write the sequence of records to an output stream."
  [records output-stream source interval & {:as opts}]
  (when-not (nonep records)
    (let [flat (mapcat identity opts)]
      (apply write-records (apply frame-records records flat)
             output-stream source interval flat))))

;; ---------------------------------------------------------------------------
;; Record reading
;; ---------------------------------------------------------------------------

(defn- read-line-truncated
  "Read one line from a PushbackReader.  Returns `[line truncated?]` or nil at
  EOF, where `truncated?` is true when the final line had no trailing newline."
  [^java.io.PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (.read rdr)]
        (cond
          (= c -1)
          (if (zero? (.length sb)) nil [(str sb) true])

          (= c 10)
          [(str sb) false]                       ; LF terminator

          (= c 13)                               ; CR: optionally followed by LF
          (let [c2 (.read rdr)]
            (if (and (not= c2 -1) (not= c2 10)) (.unread rdr c2))
            [(str sb) false])

          :else
          (do (.append sb (char c)) (recur)))))))

(defn read-records
  "Read the given stream into a list of records and return them.

  The final record (before EOF) that is not terminated by a newline is marked
  `[:truncated \"line\"]`.  (CL `READ-RECORDS`.)"
  ([] (read-records *in*))
  ([input-stream]
   (let [rdr (java.io.PushbackReader. (jio/reader input-stream))]
     (loop [acc []]
       (if-let [[line truncated?] (read-line-truncated rdr)]
         (recur (conj acc (if truncated? [:truncated line] line)))
         acc)))))
