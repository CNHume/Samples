;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;;
;;; Source: srccom-patch.lisp        Module: compare                Status:        operational
;;;
;;; Author     Version        Edit Date        Purpose of Edit
;;; ------     -------        ---------        ---------------
;;; Chris Hume         2.5         1-Feb-91        Renamed *SPACE-CHARACTERS*.
;;; Chris Hume         2.4        29-Jan-91        Adjusted Delta Format Output.
;;; Chris Hume         2.3        16-Jan-91        Provided for Compare Format Selection.
;;; Chris Hume         2.2         1-Jan-91        Eliminated extraneous File Line list.
;;; Chris Hume         2.1        14-Dec-90        Presented Delta Format Output Buffer.
;;; Chris Hume         2.0        11-Dec-90        Header Bags became Character Types.
;;; Chris Hume         1.24        10-Dec-90        Isolated the FILE-P interface.
;;; Chris Hume         1.23         6-Dec-90        Identified the HEADER-TYPE Interface.
;;; Chris Hume         1.22         3-Dec-90        Identified a STREAM-BUFFER Interface.
;;; Chris Hume         1.21        28-Nov-90        Improved "comments" for Delta Format.
;;; Chris Hume         1.20        14-Nov-90        Fixed Pathname Merge and File Creation.
;;; Chris Hume         1.19        18-Oct-90        Present Files as their indicated Type.
;;; Chris Hume         1.18        15-Oct-90        Renamed :STREAM-PAIR to :SOURCE-PAIR.
;;; Chris Hume         1.17         6-Oct-90        Added File Object and its "Methods".
;;; Chris Hume         1.16         3-Oct-90        Added the DEFVARs used from SRCCOM.
;;; Chris Hume         1.15        22-Sep-90        No *LINES-TO-PRINT-AFTER*, by default.
;;; Chris Hume         1.14        22-Sep-90        Now handle Mode via :HEADER-TYPE-PAIR.
;;; Chris Hume         1.13         9-Sep-90        Began Mode support for Series Format.
;;; Chris Hume         1.12        30-Jul-90        Pass "File Objects" via :SOURCE-PAIR.
;;; Chris Hume         1.11         3-Jul-90        Cleaned up the Merge Interfaces.
;;; Chris Hume         1.10         2-Jul-90        Added some remaining SRCCOM Interfaces.
;;; Chris Hume         1.9        29-Jun-90        Began work on (ZWEI) COMPARE-AND-MERGE.
;;; Chris Hume         1.8        29-Jun-90        Genera 8.0 tells of output to ZWEI.
;;; Chris Hume         1.7         6-Jun-90        Added Patch-File Attribute.
;;; Chris Hume         1.6        31-May-90       Reinstated Attribute Line.
;;; Chris Hume         1.5        29-May-90       Simplified affix defaulting.
;;; Chris Hume         1.4        21-May-90       Moved in "merge" interfaces.
;;; Chris Hume         1.3        20-May-90       Re-organized "affix" keywords.
;;; Chris Hume         1.2        24-Apr-90        Internalized stream width sizing.
;;; Chris Hume         1.1        30-Mar-90        Cleaned up keyword arguments.
;;; Chris Hume         1.0        26-Mar-90        Created file.
;;;
;;; Purpose:        Patch the Sequence Comparison Utility into SRCCOM.
;;;
;;; Usage:        This file assumes the Symbolics Common Lisp Environment.
;;;
;;; Compile:        (compile-file "compare:compare;srccom-patch")
;;;
;;; Contents:
;;;
;;;        This file supersedes the "Primary" SRCCOM Interface definitions.
;;;
;;; (Redefined) External Interfaces:
;;;
;;;        source-compare-files        a-file b-file
;;;                                &optional output-stream comment-p
;;;                                &key ignore-case-and-style ignore-whitespace
;;;                                affix format width
;;;
;;;        source-compare                a-filename b-filename
;;;                                &optional output-stream comment-p
;;;                                &key ignore-case-and-style ignore-whitespace
;;;                                deleted affix format width
;;;
;;;        prompted-source-compare        a-filename b-filename
;;;                                &key ignore-case-and-style ignore-whitespace
;;;                                affix format width
;;;
;;;        source-compare-automatic-merge-1
;;;                                a-file b-file output-stream
;;;                                &key ignore-case-and-style ignore-whitespace
;;;                                affix width
;;;
;;;        source-compare-automatic-merge-recording
;;;                                a-file b-file output-stream
;;;                                &key ignore-case-and-style ignore-whitespace
;;;                                affix width
;;;
;;;        source-compare-automatic-merge
;;;                                a-filename b-filename output-filename
;;;                                &key ignore-case-and-style ignore-whitespace
;;;                                affix width
;;;
;;; Local Interfaces:
;;;
;;;        create-file                 filename &key deleted
;;;
;;;        header-type                mode
;;;
;;;        get-file-line                file line-index
;;;
;;;        stream-buffer                buffer-stream
;;;

;;;
;;; Module Prologue:
;;;
;;; Stay in the Default Package.
(require 'compare)

;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Objects, Initializers, and Accessors:
;;;
(cl:defstruct (SRCCOM:FILE
                :array-leader (:make-array (:length 100.))
                (:conc-name nil)
                (:constructor MAKE-SRCCOM-FILE)
                (:copier nil))
  (file-length 0)                                ; The number of lines
  file-name                                        ; The name of the file
  (file-type "File")                                ; The class of file object
  file-stream                                        ; The Input Stream
  file-major-mode                                ; The Mode Symbol
  (file-bp-table nil)                                ; Try :READ-BP on each line
  (file-eof nil)                                ; End of file reached
  presentation-type                                ; Presentation Type of file
  presentation-object)                                ; Object implicitly presented

(defun SRCCOM:CREATE-FILE (filename &key deleted &aux stream mode)
  
  (setq stream (open filename :deleted deleted))

  (let ((generic-pathname (funcall filename :generic-pathname)))
    (fs:read-attribute-list generic-pathname stream)
    (setq mode (or (funcall generic-pathname :get :mode)
                   :lisp)))

  (make-srccom-file :file-stream stream
                    :file-name (funcall stream :truename)
                    :file-major-mode mode
                    :presentation-type 'fs:pathname
                    :presentation-object filename))

(defun SRCCOM:HEADER-TYPE (mode)
  "Return a header character type appropriate to the specified mode."
  (declare (special compare:*space-characters*))
  (case mode
    ((:lisp :ztop) '(member #\( #\;))
    (:bolio '(member #\.))
    (:text `(not (member . ,compare:*space-characters*)))
    (otherwise nil)
    ))

;;;
;;; The following "method" will obtain a line from the File, or the Stream:
;;;
(defun SRCCOM:GET-FILE-LINE (file line-index)
  "Read in the specified line, if it's not already cached, and return it."
  (cond ((< line-index (file-length file)) (aref file line-index))
        ((file-eof file) nil)
        (t (let ((bp (and (file-bp-table file)
                          (funcall (file-stream file) :read-bp t))))
             (multiple-value-bind (line eof)
                 (funcall (file-stream file) :line-in t)
               (cond ((and eof (or (null line) (equal line "")))
                      (setf (file-eof file) t)
                      nil)
                     (t (vector-push-extend line file)
                        (when bp
                          (vector-push-extend bp (file-bp-table file)))
                        line)
                     ))
             ))
        ))

(defun SRCCOM:STREAM-BUFFER (buffer-stream)
  "Obtain any ZWEI Buffer associated with the specified stream."
  (when (cli::broadcast-stream-p buffer-stream)
    (dolist (stream (cli::broadcast-stream-streams buffer-stream))
      (when (and (typep stream 'zwei:interval-stream)
                 (boundp-in-instance stream 'zwei:buffer))
        (let ((buffer (symbol-value-in-instance stream 'zwei:buffer)))
          (when (typep buffer 'zwei:buffer) (return buffer))))
      )))

;;;
;;; Special Variables and Constants:
;;;
(defparameter SRCCOM:*DELTA-ATTRIBUTE-FORMAT*
              "~&;;; -*- Mode: ~A; Syntax: ~A; Package: ~A; Base: ~D -*-~%~%"
  "The Format Directive String for a Delta's Attributes")

(defparameter SRCCOM:*COMPARE-FORMAT-KEYWORDS*
              '(:delta :merge :parallel :review :series)
  "The Compare Output Format Completion Keywords")

(defvar SRCCOM:*COMPARE-FORMAT-SELECTED* nil
  "The Editor Selected Source Compare Output Format")

(defvar SRCCOM:*LINES-NEEDED-TO-MATCH* 3
  "The Default exclusive bound for Transfixion")

(defvar SRCCOM:*LINES-TO-PRINT-BEFORE* 0
  "The Default inclusive bound for Prefices")
(defvar SRCCOM:*LINES-TO-PRINT-AFTER* 0                ; Formerly 1, by default.
  "The Default inclusive bound for Suffices")

(defvar SRCCOM:*PATHNAME-DEFAULTS* (fs:make-pathname-defaults)
  "The Source Comparison Pathname Defaults")

(defvar SRCCOM:*RECORD-MERGE-BOUNDS-P* nil
  "The Automatic Merge Recording Enable")
(defvar SRCCOM:*MERGE-RECORD* ()
  "The Automatic Merge Record")

;;;
;;; Now for the Code:
;;;
(defun SRCCOM:SOURCE-COMPARE-FILES (a-file
                                    b-file
                                    &optional
                                    (output-stream *standard-output*)
                                    (comment-p t)
                                    &key
                                    (ignore-case-and-style nil)
                                    (ignore-whitespace nil)
                                    (affix nil affix-sp)
                                    (format nil format-sp)
                                    (width nil width-sp))
  "Report differences between two record formatted streams."
  (declare (special compare:*default-compare-format*
                    compare:*compare-formatter*))
           
  (let ((any-differences nil)
        (delta-p (eq (cond (format-sp format)
                           (srccom:*compare-format-selected*)
                           (t compare:*default-compare-format*))
                     :delta)))
    (when comment-p
      (if delta-p
          ;;;
          ;;; Future:
          ;;;
          ;;;   Where OUTPUT-STREAM is to a Buffer: the Attributes ought to be
          ;;;   reparsed, but preferably without permuting the Buffer History.
          ;;;
          (format output-stream
                  srccom:*delta-attribute-format*
                  :lisp                                ; Modes aren't presentable yet.
                  (si:lisp-syntax-from-keyword :common-lisp)
                  (find-package 'compare)
                  *print-base*)
          (let ((attribute-line "-*- Mode: Fundamental -*-"))
            (format output-stream
                    "~&Source Compare made by ~A on ~\\DATIME\\ ~V,0T~A~%"
                    srccom:user-id
                    (- (if width-sp width (compare:stream-width output-stream))
                       (1+ (length attribute-line)))
                    attribute-line)
          
            (flet ((PRESENT-FILE (output-stream preposition file)
                     "Present the File Object as its indicated Type."
                     (let ((object (srccom:presentation-object file))
                           (type (srccom:presentation-type file)))
                       (if (and object type)
                           (format output-stream
                                   "~A ~A ~@\\PRESENTATION\\~%"
                                   preposition
                                   (srccom:file-type file)
                                   object
                                   type)
                           (format output-stream
                                   "~A ~A ~A~%"
                                   preposition
                                   (srccom:file-type file)
                                   (srccom:file-name file))
                           ))
                     ))

              (present-file output-stream "  of" a-file)
              (present-file output-stream "with" b-file)
              (terpri output-stream))
            )))
      
    (flet ((FILE-RECORD (file)
             "Return all of the specified file's records, as a vector."
             (do ((record-index 0 (1+ record-index)))
                 ((null (srccom:get-file-line file record-index)) file)
               ;; Null body.
               )))
      
      (let* ((a-record (file-record a-file))
             (b-record (file-record b-file))
             (a-header-type
               (srccom:header-type (srccom:file-major-mode a-file)))
             (b-header-type
               (srccom:header-type (srccom:file-major-mode b-file)))
             (format-keys
               (nconc (if affix-sp
                          (list :affix affix)
                          (list :prefix srccom:*lines-to-print-before*
                                :suffix srccom:*lines-to-print-after*
                                :transfix
                                (when (plusp srccom:*lines-needed-to-match*)
                                  (1- srccom:*lines-needed-to-match*))
                                ))
                      (when (or a-header-type b-header-type)
                        (list :header-type-pair
                              (cons a-header-type b-header-type)))
                      (cond (format-sp
                             (list :format format))
                            (srccom:*compare-format-selected*
                             (list :format srccom:*compare-format-selected*)))
                      (when width-sp (list :width width)))
               ))

        (setq any-differences
              (apply compare:*compare-formatter*
                     ;;
                     ;; Obtain the Differences (not the Correspondences:)
                     ;;
                     t
                   
                     ;;
                     ;; Pass the list of records obtained from each "file:"
                     ;;
                     a-record
                     b-record
                   
                     ;;
                     ;; Pass along the "normalizations" requested:
                     ;;
                     :ignore-case-and-style ignore-case-and-style
                     :ignore-whitespace ignore-whitespace
                   
                     ;;
                     ;; The Source Pair is normally used solely for the purpose
                     ;; of generating reasonable pathnames.  Here, however, the
                     ;; presence of a ZWEI "File Object" is indicated by a list
                     ;; (the first element in which is the symbol SRCCOM:FILE)
                     ;; in order to provide ZWEI:PRESENT-LINE with this object.
                     ;;
                     :source-pair
                     `((srccom:file ,a-file) . (srccom:file ,b-file))

                     :output-stream output-stream
                     format-keys))
        ))

    (when comment-p
      (cond (delta-p
             (format output-stream
                     "~&;;;~@[ End of ~A~]~%"
                     (srccom:stream-buffer output-stream)))
            (any-differences
             (format output-stream
                     "~&Done.~@[  Output is in buffer ~A.~]~%"
                     (srccom:stream-buffer output-stream)))
            (t
             (format output-stream
                     "~&No differences encountered.~%"))
            ))

    (close (srccom:file-stream a-file))
    (close (srccom:file-stream b-file))

    any-differences))

(defun SRCCOM:SOURCE-COMPARE (a-filename
                              b-filename
                              &optional
                              (output-stream *standard-output*)
                              (comment-p t)
                              &key
                              ignore-case-and-style
                              ignore-whitespace
                              deleted
                              (affix nil affix-sp)
                              (format nil format-sp)
                              (width nil width-sp))
  "Report differences between two record formatted files."
  (unwind-protect
      (let* ((a-path (fs:merge-pathnames-and-set-defaults
                       a-filename srccom:*pathname-defaults* :oldest))
             (b-path (fs:merge-pathnames b-filename a-path))
             (a-file (srccom:create-file a-path :deleted deleted))
             (b-file (srccom:create-file b-path :deleted deleted))
             (format-keys (nconc (when affix-sp (list :affix affix))
                                 (when format-sp (list :format format))
                                 (when width-sp (list :width width)))
                          ))
        (apply #'srccom:source-compare-files
               a-file
               b-file
               output-stream
               comment-p
               :ignore-case-and-style ignore-case-and-style
               :ignore-whitespace ignore-whitespace
               format-keys)
        (and a-file (funcall (srccom:file-stream a-file) :close))
        (and b-file (funcall (srccom:file-stream b-file) :close))
        )))

(defun SRCCOM:PROMPTED-SOURCE-COMPARE (a-filename
                                       b-filename
                                       &key
                                       ignore-case-and-style
                                       ignore-whitespace
                                       (affix nil affix-sp)
                                       (format nil format-sp)
                                       (width nil width-sp)
                                       &aux
                                       (output-stream *standard-output*)
                                       (comment-p t))
  (multiple-value-setq (a-filename b-filename)
    (srccom:get-srccom-file-names a-filename b-filename))
  
  (and a-filename
       b-filename
       (catch-error-restart ((error sys:abort) "Exit SRCCOM")
         (let ((format-keys (nconc (when affix-sp (list :affix affix))
                                   (when format-sp (list :format format))
                                   (when width-sp (list :width width)))
                            ))
           (apply #'srccom:source-compare
                  a-filename
                  b-filename
                  output-stream
                  comment-p
                  :ignore-case-and-style ignore-case-and-style
                  :ignore-whitespace ignore-whitespace
                  format-keys)
           ))
       ))

(defun SRCCOM:SOURCE-COMPARE-AUTOMATIC-MERGE-1 (a-file
                                                b-file
                                                output-stream
                                                &key
                                                ignore-case-and-style
                                                ignore-whitespace
                                                (affix nil affix-sp)
                                                (width nil width-sp)
                                                &aux
                                                (comment-p nil))
  "Merge differences between two record formatted streams."
  (let ((output-keys (nconc (when affix-sp (list :affix affix))
                            (when width-sp (list :width width)))
                     ))
    (apply #'srccom:source-compare-files
           a-file
           b-file
           output-stream
           comment-p
           :ignore-case-and-style ignore-case-and-style
           :ignore-whitespace ignore-whitespace
           :format :merge
           output-keys)))

(defun SRCCOM:SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING
       (a-file
        b-file
        output-stream
        &key
        ignore-case-and-style
        ignore-whitespace
        (affix nil affix-sp)
        (width nil width-sp))
  "Merge differences while recording each interval, for interactive editing."
  (let ((srccom:*merge-record* ())
        (srccom:*record-merge-bounds-p* t)
        (output-keys (nconc (when affix-sp (list :affix affix))
                            (when width-sp (list :width width)))
                     ))
    (declare (special srccom:*merge-record* srccom:*record-merge-bounds-p*))

    (apply #'srccom:source-compare-automatic-merge-1
           a-file
           b-file
           output-stream
           :ignore-case-and-style ignore-case-and-style
           :ignore-whitespace ignore-whitespace
           output-keys)
  
    (setq srccom:*merge-record* (nreverse srccom:*merge-record*))

    ;;;
    ;;; Set the status of those Buffer Pointers preceding a Merge Format Header
    ;;; so that any immediately preceding text insertions do not fall "inside".
    ;;;
    (dolist (record srccom:*merge-record*)
      (setf (zwei:bp-status (first record)) :moves)
      (setf (zwei:bp-status (third record)) :moves)
      (setf (zwei:bp-status (fifth record)) :moves))

    srccom:*merge-record*))

;;;
;;; It isn't clear who, if anyone, ever calls the following function.
;;;
(defun SRCCOM:SOURCE-COMPARE-AUTOMATIC-MERGE (a-filename
                                              b-filename
                                              output-filename
                                              &key
                                              ignore-case-and-style
                                              ignore-whitespace
                                              (affix nil affix-sp)
                                              (width nil width-sp))
  "Merge differences between two record formatted files."
  ;;
  ;; The following SETQ added to enhance the apparent symmetry
  ;; with SRCCOM:SOURCE-COMPARE.
  ;;
  (unwind-protect
      (let* ((a-path (fs:merge-pathnames-and-set-defaults
                       a-filename srccom:*pathname-defaults* :oldest))
             (b-path (fs:merge-pathnames b-filename a-path))
             (a-file (srccom:create-file a-path))
             (b-file (srccom:create-file b-path))
             (output-keys (nconc (when affix-sp (list :affix affix))
                                 (when width-sp (list :width width)))
                          ))
        (with-open-file (output-stream output-filename :direction :output)
          (apply #'srccom:source-compare-automatic-merge-1
                 a-file
                 b-file
                 output-stream
                 :ignore-case-and-style ignore-case-and-style
                 :ignore-whitespace ignore-whitespace
                 output-keys))
        (and a-file (funcall (srccom:file-stream a-file) :close))
        (and b-file (funcall (srccom:file-stream b-file) :close))
        )))

;;;
;;; Module Epilogue:
;;;
(provide 'srccom-patch)
