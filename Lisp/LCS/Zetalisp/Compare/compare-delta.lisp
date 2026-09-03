;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-delta.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the Sequence Comparison Utility with a
;;;		"Delta Compare Formatter".
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	This file provides the COMPARE-AS-DELTA "Compare Formatter".
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;	compare-as-delta	differ-p a-sequence b-sequence &key pretty
;;;				source-pair output-stream width hash-version
;;;
;;;	cons-pair-as-edit	differ-p sequence-pair &optional
;;;				interval-pair interval-index hash-version
;;;
;;;	cons-delta		differ-p a-sequence b-sequence edit-forms
;;;				&optional path-pair hash-version
;;;
;;;	print-delta		delta &optional output-stream
;;;
;;; Hooked Interfaces:
;;;
;;;	*delta-formatter*	output-stream &rest args
;;;

;;;
;;; Resume Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.

;;;
;;; Exported Interfaces:
;;;
;;; NOTE!  The "TRUNCATED" symbol is EXPORTed so that it may appear without
;;;	   Package Qualification, where PRINTed below.
;;;
;;;	   A string wrapped in a TRUNCATED form indicates the final record
;;;	   of a file, which had no following Newline when it was read in.
;;;
(export '(truncated))

;;;
;;; Special Variables and Constants:
;;;
#+:xp
(defparameter *DELTA-FORMATTER*
	      (xp:formatter "~:<~W~1I ~_~
			     ~:<~@{~:<~W~1I ~_~
			     ~:<~@{~W~^ ~:@_~}~:>~^ ~_~
			     ~@{~W ~W~^ ~:_~}~
			     ~:>~^ ~_~}~:>~^ ~_~
			     ~@{~W ~W~^ ~_~}~:>")
  "The Formatter used to Pretty Print a Delta")

;;;
;;; Now for the Code:
;;;
(defun COMPARE-AS-DELTA (differ-p
                         a-sequence
                         b-sequence
                         &rest keys
                         &key
                         (source-pair nil)
                         (output-stream *standard-output*)
			 (width nil width-sp)
                         (hash-version *default-hash-version*)
                         (pretty *print-pretty*)
                         &allow-other-keys)
  "Perform a Formatted Compare in Series."
  (declare (special *default-hash-version* #+:xp xp:*print-right-margin*)
	   #-:xp
	   (ignore width width-sp))
  (multiple-value-bind (sequence-pairs interval-pairs length-pair)
      (apply #'basic-compare-sequences differ-p a-sequence b-sequence keys)
    (declare (ignore length-pair))
    (let ((interval-index 0)
	  #+:xp
	  (pretty-margin (if width-sp width (stream-width output-stream)))
          (path-pair (stream-path-pair source-pair))
          (edit-forms ()))
      (do ((interval-pair-remains interval-pairs (rest interval-pair-remains))
           (sequence-pair-remains sequence-pairs (rest sequence-pair-remains)))
          ((endp sequence-pair-remains))
        (incf interval-index)
        (let ((sequence-pair (first sequence-pair-remains))
              (interval-pair (first interval-pair-remains)))
          ;;
          ;; CONS up an Edit from each Pair:
          ;;
          (push (cons-pair-as-edit
		  differ-p
		  sequence-pair
		  interval-pair
		  interval-index
		  hash-version)
                edit-forms)
	  ))
      
      ;;
      ;; Print the Delta whether or not any Edits have been made.
      ;;
      (let ((delta-form (cons-delta differ-p
				    a-sequence
				    b-sequence
				    (nreverse edit-forms)
				    path-pair
				    hash-version))
	    (*package* (find-package 'compare))
	    (*print-pretty* pretty)
	    #+:xp
	    (xp:*print-right-margin* pretty-margin))
	(print-delta delta-form output-stream))
      
      (when interval-pairs t))
    ))

(defun CONS-PAIR-AS-EDIT (differ-p
                          sequence-pair
                          &optional
                          (interval-pair nil)
                          (interval-index nil)
                          (hash-version *default-hash-version*))
  "Convert a matched record pair into an edit."
  (declare (special *default-hash-version*))
  (let ((a-interval (car interval-pair))
        (a-records (car sequence-pair))
        (b-records (cdr sequence-pair)))
    
    `(,(if differ-p 'delta-edit 'merge-edit)
      ,(coerce b-records 'list)
      ,.(when a-interval
          (let ((a-start (first a-interval))
                (a-end (second a-interval)))
            (when a-start
              `(:start ,a-start
		,.(when (and a-end (<= a-start a-end))
		    `(:end ,a-end))))
            ))
      ,.(unless (nonep a-records)
	  `(:hash ,(hash-records a-records 0 hash-version)))
      ,.(when interval-index `(:index ,interval-index))
      )))

(defun CONS-DELTA (differ-p
                   a-sequence
                   b-sequence
                   edit-forms
                   &optional
                   (path-pair nil)
                   (hash-version *default-hash-version*))
  "Convert a list of edit forms into a delta."
  (declare (ignore a-sequence) (special *default-hash-version*))
  (let ((a-path (car path-pair))
        (b-path (cdr path-pair)))
    (multiple-value-bind (hash version)		; Checksum the overall result.
	(hash-records b-sequence 0 hash-version)
      `(,(if differ-p 'delta-file 'merge-file)
        ,edit-forms
        ,.(when a-path `(:input-file ,a-path))
        ,.(when b-path `(:output-file ,b-path))
        ,.(when (and differ-p b-sequence) `(:output-hash ,hash))
        :hash-version ,version))
    ))

(defun PRINT-DELTA (delta &optional (output-stream *standard-output*))
  "Print the Delta prettily, as would be pleased."
  #+:xp (declare (special *delta-formatter*))
  #+:xp (if *print-pretty*
	    (funcall *delta-formatter* output-stream delta)
	    (prin1 delta output-stream))
  #-:xp (prin1 delta output-stream))

;;;
;;; Suspend Module Context:
;;;
