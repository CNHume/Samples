;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-review.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the Sequence Comparison Utility with a
;;;		"Review Compare Formatter".
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	This file provides the COMPARE-IN-REVIEW "Compare Formatter".
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;	compare-in-review	differ-p a-sequence b-sequence
;;;				&key source-pair output-stream width
;;;
;;;	write-pair-in-review	differ-p sequence-pair
;;;				&optional interval-pair interval-index
;;;				&key source-pair output-stream width
;;;
;;;	write-review-head	name
;;;				&optional output-stream width object type
;;;
;;;	write-trace		diagnostic fill-char
;;;				&optional output-stream width object type
;;;

;;;
;;; Resume Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Now for the Code:
;;;
(defun COMPARE-IN-REVIEW (differ-p
                          a-sequence
                          b-sequence
                          &rest keys
                          &key
                          (source-pair nil)
                          (output-stream *standard-output*)
                          (width nil width-sp)
                          &allow-other-keys)
  "Perform a Formatted Compare in Review."
  (multiple-value-bind (in-seq-pairs interval-pairs length-pair)
      (apply #'basic-compare-sequences differ-p a-sequence b-sequence keys)
    (let* ((exterval-pairs (complement-intervals interval-pairs length-pair))
           (ex-seq-pairs
	     (subseq-intervals a-sequence b-sequence exterval-pairs)))

      (when (or interval-pairs exterval-pairs)
        (let* ((interval-index 0)
	       (first-interval-pair (first interval-pairs))
               (first-exterval-pair (first exterval-pairs))
               (interval-led
		 (interval-led-p first-interval-pair first-exterval-pair))
               (name-pair (stream-name-pair source-pair))
               (b-name (cdr name-pair))
               (head-width (if width-sp width (stream-width output-stream)))
               (width-keys (when width-sp (list :width width))))

	  (multiple-value-bind (b-object b-type)
	      (stream-presentation (cdr source-pair))
	    (fresh-line output-stream)
	    (write-review-head b-name
			       output-stream
			       head-width
			       b-object
			       b-type))
          
          (do ((interval-pair-remains interval-pairs
                                      (rest interval-pair-remains))
               (exterval-pair-remains exterval-pairs
                                      (rest exterval-pair-remains))
               (in-seq-pair-remains in-seq-pairs (rest in-seq-pair-remains))
               (ex-seq-pair-remains ex-seq-pairs (rest ex-seq-pair-remains)))
              ((and (endp in-seq-pair-remains) (endp ex-seq-pair-remains)))
            (incf interval-index)
            (let ((in-seq-pair (first in-seq-pair-remains))
                  (ex-seq-pair (first ex-seq-pair-remains))
                  (interval-pair (first interval-pair-remains))
                  (exterval-pair (first exterval-pair-remains)))
              
              ;;
              ;; Write each Record "in Review:"
              ;;
              (dolist (this-is-differ-p (list interval-led (not interval-led)))
                (multiple-value-bind
                  (this-differ-p this-interval-pair this-seq-pair)
		    (if this-is-differ-p
			(values differ-p interval-pair in-seq-pair)
			(values (not differ-p) exterval-pair ex-seq-pair))
                  (when this-interval-pair
                    (apply #'write-pair-in-review
			   this-differ-p
			   this-seq-pair
			   this-interval-pair
			   interval-index
			   :source-pair source-pair
			   :output-stream output-stream
			   width-keys))
                  ))
              ))
          ))
      
      (when interval-pairs t))
    ))

(defun WRITE-PAIR-IN-REVIEW (differ-p
                             sequence-pair
                             &optional
                             (interval-pair nil)
                             (interval-index nil)
                             &key
                             (source-pair nil)
                             (output-stream *standard-output*)
                             (width nil width-sp)
                             &allow-other-keys)
  "Mark each output record appropriately and send it to the output stream."
  (declare (ignore interval-index))
  (let* ((diagnostic (diagnose-pair differ-p sequence-pair))
         (b-records (cdr sequence-pair))
         (change-bar (case diagnostic
		       (modify "|")
		       (delete "<")
		       (insert ">")
		       (voided "#")
		       (common (if (nonep b-records) "=" " "))
		       (otherwise "?")))
	 (head-width (if width-sp width (stream-width output-stream)))
	 (width-keys (when width-sp (list :width width)))
	 (change-keys (list* :change-bar change-bar width-keys)))
    
    ;;
    ;; Write out the new records, "marked" as appropriate.
    ;;
    (if (nonep b-records)
	(multiple-value-bind (a-object a-type)
	    (stream-presentation (car source-pair))
	  (write-trace diagnostic
		       (schar change-bar 0)
		       output-stream
		       head-width
		       a-object
		       a-type))
	(apply #'write-frame
	       b-records
	       output-stream
	       (cdr source-pair)
	       (cdr interval-pair)
	       change-keys))
    
    (values)))

(defun WRITE-REVIEW-HEAD (name
			  &optional
			  (output-stream *standard-output*)
			  (width nil width-sp)
			  object
			  type)
  "Write a head for the file under review, to the specified stream."
  (declare (special *label-format*))
  (let ((label (format nil *label-format* name))
	(head-width (if width-sp width (stream-width output-stream)))
	(mark-fill #\-)
	(mark-edge #\+))
    (multiple-value-bind (head start end)
	(center-fill label head-width mark-fill)
      (let ((head-length (length head)))
	(when (plusp head-length)
	  (setf (schar head 0) mark-edge)
	  (setf (schar head (1- head-length)) mark-edge))
	(write-record-as-object head output-stream object type
				:presentation-start start
				:presentation-end end)
	))
    ))

(defun WRITE-TRACE (diagnostic
		    fill-char
		    &optional
		    (output-stream *standard-output*)
		    (width nil width-sp)
		    object
		    type)
  "Write a trace (for a record interval), to the specified stream."
  (declare (special *label-format*))
  (let* ((trace (format nil
			*label-format*
			(when diagnostic (string-capitalize diagnostic))
			))
	 (trace-length (length trace))
	 (head-width (if width-sp width (stream-width output-stream))))
    (multiple-value-bind (fill-start fill-end crop-start crop-end)
	(center-values trace-length head-width)
      (let* ((crop-keys (nconc (when (plusp crop-start)
				 (list :start crop-start))
			       (when (< crop-end trace-length)
				 (list :end crop-end))
			       ))
	     (fill-finish (- head-width fill-end))
	     (fill-maximum (max fill-start fill-finish))
	     (fill (pure-string fill-maximum fill-char)))
	(write-string fill output-stream :end fill-start)
	(apply #'write-record-as-object
	       trace output-stream object type :truncate t crop-keys)
	(write-line fill output-stream :end fill-finish)))
    ))

;;;
;;; Suspend Module Context:
;;;
