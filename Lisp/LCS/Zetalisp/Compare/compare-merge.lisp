;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-merge.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the Sequence Comparison Utility with a
;;;		"Merge Compare Formatter".
;;;
;;; Usage:	This file will make specific reference to the Genera 8.0
;;;		rendition of the "ZWEI Internal" Merge Recording Interface
;;;		where the Feature GENERA is asserted.  Otherwise, this file
;;; 		is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	This file provides the COMPARE-AND-MERGE Output Formatter.
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Genera (ZWEI) Specific Interfaces:
;;;
;;;	push-mark		marks &optional stream
;;;
;;; Local Interfaces:
;;;
;;;	compare-and-merge	differ-p a-sequence b-sequence
;;;				&key source-pair output-stream width
;;;
;;;	write-pair-and-merge	differ-p sequence-pair &optional interval-pair
;;;				interval-index &key source-pair name-pair
;;;				output-stream width object-pair type-pair
;;;
;;;	write-half-head		sequence-index &optional interval-pair
;;;				interval-index name-pair diagnostic
;;;				output-stream width object type
;;;
;;;	write-half-foot		sequence-index &optional interval-pair
;;;				interval-index name-pair diagnostic
;;;				output-stream width object type
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
(defun COMPARE-AND-MERGE (differ-p
			  a-sequence
			  b-sequence
			  &rest keys
			  &key
			  (source-pair nil)
			  (output-stream *standard-output*)
			  (width nil width-sp)
			  &allow-other-keys)
  "Perform a Formatted Compare and Merge."
  (multiple-value-bind (in-seq-pairs interval-pairs length-pair)
      (apply #'basic-compare-sequences differ-p a-sequence b-sequence keys)
    (let* ((exterval-pairs (complement-intervals interval-pairs length-pair))
           (ex-seq-pairs
	     (subseq-intervals a-sequence b-sequence exterval-pairs)))

      (when (or interval-pairs exterval-pairs)
        (let* ((interval-index 0)
	       (name-pair (stream-name-pair source-pair))
	       (a-source (car source-pair))
	       (b-source (cdr source-pair))
	       (first-interval-pair (first interval-pairs))
               (first-exterval-pair (first exterval-pairs))
               (interval-led
		 (interval-led-p first-interval-pair first-exterval-pair))
	       (width-keys (when width-sp (list :width width))))
          
	  (multiple-value-bind (a-object a-type)
	      (stream-presentation a-source)
	    (multiple-value-bind (b-object b-type)
		(stream-presentation b-source)
	      (let ((object-pair (cons a-object b-object))
		    (type-pair (cons a-type b-type)))

		(fresh-line output-stream)
		(do ((interval-pair-remains interval-pairs
					    (rest interval-pair-remains))
		     (exterval-pair-remains exterval-pairs
					    (rest exterval-pair-remains))
		     (in-seq-pair-remains in-seq-pairs
					  (rest in-seq-pair-remains))
		     (ex-seq-pair-remains ex-seq-pairs
					  (rest ex-seq-pair-remains)))
		    ((and (endp in-seq-pair-remains)
			  (endp ex-seq-pair-remains)))
		  (incf interval-index)
		  (let ((in-seq-pair (first in-seq-pair-remains))
			(ex-seq-pair (first ex-seq-pair-remains))
			(interval-pair (first interval-pair-remains))
			(exterval-pair (first exterval-pair-remains)))
              
		    ;;
		    ;; Write each Record and "Merge:"
		    ;;
		    (dolist (this-is-differ-p
			      (list interval-led (not interval-led)))
		      (multiple-value-bind
			(this-differ-p this-interval-pair this-seq-pair)
			  (if this-is-differ-p
			      (values differ-p
				      interval-pair
				      in-seq-pair)
			      (values (not differ-p)
				      exterval-pair
				      ex-seq-pair))
			(when this-interval-pair
			  (apply #'write-pair-and-merge
				 this-differ-p
				 this-seq-pair
				 this-interval-pair
				 interval-index
				 :source-pair source-pair
				 :name-pair name-pair
				 :object-pair object-pair
				 :type-pair type-pair
				 :output-stream output-stream
				 width-keys))
			))
		    ))
		)))
          ))
      
      (when interval-pairs t))
    ))

#+genera
(defmacro PUSH-MARK (marks &optional stream)
  "Push a mark onto a six mark sub-list, for ZWEI to manipulate later."
  `(push (zwei:copy-bp (funcall ,stream :read-bp t) :normal) ,marks))

(defun WRITE-PAIR-AND-MERGE (differ-p
			     sequence-pair
                             &optional
                             (interval-pair nil)
                             (interval-index nil)
                             &key
                             (source-pair nil)
                             (name-pair nil)
                             (output-stream *standard-output*)
                             (width nil width-sp)
                             (object-pair nil)
                             (type-pair nil)
                             &allow-other-keys)
  "Format a matched record pair vertically, and merge it with the later file."
  #+genera
  (declare (special srccom:*merge-record* srccom:*record-merge-bounds-p*))
  (let ((diagnostic (diagnose-pair differ-p sequence-pair))
	(a-index 0)
        (b-index 1)
        (a-source (car source-pair))
        (b-source (cdr source-pair))
	(a-object (car object-pair))
	(b-object (cdr object-pair))
	(a-type (car type-pair))
	(b-type (cdr type-pair))
        (a-records (car sequence-pair))
        (b-records (cdr sequence-pair))
        (head-width (if width-sp width (stream-width output-stream)))
        (width-keys (when width-sp (list :width width)))
	#+genera
	(merge-this-record ()))

    (when differ-p
      #+genera
      (when srccom:*record-merge-bounds-p*
	;;
	;; Push the First of Six Marks delimiting the merge constituents.
	;;
	(push-mark merge-this-record output-stream))

      ;;
      ;; Write out the pair of halves: one after the other,
      ;; delimited by their respective "heads" and "feet".
      ;;
      (write-half-head a-index
		       interval-pair
		       interval-index
		       name-pair
		       diagnostic
		       output-stream
		       head-width
		       a-object
		       a-type)

      #+genera
      (when srccom:*record-merge-bounds-p*
	(push-mark merge-this-record output-stream))	; Push Second Mark.

      (unless (nonep a-records)
	(apply #'write-frame
	       a-records output-stream a-source (car interval-pair) width-keys))
    
      #+genera
      (when srccom:*record-merge-bounds-p*
	(push-mark merge-this-record output-stream))	; Push Third Mark.

      (write-half-foot a-index
		       interval-pair
		       interval-index
		       name-pair
		       diagnostic
		       output-stream
		       head-width
		       a-object
		       a-type)

      (write-half-head b-index
		       interval-pair
		       interval-index
		       name-pair
		       diagnostic
		       output-stream
		       head-width
		       b-object
		       b-type)
      
      #+genera
      (when srccom:*record-merge-bounds-p*
	(push-mark merge-this-record output-stream)))	; Push Fourth Mark.

    ;;
    ;; In MERGE Format: all records from the "later" file are written out here,
    ;; whether or not they are from a difference.
    ;;
    (unless (nonep b-records)
      (apply #'write-frame
	     b-records output-stream b-source (cdr interval-pair) width-keys))

    (when differ-p
      #+genera
      (when srccom:*record-merge-bounds-p*
	(push-mark merge-this-record output-stream))	; Push Fifth Mark.

      (write-half-foot b-index
		       interval-pair
		       interval-index
		       name-pair
		       diagnostic
		       output-stream
		       head-width
		       b-object
		       b-type)
      
      #+genera
      (when srccom:*record-merge-bounds-p*
	;;
	;; Push the Sixth and Final Mark, then push this Six Mark sub-list
	;; onto the "Merge Record".
	;;
	(push-mark merge-this-record output-stream)
	(push (nreverse merge-this-record) srccom:*merge-record*)))
    
    (values)))

;;;
;;; The first of the following "banner writers," WRITE-HALF-HEAD,
;;; was moved here from "compare-series.lisp", which still uses it.
;;;
;;; WRITE-PAIR-AND-MERGE uses the complementary banner writer,
;;; WRITE-HALF-FOOT, in order to clearly delimit both "halves"
;;; from text common to both input sequences.
;;;
(defun WRITE-HALF-HEAD (sequence-index
			&optional
			interval-pair
			interval-index
			name-pair
			diagnostic
			(output-stream *standard-output*)
			(width nil width-sp)
			object
			type)
  "Write a head for the next half (of a pair), to the specified stream."
  (declare (special *label-format*))
  (multiple-value-bind (interval name pure-fill)
      (if (zerop sequence-index)
	  (values (car interval-pair) (car name-pair) #'pure-less)
	  (values (cdr interval-pair) (cdr name-pair) #'pure-greater))
    (let* ((label (format nil
			  *label-format*
			  interval-index
			  (when diagnostic (string-capitalize diagnostic))
			  (interval-to-string interval)
			  name))
	   (label-length (length label))
	   (head-width (if width-sp width (stream-width output-stream))))
      (multiple-value-bind (fill-start fill-end crop-start crop-end)
	  (center-values label-length head-width)
	(let* ((crop-keys (nconc (when (plusp crop-start)
				   (list :start crop-start))
				 (when (< crop-end label-length)
				   (list :end crop-end))
				 ))
	       (fill-finish (- head-width fill-end))
	       (fill-maximum (max fill-start fill-finish))
	       (fill (funcall pure-fill fill-maximum)))
	  (write-string fill output-stream :end fill-start)
	  (apply #'write-record-as-object
		 label output-stream object type :truncate t crop-keys)
	  (write-line fill output-stream :end fill-finish)))
      )))

(defun WRITE-HALF-FOOT (sequence-index
			&optional
			interval-pair
			interval-index
			name-pair
			&rest args)
  "Write a foot for the next half (of a pair), to the specified stream."
  (declare (ignore interval-pair name-pair))
  (apply #'write-half-head sequence-index nil interval-index nil args))

;;;
;;; Suspend Module Context:
;;;
