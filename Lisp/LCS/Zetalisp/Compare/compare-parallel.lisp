;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-parallel.lisp  Module: compare	Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the Sequence Comparison Utility with a
;;;		"Parallel Compare Formatter".
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	This file provides the COMPARE-IN-PARALLEL "Compare Formatter".
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;	compare-in-parallel	differ-p a-sequence b-sequence
;;;				&key source-pair output-stream width
;;;
;;;	write-pair-in-parallel	sequence-pair &optional interval-pair
;;;				interval-index finishing &key source-pair
;;;				output-stream width object-pair type-pair
;;;
;;;	write-record-pair	record-pair
;;;				&optional divider output-stream width
;;;				object-pair type-pair
;;;
;;;	write-side-head-pair	&optional interval-pair interval-index
;;;				name-pair divider output-stream width
;;;				object-pair type-pair
;;;
;;;	write-side-head		sequence-index &optional interval-pair
;;;				interval-index name-pair output-stream width
;;;				object-pair type-pair
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
;;; Special Variables and Constants:
;;;
(defparameter *MAXIMUM-DEFAULT-PARALLEL-WIDTH* 256.
  "The Maximum Default (Parallel Format) Width")

;;;
;;; Now for the Code:
;;;
(defun COMPARE-IN-PARALLEL (differ-p
                            a-sequence
                            b-sequence
                            &rest keys
                            &key
                            (source-pair nil)
                            (output-stream *standard-output*)
                            (width nil width-sp)
                            &allow-other-keys)
  "Perform a Formatted Compare in Parallel."
  (declare (special *maximum-default-parallel-width*))
  (multiple-value-bind (sequence-pairs interval-pairs length-pair)
      (apply #'basic-compare-sequences differ-p a-sequence b-sequence keys)
    (declare (ignore length-pair))
      
    ;;
    ;; Generate a heading when there is SOME output is to follow:
    ;;
    (when interval-pairs
      (let ((interval-index 0)
	    (name-pair (stream-name-pair source-pair))
	    (a-source (car source-pair))
	    (b-source (cdr source-pair))
	    (full-width (if width-sp
			    width (min (stream-width output-stream)
				       *maximum-default-parallel-width*))))
          
	(multiple-value-bind (a-object a-type)
	    (stream-presentation a-source)
	  (multiple-value-bind (b-object b-type)
	      (stream-presentation b-source)
	    (let ((object-pair (cons a-object b-object))
		  (type-pair (cons a-type b-type)))

	      (fresh-line output-stream)
	      (write-side-head-pair nil		; There's no interval yet,
				    nil		;and so neither any index.
				    name-pair
				    " "		; Allow for dividers below.
				    output-stream
				    full-width
				    object-pair
				    type-pair)

	      (do ((interval-pair-remains interval-pairs
					  (rest interval-pair-remains))
		   (sequence-pair-remains sequence-pairs
					  (rest sequence-pair-remains)))
		  ((endp sequence-pair-remains))
		(incf interval-index)
		(let ((sequence-pair (first sequence-pair-remains))
		      (interval-pair (first interval-pair-remains))
		      (finishing (endp (rest sequence-pair-remains))))

		  ;;
		  ;; Write each Pair "in Parallel:"
		  ;;
		  (write-pair-in-parallel
		    sequence-pair
		    interval-pair
		    interval-index
		    finishing
		    :source-pair source-pair
		    :output-stream output-stream
		    :width full-width
		    :object-pair object-pair
		    :type-pair type-pair)))
	      )))
	))
      
    (when interval-pairs t)))

(defun WRITE-PAIR-IN-PARALLEL (sequence-pair
                               &optional
                               (interval-pair nil)
                               (interval-index nil)
                               (finishing nil)
                               &key
			       (source-pair nil)
                               (output-stream *standard-output*)
                               (width nil width-sp)
			       (object-pair nil)
			       (type-pair nil)
                               &allow-other-keys)
  "Format a matched record pair horizontally and send it to the output stream."
  (declare (special *maximum-default-parallel-width*)
	   #-genera
	   (ignore source-pair))
  (let* ((a-records (car sequence-pair))
         (b-records (cdr sequence-pair))
	 #+genera (a-source (car source-pair))
	 #+genera (b-source (cdr source-pair))
	 #+genera (a-file (when (srccom:file-p a-source) (second a-source)))
	 #+genera (b-file (when (srccom:file-p b-source) (second b-source)))
	 #+genera (a-table (and a-file (srccom:file-bp-table a-file)))
	 #+genera (b-table (and b-file (srccom:file-bp-table b-file)))
	 #+genera (bp-type-pair '(zwei:bp . zwei:bp))
	 (full-width (if width-sp
			 width (min (stream-width output-stream)
				    *maximum-default-parallel-width*)))
         (side-width (truncate (1- full-width) 2))
	 (cross "+"))

    ;;
    ;; Write out the pair of sides: one beside the other,
    ;; delimited by their respective "heads" and "feet".
    ;;
    (write-side-head-pair interval-pair
			  interval-index
			  nil			; Names are omitted here.
			  cross
			  output-stream
			  full-width
			  object-pair
			  type-pair)

    (let* ((foot (pure-minus side-width))
	   (a-length (length a-records))
	   (b-length (length b-records))
	   (length-max (max a-length b-length))
	   (line-limit (if (and finishing (plusp length-max))
			   (1+ length-max) length-max))
	   #+genera (a-line (first (car interval-pair)))
	   #+genera (b-line (first (cdr interval-pair)))
	   (a-frames (frame-records a-records :width side-width))
	   (b-frames (frame-records b-records :width side-width))
	   (a-remains (when (listp a-frames) a-frames))
	   (b-remains (when (listp b-frames) b-frames)))
      (dotimes (line-count line-limit)
	(let* ((a-ending (= line-count a-length))
	       (b-ending (= line-count b-length))
	       (a-footed (and a-ending (plusp a-length)))
	       (b-footed (and b-ending (plusp b-length)))
	       (a-element (when (< line-count a-length)
			    (if (listp a-frames)
				(pop a-remains) (aref a-frames line-count))
			    ))
	       (b-element (when (< line-count b-length)
			    (if (listp b-frames)
				(pop b-remains) (aref b-frames line-count))
			    ))
	       (a-framer (if a-footed foot a-element))
	       (b-framer (if b-footed foot b-element))
	       (record-pair (cons a-framer b-framer))
	       (divider (if (or a-footed b-footed) "+" "|")))

	  #+genera (when a-ending (setq a-line nil))
	  #+genera (when b-ending (setq b-line nil))

	  (let* (#+genera (a-bp (when (and a-table a-line)
				  (aref a-table a-line)))
		 #+genera (b-bp (when (and b-table b-line)
				  (aref b-table b-line)))
		 #+genera (bp-object-pair (cons a-bp b-bp)))
	    (write-record-pair record-pair
			       divider
			       output-stream
			       full-width
			       #+genera bp-object-pair
			       #+genera bp-type-pair)))

	#+genera (when a-line (incf a-line))
	#+genera (when b-line (incf b-line))
	))
    
    (values)))

(defun WRITE-RECORD-PAIR (record-pair
			  &optional
			  (divider "|")
			  (output-stream *standard-output*)
			  (width nil width-sp)
			  (object-pair nil)
			  (type-pair nil))
  "Present the pair of records, as the specified pair of objects (and types.)"
  (declare (special *maximum-default-parallel-width*))
  (let* ((a-record (car record-pair))
	 (b-record (cdr record-pair))
	 (a-object (car object-pair))
	 (b-object (cdr object-pair))
	 (a-type (car type-pair))
	 (b-type (cdr type-pair))
	 (full-width (if width-sp
			 width (min (stream-width output-stream)
				    *maximum-default-parallel-width*)))
	 (side-width (truncate (1- full-width) 2))
	 (side-keys (list :end side-width))
	 (a-width 0))

    (when a-record
      (let* ((a-string (string a-record))
	     (a-length (length a-string))
	     ;;
	     ;; "Newline" must be suppressed on the left side.
	     ;;
	     (end-keys (list* :truncate t
			      (when (> a-length side-width) side-keys)
			      )))
	(apply #'write-record-as-object
	       a-string output-stream a-object a-type end-keys)
	(setq a-width (min a-length side-width))
	))

    (let ((c-width (- side-width a-width)))
      (when (plusp c-width)
	(write-string (pure-space c-width) output-stream :end c-width)))

    (cond (b-record
	   (when (plusp full-width) (write-string divider output-stream))

	   (let* ((b-string (string b-record))
		  (b-length (length b-string))
		  (end-keys (when (> b-length side-width) side-keys)))
	     (apply #'write-record-as-object
		    b-string output-stream b-object b-type end-keys)
	     ))
	  ((plusp full-width) (write-line divider output-stream))
	  (t (terpri output-stream)))
    ))

(defun WRITE-SIDE-HEAD-PAIR (&optional
			     (interval-pair nil)
			     (interval-index nil)
			     (name-pair nil)
			     (divider "+")
			     (output-stream *standard-output*)
			     (width nil width-sp)
			     object-pair
			     type-pair)
  "Write a pair of interval headers, side by side, to the specified stream."
  (declare (special *maximum-default-parallel-width*))
  (let* ((a-index 0)
	 (b-index 1)
	 (full-width (if width-sp
			 width (min (stream-width output-stream)
				    *maximum-default-parallel-width*)))
	 (side-width (truncate (1- full-width) 2)))
    (write-side-head a-index
		     interval-pair
		     interval-index
		     name-pair
		     output-stream
		     side-width
		     object-pair
		     type-pair)

    (when (plusp full-width) (write-string divider output-stream))

    (write-side-head b-index
		     interval-pair
		     interval-index
		     name-pair
		     output-stream
		     side-width
		     object-pair
		     type-pair)
    ))

(defun WRITE-SIDE-HEAD (sequence-index
			&optional
			interval-pair
			interval-index
			name-pair
			(output-stream *standard-output*)
			(width nil width-sp)
			object-pair
			type-pair)
  "Write a head for the next side, to the specified stream."
  (declare (special *label-format*) (ignore interval-index))
  (multiple-value-bind (interval name object type)
      (values-list
	(mapcar (if (zerop sequence-index) #'car #'cdr)
		(list interval-pair name-pair object-pair type-pair)))
    (let* ((side-label (format nil
			       *label-format*
			       (interval-to-string interval)
			       name))
	   (side-label-length (length side-label))
	   (side-width (if width-sp width (stream-width output-stream)))
	   (side-start (first interval))
	   (side-end (second interval))
	   (minimum-keys (when (null interval) (list :fill-minimum 1)))
	   (pure-fill (cond ((null interval) #'pure-space)
			    ((< side-start side-end) #'pure-minus)
			    (t #'pure-equal)
			    )))
      (multiple-value-bind (fill-start fill-end crop-start crop-end)
	  (apply #'center-values side-label-length side-width minimum-keys)
	(let* ((crop-keys (nconc (when (plusp crop-start)
				   (list :start crop-start))
				 (when (< crop-end side-label-length)
				   (list :end crop-end))
				 ))
	       (fill-finish (- side-width fill-end))
	       (fill-maximum (max fill-start fill-finish))
	       (fill (funcall pure-fill fill-maximum)))
	  (write-string fill output-stream :end fill-start)
	  (apply #'write-record-as-object
		 side-label output-stream object type :truncate t
		 crop-keys)
	  ;;
	  ;; Hack Newline Suppression, on the left side.
	  ;;
	  (funcall (if (zerop sequence-index) #'write-string #'write-line)
		   fill output-stream :end fill-finish)
	  )))
    ))

;;;
;;; Suspend Module Context:
;;;
