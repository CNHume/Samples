;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-io.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the Sequence Comparison Utility with I/O support.
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	This file defines the utility's default I/O behaviour.
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; External Interfaces:
;;;
;;;	stream-width		stream
;;;
;;; Local Interfaces:
;;;
;;;	stream-name-pair	source-pair
;;;	stream-path-pair	source-pair
;;;
;;;	stream-name		source
;;;	stream-path		source
;;;	stream-presentation	source
;;;
;;;	circle-plus		a-number b-number &optional demimod
;;;
;;;	hash-string		string &optional context version
;;;	hash-records		records &optional context version
;;;				&key start end
;;;
;;;	write-frame		records &optional output-stream source interval
;;;				&key tab-spaces change-bar width
;;;
;;;	write-records		records &optional output-stream source interval
;;;				&key start end change-bar
;;;
;;;	write-record-as-object	record &optional output-stream object type
;;;				&key start end change-bar truncate
;;;
;;;	read-records		&optional
;;;				input-stream eof-error-p eof-value recursive-p
;;;
;;;	truncated-p		record
;;;
;;;	file-p			source
;;;

;;;
;;; Resume Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
#+symbolics
(import '(scl:operation-handled-p))

;;;
;;; The following is proclaimed an Inline Function
;;; to expedite operation of HASH-STRING, below:
;;;
#-:ccl
(proclaim '(inline circle-plus))

;;;
;;; Exported Interfaces:
;;;
(export '(stream-width *default-width*))

;;;
;;; Special Variables and Constants:
;;;
;;; The following is now used only where a Width is not forthcoming
;;; from some sensible source, like the stream, or from the User.
;;;
(defparameter *DEFAULT-WIDTH* #-genera 80. #+genera 95.
	      "The Default (Compare Formatter) Width")

(defparameter *DEFAULT-HASH-VERSION* 0
  "The Default Hash Code Version")

;;;
;;; Now for the Code:
;;;
(defun STREAM-NAME-PAIR (source-pair)
  "Return a pair of Names for the specified pair of sources."
  (let ((a-source (car source-pair))
	(b-source (cdr source-pair)))
    (cons (stream-name a-source)
	  (stream-name b-source))
    ))

(defun STREAM-PATH-PAIR (source-pair)
  "Return a pair of Paths for the specified pair of sources."
  (let ((a-source (car source-pair))
	(b-source (cdr source-pair)))
    (cons (stream-path a-source)
	  (stream-path b-source))
    ))

(defun STREAM-NAME (source)
  "Return something like a TRUENAME associated with the source."
  #-symbolics
  ;;
  ;; NOTE!  The following only admits STREAMs that have a PATHNAME
  ;; associated with them.  Higher intelligence may be needed here.
  ;;
  (when source (namestring (or (probe-file source)
			       (pathname source))))
  #+symbolics
  (cond ((null source) nil)
	#+genera
	((srccom:file-p source) (srccom:file-name (second source)))
	((and (operation-handled-p source :truename) (probe-file source)))
	((operation-handled-p source :pathname) (pathname source))
	(t (princ-to-string source))))

(defun STREAM-PATH (source)
  "Return any PATHNAME associated with the source specification."
  #-symbolics
  ;;
  ;; NOTE!  The following only admits STREAMs that have a PATHNAME
  ;; associated with them.  Higher intelligence may be needed here.
  ;;
  (when source (pathname source))
  #+symbolics
  (cond ((null source) nil)
	#+genera
	((srccom:file-p source)
	 (let* ((file (second source))
		(object (srccom:presentation-object file)))
	   (when (pathnamep object) object)))
	((operation-handled-p source :pathname) (pathname source))
	))

(defun STREAM-PRESENTATION (source)
  "Return a Presentation Object (and Type) for the source specification."
  (declare (values object type))
  (cond ((null source) nil)
	#+genera
	((srccom:file-p source)
	 (let* ((file (second source))
		(object (srccom:presentation-object file))
		(type (srccom:presentation-type file)))
	   (values object type)))
	#+symbolics
	((operation-handled-p source :pathname)
	 (let* ((object (pathname source))
		(type (type-of object)))
	   (values object type)))
	(t (values source (type-of source)))
	))

(defun STREAM-WIDTH (stream)
  "Return the width of a stream, in characters."
  (declare (special *default-width*)
           #-symbolics
           (ignore stream))
  (values (cond #+symbolics
		((operation-handled-p stream :size-in-characters)
		 (funcall stream :size-in-characters))
		(t *default-width*))))

#+ignore
(defun CIRCLE-DOUBLE (number
		      &optional (demimod (ash 1 30.)))
  "Double the number, modularly."
  (if (< number demimod)
      (ash number 1)
      (1+ (ash (- number demimod) 1))
      ))

#-:ccl
(defun CIRCLE-PLUS (a-number b-number
		    &optional (demimod (ash 1 30.)))
  "Add the two numbers, modularly."
  (if (< a-number demimod)
      (if (< b-number demimod)
	  (+ a-number b-number)
	  (let ((around (+ a-number (- b-number demimod) 1)))
	    (if (< around demimod)
		(+ a-number b-number)
		(- around demimod)
		)))
      (if (< b-number demimod)
	  (let ((bround (+ (- a-number demimod) b-number 1)))
	    (if (< bround demimod)
		(+ a-number b-number)
		(- bround demimod)
		))
	  (+ (- a-number demimod) (- b-number demimod) 1)
	  )))

(defun HASH-STRING (string
		    &optional
		    (context 0)
		    (version nil))
  "Form the system independent Hash Code for a string, in its context."
  (declare (special *default-hash-version*)
	   #-:ccl
	   (inline circle-plus)
	   (values hash version))
  (if version
      (unless (= version *default-hash-version*)
	(error "Hash Version ~D is not supported." version))
      (setq version *default-hash-version*))
  (let* (#+:ccl
	 (coefficient (1+ (ash 1 8)))
	 #-:ccl
	 (divisor (ash 1 (- 31. 8)))
	 (demimod (ash 1 30.))			; Half Hash Overflow.
	 (modulus (+ demimod (1- demimod)))
	 (hash (mod context modulus))
	 (hash-char (string string)))
    (dotimes (index (length hash-char))
      (let* (#-:ccl (last hash)
	     #-:ccl (next hash)
	     (char (char hash-char index))
	     (offset (case char
		       (#\Backspace #o010)	; Try to support ASCII,
		       (#\Tab #o011)		; but mostly just: Tab.
		       (#\Linefeed #o012)
		       (#\Page #o014)
		       (#\Return #o015)
		       (#\Rubout #o177)
		       (#\Null #o000)		; Exceed Common Lisp?
		       (#\Escape #o033)
		       (otherwise (char-code char))
		       )))
	;;
	;; The Modular Arithmetic transformations below should prevent
	;; most CL Implementations (those which support 32-bit FIXNUMs)
	;; from finding the need to resort to BIGNUMs.  The calculation
	;; is actually speeded in preventing BIGNUM Garbage Generation.
	;;
	#-:ccl
	(multiple-value-bind (quotient remainder) (truncate next divisor)
	  ;;
	  ;; Noting that the MODULUS is one less than a multiple of 256,
	  ;; first multiply the NEXT hash by 256 congruent this MODULUS.
	  ;;
	  ;; Because Powers of Two are involved, this is equivalent
	  ;; to rotating the low order 31 bit field left, by 8 bits.
	  ;;
	  ;; Since the input is less than MODULUS, at least one bit
	  ;; must be off.  At least one bit will also be off in the
	  ;; result which will therefore be less than the MODULUS!
	  ;;
	  (setq next (+ (ash remainder 8) quotient))
	  ;;
	  ;; Now finish the COEFFICIENT.
	  ;;
	  (setq next (circle-plus next last demimod))
	  ;;
	  ;; CHAR-CODE-LIMIT will certainly not exceed MODULUS,
	  ;; but the following guarantee of correctness should
	  ;; be inexpensive since CHAR-CODE-LIMIT is Constant.
	  ;;
	  (when (> char-code-limit modulus)
	    (setq offset (mod offset modulus)))
	  ;;
	  ;; Finally, add in the OFFSET.
	  ;;
	  (setq hash (circle-plus next offset demimod)))
	;;
	;; Direct implementation runs twice as fast under Coral's,
	;; which then became Franz' Allegro, but has since become
	;; Apple Common Lisp: on a Macintosh SE.
	;;
	;; The preceding transformations may also be sub-optimal
	;; under any implementation supporting 40-bit FIXNUMs.
	;;
	#+:ccl
	(setq hash (mod (+ (* hash coefficient) offset) modulus))
	))

    (values hash version)
    ))

(defun HASH-RECORDS (records
		     &optional
		     (context 0)
		     (version nil)
		     &key
		     (start 0)
		     (end nil))
  "Form the system independent Hash Code for some records, in their context."
  (declare (special *default-hash-version*)
	   (values hash version))
  (if version
      (unless (= version *default-hash-version*)
	(warn "Hash Version ~D is not supported." version)
	(setq version *default-hash-version*))
      (setq version *default-hash-version*))
  (let* ((list-p (listp records))		; Avoid EVERY of a SUBSEQ copy.
	 (remains (when list-p (nthcdr start records)))
	 (end-index (or end (unless list-p (length records))))
	 (newline (string #\Return))		; Maintain system independence.
	 (hash context))
    (do ((record-index start (1+ record-index)))
	((or (and list-p (endp remains))
	     (and end-index (>= record-index end-index))
	     ))
      (let* ((record (if list-p
			 (pop remains) (aref records record-index)
			 ))
	     (truncated (truncated-p record))
	     (line (if truncated (second record) record)))
	(setf hash (hash-string line hash version))
	;;
	;; Distinguish the two types of Record Boundary.
	;;
	(unless truncated
	  (setf hash (hash-string newline hash version)))
	))
    (values hash version)))

(defun WRITE-FRAME (records
		    &optional
		    (output-stream *standard-output*)
		    (source nil)
		    (interval ())
		    &rest keys
		    &key
		    (change-bar "" change-bar-sp)
		    (width nil width-sp)
		    &allow-other-keys)
  "Frame and then write the sequence of records to an output stream."
  (unless (nonep records)
    (let ((presentation-keys
	    (nconc
	      (when change-bar-sp (list :change-bar change-bar))
	      (when width-sp (list :end width))
	      ))
	  (records (apply #'frame-records records keys)))

      (apply #'write-records
	     records output-stream source interval presentation-keys)
      )))

(defun WRITE-RECORDS (records
		      &optional
		      (output-stream *standard-output*)
		      (source nil)
		      (interval ())
		      &rest keys)
  #+symbolics
  "Present the sequence of records, as Editor Buffer Pointer Objects."
  #-symbolics
  "Write the sequence of records to an output stream."
  #-symbolics
  (declare (ignore source interval))
  (let #-symbolics ()
       #+symbolics
       ((bp-line (first interval))
	(bp-file (when (srccom:file-p source) (second source)))
	(bp-type 'zwei:bp))
       (every #'(lambda (record)
		  (let #-symbolics ()
		       #+symbolics
		       ((bp-object
			  (when (and bp-file (srccom:file-bp-table bp-file))
			    (aref (srccom:file-bp-table bp-file) bp-line))
			  ))
		       (apply #'write-record-as-object
			      record
			      output-stream
			      #-symbolics nil #+symbolics bp-object
			      #-symbolics nil #+symbolics bp-type
			      keys)
		       #+symbolics
		       (when bp-line (incf bp-line))
		       t))
	      records))
  (not (nonep records)))

(defun WRITE-RECORD-AS-OBJECT (record
			       &optional
			       (output-stream *standard-output*)
			       (object nil)
			       (type nil)
			       &key
			       (start nil start-sp)
			       (end nil end-sp)
			       (change-bar "" change-bar-sp)
			       (truncate nil)
			       (presentation-start nil presentation-start-sp)
			       (presentation-end nil presentation-end-sp))
  "Present the specified record, as some other type of object."
  #-symbolics
  (declare (ignore object type
		   presentation-start presentation-start-sp
		   presentation-end presentation-end-sp))
  (let* ((bottom-end (when end-sp (max end 0)))
	 (change-string (string change-bar))
	 (change-length (length change-string))
	 (change-end (when (and change-bar-sp end-sp)
		       (min bottom-end change-length)))
	 (change-keys (when (and change-end (< bottom-end change-length))
			(list :end change-end)))
	 (record-end (when end-sp (if change-bar-sp
				      (- bottom-end change-end) bottom-end)))
	 (truncated (truncated-p record))
	 (line (if truncated (second record) record))
	 (string (string line))
	 (string-length (length string))
	 #+symbolics
	 (string-end (if end-sp (min record-end string-length) string-length))
	 #+symbolics
	 (start-keys (when (and start-sp (plusp start)) (list :start start)))
	 ;;
	 ;; Unlike WRITE-LINE, etc., excessive :END values are allowed.
	 ;;
	 (end-keys (when (and end-sp (< record-end string-length))
		     (list :end record-end)))
	 (subseq-keys
	   (nconc (when (and start-sp (plusp start)) (list :start start))
		  end-keys))
	 #+symbolics
	 (presentable (and object
			   (or (not presentation-start-sp)
			       (<= (if start-sp start 0)
				   presentation-start
				   (if presentation-end-sp
				       presentation-end string-end)))
			   (or (not presentation-end-sp)
			       (<= (if start-sp start 0)
				   presentation-end
				   string-end)))
		      ))
    (when change-bar-sp
      (apply #'write-string change-string output-stream change-keys))

    #+symbolics
    (if presentable
	(let ((presentation-keys (nconc (if presentation-start-sp
					    (list :start presentation-start)
					    start-keys)
					(if presentation-end-sp
					    (list :end presentation-end)
					    end-keys)
					)))
	  (when presentation-start-sp
	    (apply #'write-string
		   string output-stream :end presentation-start start-keys))

	  (dw:with-output-as-presentation (:stream output-stream
					   :object object
					   :type type)
	    (apply (if (or presentation-end-sp truncate truncated)
		       #'write-string #'write-line)
		   string output-stream presentation-keys))

	  (when presentation-end-sp
	    (apply (if (or truncate truncated) #'write-string #'write-line)
		   string output-stream :start presentation-end end-keys)
	    ))
	(apply (if (or truncate truncated) #'write-string #'write-line)
	       string output-stream subseq-keys))
    #-symbolics
    (apply (if (or truncate truncated) #'write-string #'write-line)
	   string output-stream subseq-keys)
    ))

;;;
;;; The following routine presumes the input stream
;;; can be gathered into a finite list of records.
;;;
(defun READ-RECORDS (&optional
                     (input-stream *standard-input*)
                     (eof-error-p nil)
                     (eof-value ())
                     (recursive-p nil))
  "Read the given stream into a list of records, and return them."
  (do ((records ()))
      ((multiple-value-bind (next-line line-truncated)
	   (read-line input-stream
		      eof-error-p
		      eof-value
		      recursive-p)
	 (when next-line
	   (let ((next-record (if line-truncated
				  `(truncated ,next-line) next-line)
			      ))
	     ;;
	     ;; The input records are "stacked", in reverse order.
	     ;;
	     (push next-record records)
	     ))
         (null next-line)) (nreverse records))
    ;; Null body.
    ))

(defun TRUNCATED-P (record)
  "Is this a truncated record?"
  (and (listp record)
       (eq (first record) 'truncated)))

;;;
;;; SRCCOM:FILE-P is defined here only because it is NOT a
;;; standard SRCCOM interface.  This allows COMPARE to use
;;; the interface even where SRCCOM has not been "patched".
;;;
#+genera
(defun SRCCOM:FILE-P (source)
  "Does this source contain a File Object?"
  (and (listp source)
       (eq (first source) 'srccom:file)))

;;;
;;; Module Epilogue:
;;;
(provide 'compare)				; Common LISP is losing this!
