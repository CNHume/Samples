;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-body.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Implement the SEQUENCE COMPARISON utility's central algorithm.
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	The following functions implement the efficient algorithm for
;;;	finding the longest subsequence common to two input sequences.
;;;	In this context, "subsequence" refers not only to contiguous
;;;	subsequences (as obtained via the SUBSEQ operation) but to any
;;;	(possibly discontiguous) sequentially ordered set of elements.
;;;
;;;	The running time is O((r + s) log s), where r is the number
;;;	of ordered pairs of positions where the two sequences match,
;;;	and s is the maximum of (m and n) the two input sequence lengths.
;;;
;;;	Performance will therefore be O(s^2 log s) in the worst case,
;;;	but is O(s log s) where most positions of one sequence match
;;;	relatively few positions in the other.
;;;
;;;	A reference describing this algorithm is provided below
;;;	[Hunt and Szymanski, Communications of the ACM, May 1977].
;;;	It is in essence a refinement of the "standard" Dynamic
;;;	Programming method, which unfortunately is O(r + s) in
;;;	space (and therefore necessarily also in time.)  Reference
;;;	to an alternative refinement, requiring O(mn) time but
;;;	confining its space requirement to O(m + n), is provided
;;;	also [Hirschberg, Communications of the ACM, June 1975].
;;;
;;;	An interesting advantage to this alternative is that it would
;;;	retain the Dynamic Programming method's ability to obtain a
;;;	minimum "edit distance" between two sequences, where this
;;;	distance is defined as the total cost accrued for each of the
;;;	elements individually paired across the two input sequences.
;;;
;;;	To permit wide application, even where an input sequence
;;;	might vary to be of great length, the "efficient" algorithm
;;;	(achieving better than polynomial expected time) has been
;;;	implemented.  This algorithm restricts the cost of any pair
;;;	of unequal elements to all be of the same non-zero value.
;;;
;;;	The UNIX "diff" utility developed (in C) by James W. Hunt and
;;;	M. Douglas McIlroy uses a closely related variant of this
;;;	algorithm, but the following LISP implementation is based on
;;;	the approach described in the [Hunt and Szymanski] reference.
;;;
;;;	While this algorithm guarantees efficient identification of a
;;;	common subsequence of maximum possible length, considerations
;;;	such as minimization of subsequence "discontiguity" or other
;;;	biasing toward one among several equally maximal subsequences
;;;	have not as yet been provided for.
;;;
;;; References:
;;;
;;;	"A Fast Algorithm for Computing Longest Common Subsequences"
;;;	by James W. Hunt and Thomas G. Szymanski, published May 1977
;;;	Communications of the ACM [Volume 20, Number 5, pp. 350-353]
;;;
;;;	"A Linear Space Algorithm for Computing Maximal Common
;;;	Subsequences" by Daniel S. Hirschberg, published June 1975
;;;	Communications of the ACM [Volume 18, Number 6, pp. 341-343]
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Interfaces Wrapped for Portability:
;;;
;;;	noting-progress		(name &optional variable process) &body body
;;;
;;;	note-progress		numerator &optional denominator note
;;;
;;; Local Interfaces:
;;;
;;;	sequence-type-of	sequence
;;;
;;;	nonep			sequence
;;;
;;;	normalize-element	element &key
;;;				ignore-case-and-style ignore-whitespace
;;;
;;;	list-matches		a-sequence b-sequence &key order-up
;;;				ignore-case-and-style ignore-whitespace
;;;
;;;	binary-position		item array predicate &key key start end
;;;
;;;	basic-common-pairs	a-sequence b-sequence &key method
;;;				ignore-case-and-style ignore-whitespace
;;;

;;;
;;; Resume Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
#+symbolics
(import '(scl:string-thin))

;;;
;;; Calling BINARY-POSITION as an Inline Function from BASIC-COMMON-PAIRS
;;; achieves a 30% speed increase in typical comparisons, but achieves as
;;; great as a 50% speed increase in the most complex cases.
;;;
(proclaim '(inline binary-position))

;;;
;;; Dynamically Bound Interfaces:
;;;
(export '(*space-characters*))

;;;
;;; Interfaces Wrapped for Portability:
;;;
(defmacro NOTING-PROGRESS (args &body body)
  "Wrap the Progress Noting forms, portably."
  #-symbolics
  (declare (ignore args))
  #-symbolics
  `(progn . ,body)
  #+symbolics
  `(tv:noting-progress ,args . ,body))

(defun NOTE-PROGRESS (&rest args)
  "Note Progress, portably."
  #-symbolics
  (declare (ignore args))
  #+symbolics
  (apply #'tv:note-progress args))

;;;
;;; Special Variables and Constants:
;;;
;;; NOTE! "Element Normalization" is introduced here to implement
;;;	  the :IGNORE-WHITESPACE and :IGNORE-CASE-AND-STYLE keywords.
;;;
(defparameter *COMPARE-NORMALIZER* 'normalize-element
  "The Standard Sequence Element Normalizer")

(defparameter *SPACE-CHARACTERS* '(#\Space #\Tab)
  "The characters which may be ignored as space")

(defparameter *REDUNDANCY-RATIO* 6
  "The Redundancy Heuristic Overhead Threshold")

;;;
;;; Now for the Code:
;;;
;;; The following are introduced primarily to allow
;;; comparison of sequence types without regard for
;;; their length, or other "sub-type" distinctions.
;;;
(defun SEQUENCE-TYPE-OF (sequence)
  "Return the sequence's (essential) type."
  (ctypecase sequence
    (null 'null)
    (list 'list)
    (simple-string 'simple-string)
    (string 'string)
    (simple-bit-vector 'simple-bit-vector)
    (bit-vector 'bit-vector)
    (simple-vector 'simple-vector)
    (vector 'vector)))

(defun NONEP (sequence)
  "Determine whether the sequence contains no elements."
  (ctypecase sequence
    (list (endp sequence))
    (vector (zerop (length sequence)))
    ))

(defun NORMALIZE-ELEMENT (element
                          &key
                          (ignore-case-and-style nil)
                          (ignore-whitespace nil)
                          &allow-other-keys)
  "Normalize element as requested via ignore options."
  (declare (special *space-characters*))
  (let* ((truncated (truncated-p element))
         (line (if truncated (second element) element)))
    
    (when (or ignore-case-and-style ignore-whitespace)
      ;;
      ;; Only records acceptable to STRING are admitted here:
      ;;
      (let ((line-type (if (or (characterp line)
			       (pathnamep line)
			       (symbolp line))
			   (type-of line) (sequence-type-of line)))
            (normalization (if (pathnamep line)
			       (namestring line) (string line))
			   ))
        
        ;;
        ;; "Trim" the string first, reducing its length.
        ;;
        (when ignore-whitespace
          (setq normalization (string-trim *space-characters* normalization)))
        
        ;;
        ;; Normalize case UP, reducing new symbol generation where
        ;; string normalization applies to elements of type SYMBOL.
        ;;
        (when ignore-case-and-style
          (setq normalization (string-upcase normalization))
          
          ;;
          ;; NOTE!  The enclosed STRING-THIN form should apply
          ;; wherever style is supported via "fat" strings.
          ;;
          #+symbolics
          (setq normalization (string-thin normalization)))
        
        ;;
        ;; Restore the correct type after any String Coercion.
        ;;
        (setq line (cond ((pathnamep line) (pathname normalization))
			 ;; Admit SYMBOL too.
			 ((symbolp line) (intern normalization))
			 (t (coerce normalization line-type))
			 ))
        ))
    
    (if truncated
	`(truncated ,line . ,(rest element)) line)
    ))

;;;
;;; The following function returns a list of lists.  Each of the sub-lists
;;; is associated with a position along the A-SEQUENCE, and provides those
;;; positions along the B-SEQUENCE (in descending order) which match this
;;; A-SEQUENCE element.
;;;
;;; The total number of element pairs the two sequences have in common is
;;; returned also, as the second value.
;;;
(defun LIST-MATCHES (a-sequence
                     b-sequence
                     &rest keys
                     &key
		     (order-up nil)
                     &allow-other-keys)
  "Return (ordered) position sub-lists of B elements equal to each A."
  (declare (special *compare-normalizer*))
  (let* ((a-length (length a-sequence))
	 (b-length (length b-sequence))
	 (progress-length (+ a-length b-length))
         (b-table (when (plusp b-length)
                    (make-hash-table :test #'equal :size b-length))))
    (noting-progress ("Locating Equivalent Elements")
      (when b-table
	(let ((b-index 0))
	  (every #'(lambda (b-element)
		     (push b-index (gethash (apply *compare-normalizer*
						   b-element keys)
					    b-table))
		     (incf b-index)
		     (note-progress b-index progress-length)
		     t)
		 b-sequence))
      
	(maphash
	  #'(lambda (element-class b-indices)
	      (when order-up
		(setf (gethash element-class b-table) (nreverse b-indices)))
	      ;;
	      ;; The following augmentation to the basic algorithm permits
	      ;; efficient identification of the case (where the sequences
	      ;; have a large number of element pairs in common) in which
	      ;; the comparison takes much time.
	      ;;
	      ;; It is a necessary, but insufficient, condition that a
	      ;; large number of match pairs exist before the algorithm
	      ;; will require much space.
	      ;;
	      (push (length b-indices) (gethash element-class b-table)))
	  b-table))
    
      (let* ((progress-index b-length)
	     (match-count 0)
	     (ab-matches
	       (map 'list
		    #'(lambda (a-element)
			(when b-table
			  (let ((count-and-indicies
				  (gethash (apply *compare-normalizer*
						  a-element keys)
					   b-table)))
			    (when count-and-indicies
			      (incf match-count (first count-and-indicies)))
			    (incf progress-index)
			    (note-progress progress-index progress-length)
			    (rest count-and-indicies)
			    )))
		    a-sequence)
	       ))

	(values ab-matches match-count)
	))
    ))

;;;
;;; The following function PERFORMS A BINARY SEARCH OVER AN ARRAY,
;;; sorted according to the specified predicate, for the position
;;; at which the specified item would also satisfy this predicate.
;;;
;;; Assuming the array has indeed been sorted as specified, no element
;;; preceding this position satisfies the predicate and all elements
;;; from this position through the end of the array do satisfy the
;;; predicate with respect to the specified item.
;;;
(defun BINARY-POSITION (item array predicate &key (key nil) (start 0) (end nil))
  "Find the position for an item within a sorted array, efficiently."
  #+(or ansi-cl symbolics)
  (declare #+ansi-cl
	   (dynamic-extent predicate)
	   #+(and (not ansi-cl) symbolics)
	   (sys:downward-funarg predicate))
  (let ((start-position start)
        (end-position (or end (length array))))
    (if (<= start-position end-position (length array))
	(do ((item-position nil))
	    ((>= start-position end-position) item-position)
	  (let* ((array-position (truncate (+ start-position end-position) 2))
		 (array-element (aref array array-position))
		 (array-item (if key
				 (funcall key array-element)
				 array-element)))
	    (if (funcall predicate item array-item)
            
		;;
		;; The position item is "strictly" less than the current
		;; array item: so search towards the array start.
		;;
		(setq end-position (setq item-position array-position))
            
		;;
		;; The position item is greater than or equal to the current
		;; array item: so search towards the array end.
		;;
		(setq start-position (1+ array-position)))
	    ))
	(error "Invalid bounds: start = ~S, end = ~S; for array ~S"
	       start-position end-position array)
	)))

;;;
;;; The function below implements the algorithm discussed under
;;; "Contents", above.  PLEASE CONSULT THE REFERENCE cited there
;;; [Hunt and Szymanski, Communications of the ACM, May 1977] for
;;; explanation of the algorithm (and for additional references.)
;;;
;;; The algorithm can be viewed as a modification of the "standard"
;;; O(mn) Dynamic Programming algorithm for maximizing the "magnitude"
;;; of matching sequentially ordered elements from the two sequences.
;;; In the present case the magnitude will only increase either by
;;; One for those letters of the "alphabet" which match, or by Zero
;;; for non-matching letters.
;;;
;;; This simplifying assumption allows the improved algorithm to achieve
;;; its distinctive performance advantage over the standard algorithm.
;;;
(defun BASIC-COMMON-PAIRS (a-sequence b-sequence
			   &rest keys
			   &key
			   (method *default-compare-method*)
			   (optimize-log-entry t)
			   (debug-method nil)
			   &allow-other-keys)
  "Match elements in the longest subsequence common to two sequences, quickly."
  (declare (special *redundancy-ratio* *default-compare-method*)
	   (values matched-pairs length-pair lcs-length
		   pair-count match-count work-done)
	   (inline binary-position))
  (let ((a-length (length a-sequence))
	(b-length (length b-sequence))
	(work-done :matched))
    (multiple-value-bind (ab-matches match-count)
	(apply #'list-matches a-sequence b-sequence :order-up nil keys)
      
      ;;
      ;; The case where two equivalent (and redundant) sequences are passed
      ;; occurs frequently enough that special optimization is warranted.
      ;;
      ;; It is preferable to confine use of this, and other such heuristics,
      ;; to those cases where redundancy impairs performance.  The criterion
      ;; that the number of Matched Pairs exceed a multiple (e.g., 6) of
      ;; the [longer] sequence length ensures that extra overhead is only
      ;; incurred where its cost is low with respect to potential benefit.
      ;;
      ;; A Symbolics 3620 can process 5K pairs/second.  Performance would
      ;; therefore seem acceptable as long as the number of Matched Pairs
      ;; remains on the order of One Million.
      ;;
      (cond
	((and (eq method :any)			; This is allowed by default.
	      (= a-length b-length)		; MAX unnecessary under this.
	      (< (* b-length *redundancy-ratio*) match-count)
	      (flet ((EQUIVALENT (a-element b-element)
		       (declare (special *compare-normalizer*))
		       "Normalize and test an element pair for equality."
		       (equal
			 (apply *compare-normalizer* a-element keys)
			 (apply *compare-normalizer* b-element keys))
		       ))
		(every #'equivalent a-sequence b-sequence)
		))
	 ;;
	 ;; Fake up return values for a Perfect Match:
	 ;;
	 (do* ((pair-index b-length (1- pair-index))
	       (pair-stack () (acons pair-index pair-index pair-stack)))
	      ((zerop pair-index)
	       (values pair-stack		; Match Pair Sequence Found
		       (cons a-length b-length)	; Input Sequence Length Pair
		       b-length			; Common Subsequence Length
		       b-length			; Number of Log Entry Pairs
		       match-count		; Number of Matching Pairs
		       work-done))		; The Work Done
	   ;; Null body.
	   ))

	(t
	 ;;
	 ;; Here follows implementation of the actual algorithm:
	 ;;
	 (let ((threshold (make-array (1+ a-length)
				      :initial-element b-length))
	       (log-entry (make-array a-length
				      :initial-element ()))
	       (pair-count 0)
	       (trip-count 0))
	   (noting-progress ("Maximizing Common Subsequence")
	     ;;
	     ;; After each A-POSITION iteration THRESHOLD[C-POSITION] contains
	     ;; the smallest B-POSITION such that A-SEQUENCE[0:A-POSITION] and
	     ;; B-SEQUENCE[0:B-POSITION] have a common C-POSITION subsequence.
	     ;;
	     (do ((a-position 0 (1+ a-position))
		  (ab-common-remains ab-matches (rest ab-common-remains)))
		 ((endp ab-common-remains)
		  (let ((c-position (binary-position b-length threshold #'<=)))
		    (values
		      (when (plusp c-position)	; Match Pair Sequence Found
			(reverse (svref log-entry (1- c-position))))
		      (cons a-length b-length)	; Input Sequence Length Pair
		      c-position		; Common Subsequence Length
		      pair-count		; Number of Log Entry Pairs
		      match-count		; Number of Matching Pairs
		      work-done)))		; The Work Done
	       (do ((b-common-remains
		      (first ab-common-remains) (rest b-common-remains))
		    (c-limit (1+ a-position)))	; Expedite the binary searches.
		   ((endp b-common-remains))
		 ;;
		 ;; There will be a total of MATCH-COUNT trips through here.
		 ;;
		 (incf trip-count)
		 (let* ((b-position (first b-common-remains))
			(b-lower (second b-common-remains))
			(c-position (binary-position b-position
						     threshold
						     #'<=
						     :end c-limit)))
		   (setq c-limit (1+ c-position))
		   (when
		     (and (< b-position (svref threshold c-position))
			  ;;
			  ;; The following "log optimization" was not included
			  ;; in the [Hunt and Szymanski] algorithm but has the
			  ;; effect of eliminating some unnecessary overhead.
			  ;;
			  ;; The optimization's contribution increases as the
			  ;; sequences become increasingly alike.  Therefore,
			  ;; it improves worst case behaviour only trivially.
			  ;;
			  (not (and optimize-log-entry
				    b-lower
				    (or (not (plusp c-position))
					(> b-lower
					   (svref threshold (1- c-position))))
				    )))
		     (let ((last-entry (when (plusp c-position)
					 (svref log-entry (1- c-position))))
			   (next-pair (cons a-position b-position)))
		       (when debug-method
			 (format t
				 "~&i =~3D, j =~3D < THRESHOLD[k =~3D] =~3D~%"
				 a-position b-position c-position
				 (svref threshold c-position)))
		       (setf (svref threshold c-position) b-position)
		       (setf (svref log-entry c-position)
			     (cons next-pair last-entry))
		       (incf pair-count)
		       ))
		   ))
	       (note-progress trip-count match-count)
	       ))
	   )))
      )))

;;;
;;; Suspend Module Context:
;;;
