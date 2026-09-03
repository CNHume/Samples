;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-interval.lisp  Module: compare	Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the SEQUENCE COMPARISON utility with support
;;;		for various operators over "Interval Lists".
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:	The following includes a number of fundamental sequence
;;;		operations and transformations.  Among these: support
;;;		for the :PREFIX and :SUFFIX options is provided here.
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;	subseq-intervals	a-sequence b-sequence interval-pairs
;;;
;;;	fasten-intervals	interval-pairs length-pair &key transfix
;;;
;;;	affix-intervals		interval-pairs length-pair &key prefix suffix
;;;
;;;	complement-intervals	interval-pairs length-pair
;;;
;;;	final-interval		interval-pairs length-pair
;;;
;;;	interval-pairs		pairs
;;;
;;;	interval-led-p		interval-pair exterval-pair
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
;;; NOTE!  With individual respect to each of the sequences, correspondence
;;; and difference intervals will be "strictly ascending".  Thus subsequence
;;; operation optimization is warranted here, since application to long
;;; sequences is anticipated.  In order to preclude repetitive traversal
;;; of subsequence "remains", a base position along sequences (of type LIST)
;;; is maintained.
;;;
;;; This reduces O(sp) overhead un-optimized SUBSEQUENCE SEARCHES would
;;; introduce to: O(s), even where p (the number of correspondence or
;;; difference intervals) might in some cases attain the maximum of O(s).
;;;
;;; For p above O(log s), repeated SUBSEQ overhead would have otherwise
;;; interfered with expected performance of the central algorithm.
;;;
(defun SUBSEQ-INTERVALS (a-sequence b-sequence interval-pairs)
  "Extract the subsequence pairs as specified by the interval pairs."
  (let ((a-position (when (listp a-sequence) 0))
        (b-position (when (listp b-sequence) 0))
        (a-remains (when (listp a-sequence) a-sequence))
        (b-remains (when (listp b-sequence) b-sequence)))
    (mapcar #'(lambda (interval-pair)
		(let ((a-interval (car interval-pair))
		      (b-interval (cdr interval-pair))
		      (a-subseq ())
		      (b-subseq ()))
               
		  (when a-interval
		    (if a-position		; (LISTP A-SEQUENCE)?
			(let* ((a-start (- (first a-interval) a-position))
			       (a-end (- (second a-interval) a-position))
			       (a-subseq-length (- a-end a-start)))
			  (setf a-remains (nthcdr a-start a-remains))
			  (setq a-subseq (subseq a-remains 0 a-subseq-length))
			  (setf a-remains (nthcdr a-subseq-length a-remains))
			  (incf a-position a-end))
			(let ((a-start (first a-interval))
			      (a-end (second a-interval)))
			  (setq a-subseq (subseq a-sequence a-start a-end)))
			))

		  (when b-interval
		    (if b-position		; (LISTP B-SEQUENCE)?
			(let* ((b-start (- (first b-interval) b-position))
			       (b-end (- (second b-interval) b-position))
			       (b-subseq-length (- b-end b-start)))
			  (setf b-remains (nthcdr b-start b-remains))
			  (setq b-subseq (subseq b-remains 0 b-subseq-length))
			  (setf b-remains (nthcdr b-subseq-length b-remains))
			  (incf b-position b-end))
			(let ((b-start (first b-interval))
			      (b-end (second b-interval)))
			  (setq b-subseq (subseq b-sequence b-start b-end)))
			))

		  (cons a-subseq b-subseq)
		  ))
	    interval-pairs)))

(defun FASTEN-INTERVALS (interval-pairs
                         length-pair
                         &key (transfix 0)
                         &allow-other-keys)
  "Fasten adjacent intervals, if both pairs lie within the specified transfix."
  (if (and interval-pairs transfix)
      (let* ((a-length (car length-pair))
	     (b-length (cdr length-pair))
	     (a-terminal (list a-length a-length))
	     (b-terminal (list b-length b-length))
	     (terminal-pair (cons a-terminal b-terminal))
	     (last-a-start 0)
	     (last-b-start 0)
	     (last-a-end 0)
	     (last-b-end 0)
	     (intraval-pending nil))
	(do ((interval-pair-remains (append interval-pairs (list terminal-pair))
				    (rest interval-pair-remains))
	     (intraval-pairs ()))
	    ((endp interval-pair-remains) (values (nreverse intraval-pairs)
						  length-pair))
	  (let* ((intraval-broken nil)
		 (interval-pair (first interval-pair-remains))
		 (next-interval-pair-remains (rest interval-pair-remains))
		 (a-interval (car interval-pair))
		 (b-interval (cdr interval-pair))
		 (a-start (first a-interval))
		 (a-end (second a-interval))
		 (b-start (first b-interval))
		 (b-end (second b-interval))
		 (a-gap (- a-start last-a-end))
		 (b-gap (- b-start last-b-end))
		 (a-intraval ())
		 (b-intraval ()))
	    ;;
	    ;; Intervals will be "transfixed", through the intervening "gap",
	    ;; if adjacent interval pairs, on BOTH sequences, lie within the
	    ;; "transfix" specified.
	    ;;
	    (when (or (> a-gap transfix) (> b-gap transfix))
	      (setq intraval-broken t))
          
	    ;;
	    ;; Allow transfixion through the "ends" of either sequence.
	    ;;
	    (unless intraval-broken
	      (when (and (zerop last-a-end) (zerop last-b-end))
		(setq intraval-pending t))
            
	      (when intraval-pending
		(setq last-a-end a-end
		      last-b-end b-end)))
          
	    (when (or intraval-broken (endp next-interval-pair-remains))
	      (when intraval-pending
		(setq a-intraval (list last-a-start last-a-end)
		      b-intraval (list last-b-start last-b-end))
		;;
		;; The intraval pairs are "stacked", in reverse order.
		;;
		(push (cons a-intraval b-intraval) intraval-pairs)
		(setq intraval-pending nil)))
          
	    (unless intraval-pending
	      (setq last-a-start a-start
		    last-b-start b-start
		    last-a-end a-end
		    last-b-end b-end
		    intraval-pending t))
	    )))
      (values interval-pairs length-pair)))

(defun AFFIX-INTERVALS (interval-pairs
                        length-pair
                        &key
                        (prefix 0)
                        (suffix 0)
                        &allow-other-keys)
  "Affix the specified number of records to each interval pair."
  (let ((a-length (car length-pair))
        (b-length (cdr length-pair)))
    (do ((a-position 0)
         (b-position 0)
         (last-a-end 0)
         (last-b-end 0)
         (superval-pairs ())
         (interval-pair-remains interval-pairs (rest interval-pair-remains)))
        ((endp interval-pair-remains) (values (nreverse superval-pairs)
                                              length-pair))
      (let* ((interval-pair (first interval-pair-remains))
             (a-interval (car interval-pair))
             (b-interval (cdr interval-pair))
             (next-interval-pair (second interval-pair-remains))
             (next-a-interval (car next-interval-pair))
             (next-b-interval (cdr next-interval-pair))
             (a-superval ())
             (b-superval ()))
        
        ;;
        ;; The Affix Precedence Rules are implemented here.  Their purpose
        ;; is to prevent the confusion that might arise if sequence elements
        ;; were to be presented more than once (from overlapping intervals),
        ;; or if elements from one interval were to be represented as
        ;; originating from some other interval.
        ;;
        ;;  1) Prefices must not precede a Previous Suffix (or Interval) End.
        ;;  2) Negative Prefices must not succeed the Current Interval End.
        ;;  3) Suffices must not succeed a Subsequent Interval Start.
        ;;  4) Negative Suffices must not precede the Current Prefix
        ;;     (or Interval) Start.
        ;;
        ;; A first alternative might have been to omit these precedence
        ;; rules entirely, but these support a "non-overlapping" model.
        ;; Among non-overlapping models these rules further provide a
        ;; "bounded" as opposed to a "floating" model: where affices would
        ;; have been allowed to range freely beyond their immediate context.
        ;; In the floating model additional rules would then be needed
        ;; to grant "local" affices precedence over "remote" affices.
        ;;
        ;; This bounded model is less complicated and should be
        ;; generally more useful than the floating alternative.
        ;; Its rules are implemented by the MAX and MIN forms below,
        ;; which occur (lexically) according to their numerical order.
        ;;
        (when a-interval
          (let* ((a-start (first a-interval))
                 (a-end (second a-interval))
                 (next-a-start (first next-a-interval))
                 (super-a-start (max (min (- a-start prefix) a-end)
                                     a-position
                                     last-a-end))
                 (super-a-end (min (max (+ a-end suffix)
                                        super-a-start
                                        a-start)
                                   (or next-a-start a-length))))
            
            (when (<= super-a-start super-a-end)
              (setq a-superval (list super-a-start super-a-end)))
            
            (setq last-a-end a-end
                  a-position super-a-end)
            ))
        
        (when b-interval
          (let* ((b-start (first b-interval))
                 (b-end (second b-interval))
                 (next-b-start (first next-b-interval))
                 (super-b-start (max (min (- b-start prefix) b-end)
                                     last-b-end
                                     b-position))
                 (super-b-end (min (max (+ b-end suffix)
                                        super-b-start
                                        b-start)
                                   (or next-b-start b-length))))
            
            (when (<= super-b-start super-b-end)
              (setq b-superval (list super-b-start super-b-end)))
            
            (setq last-b-end b-end
                  b-position super-b-end)
            ))
        
        (when (or a-superval b-superval)
          (push (cons a-superval b-superval) superval-pairs))
        ))
    ))

(defun COMPLEMENT-INTERVALS (interval-pairs length-pair &rest keys)
  "Given an interval sequence, return its complementary interval sequence."
  (declare (ignore keys))
  (let* ((a-length (car length-pair))
         (b-length (cdr length-pair))
         (a-terminal (list a-length a-length))
         (b-terminal (list b-length b-length))
         (terminal-pair (cons a-terminal b-terminal))
	 (exterval-pairs ())
         (a-position 0)
         (b-position 0))
    (dolist (interval-pair (append interval-pairs (list terminal-pair))
			   (values (nreverse exterval-pairs) length-pair))
      (let ((interval-broken nil)
	    (a-interval (car interval-pair))
	    (b-interval (cdr interval-pair))
	    (a-exterval ())
	    (b-exterval ()))
        
        (when a-interval
          (let ((a-start (first a-interval))
                (a-end (second a-interval)))
            (when (and (plusp a-start)
                       (< a-position a-length)
                       (<= a-position a-start))
              (setq interval-broken t))
            (setq a-exterval (list a-position a-start))
            (setq a-position a-end)))
        
        (when b-interval
          (let ((b-start (first b-interval))
                (b-end (second b-interval)))
            (when (and (plusp b-start)
                       (< b-position b-length)
                       (<= b-position b-start))
              (setq interval-broken t))
            (setq b-exterval (list b-position b-start))
            (setq b-position b-end)))
        
        (when interval-broken
          (push (cons a-exterval b-exterval) exterval-pairs))
        ))
    ))

;;;
;;; The following routine is provided in support of the Pair Writer Interface.
;;; It provides certain of these output formatters convenient access to the
;;; FINAL (which comes after the LAST) pair of subsequence intervals.
;;;
(defun FINAL-INTERVAL (interval-pairs length-pair)
  "Return the final interval pair."
  (let* ((last-interval (first (last interval-pairs)))
         (a-start (if last-interval (second (car last-interval)) 0))
         (b-start (if last-interval (second (cdr last-interval)) 0))
         (a-end (car length-pair))
         (b-end (cdr length-pair))
         (a-final (list a-start a-end))
         (b-final (list b-start b-end)))
    (cons a-final b-final)
    ))

(defun INTERVAL-PAIRS (pairs)
  "Convert a sequence of pairs into a sequence of intervals."
  (do ((a-start)
       (b-start)
       (interval-pending nil)
       (a-index 0 (1+ a-index))
       (b-index 0 (1+ b-index))
       (interval-pairs ())
       (matched-pair-remains pairs (rest matched-pair-remains)))
      ((endp matched-pair-remains) (nreverse interval-pairs))
    (let* ((interval-broken nil)
           (matched-pair (first matched-pair-remains))
           (next-matched-pair-remains (rest matched-pair-remains))
           (a-position (car matched-pair))
           (b-position (cdr matched-pair)))
      
      ;;
      ;; Check both sequences for a "contiguity break".
      ;;
      (if interval-pending
	  (unless (and (= a-position a-index) (= b-position b-index))
	    (setq interval-broken t))
	  (setq interval-pending t
		a-index (setq a-start a-position)
		b-index (setq b-start b-position)))
      
      ;;
      ;; Once the longest common subsequence demands that contiguity
      ;; be broken within either subsequence, the interval currently
      ;; being accumulated must be emitted and a new one begun.
      ;;
      (when interval-broken
        (let ((a-interval (list a-start a-index))
              (b-interval (list b-start b-index)))
          
          (setq a-index (setq a-start a-position)
                b-index (setq b-start b-position))
        
          ;;
          ;; The interval pairs are "stacked", in reverse order.
          ;;
          (push (cons a-interval b-interval) interval-pairs)))
      
      (when (and (endp next-matched-pair-remains) interval-pending)
        (let ((a-interval (list a-start (1+ a-index)))
              (b-interval (list b-start (1+ b-index))))
        
          (push (cons a-interval b-interval) interval-pairs)))
      )))

(defun INTERVAL-LED-P (interval-pair exterval-pair)
  "Determine whether the interval leads the exterval."
  (and interval-pair
       (or (null exterval-pair)
           (let* ((a-interval (car interval-pair))
                  (b-interval (cdr interval-pair))
                  (a-exterval (car exterval-pair))
                  (b-exterval (cdr exterval-pair))
                  (a-interval-start (first a-interval))
                  (b-interval-start (first b-interval))
                  (a-exterval-start (first a-exterval))
                  (b-exterval-start (first b-exterval)))
             (or (< b-interval-start b-exterval-start)
                 (and (= b-interval-start b-exterval-start)
                      (< a-interval-start a-exterval-start))))
           )))

;;;
;;; Suspend Module Context:
;;;
