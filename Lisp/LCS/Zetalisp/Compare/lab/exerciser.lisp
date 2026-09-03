;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: exerciser.lisp	Module: compare		Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 1.3	30-May-90	Can you spell "exercise"?
;;; Chris Hume	 1.2	18-May-90	Added :FUNCTION keywords.
;;; Chris Hume	 1.1	 8-May-90	Added SYMMETRY tester.
;;; Chris Hume	 1.0	 6-May-90	Created file.
;;;
;;; Purpose:	Exercise the Sequence Comparison Utility, for performance.
;;;
;;; Usage:	This file assumes the Symbolics Common Lisp Environment.
;;;
;;; Compile:	(compile-file "compare:compare;exerciser")
;;;
;;; Contents:
;;;
;;;	exerciser		alphabet-length
;;;				&key debug-log-entry randomize function
;;;
;;;	symmetry		&key alphabet-length improbability
;;;				short-length type function
;;;

;;;
;;; Module Prologue:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.

;;;
;;; The External Interfaces:
;;;
(export '(exerciser symmetry))

;;;
;;; Now for the Code:
;;;
(defun EXERCISER (alphabet-length
		  &rest keys
		  &key
		  (randomize nil)
		  (function #'compare:common-sequence)
		  &allow-other-keys)
  "Exercise the Sequence Comparison Utility, for perfomance."
  (let* ((test-length (expt alphabet-length 2))
	 (a-sequence (make-array test-length)))
    (do ((index 0 (1+ index)))
	((= index test-length) a-sequence)
      (let ((element (if randomize
			 (random alphabet-length)
			 (mod index alphabet-length)
			 )))
	(setf (svref a-sequence index) element)
	))

    (let ((b-sequence (reverse a-sequence)))
      (apply function a-sequence b-sequence keys)
      )))

(defun SYMMETRY (&rest keys
		 &key
		 (alphabet-length 26)
		 (improbability 16)
		 (short-length 52)
		 (type 'list)
		 (function #'compare:common-sequence)
		 &allow-other-keys)
  "Test the Sequence Comparison Utility, for performance assymetry."
  (let* ((long-length (* alphabet-length improbability short-length))
         (short-sequence
	   (make-random-sequence type short-length alphabet-length))
         (long-sequence
	   (make-random-sequence type long-length alphabet-length))
         (forward-common
	   (apply function short-sequence long-sequence
		  :time-log-entry t keys))
         (reverse-common
	   (apply function long-sequence short-sequence
		  :time-log-entry t keys)))
    (values forward-common
	    reverse-common)
    ))
