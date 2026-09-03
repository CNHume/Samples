;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-dynamic.lisp  Module: compare	Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 1.1	 3-Nov-90	Added this header.
;;; Chris Hume	 1.0	 2-May-90	Gathered performance data.
;;;
;;; Purpose:	Implement the "Standard" Dynamic Programming (LCS) algorithm.
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;	   level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;	basic-common-pairs	a-sequence b-sequence &key
;;;				ignore-case-and-style ignore-whitespace
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

(defun BASIC-COMMON-PAIRS (a-sequence b-sequence
			   &rest keys
			   &key
			   (debug-log-entry nil)
			   &allow-other-keys)
  "Match elements in the longest subsequence common to two sequences, simply."
  (let* ((a-length (length a-sequence))
         (b-length (length b-sequence))
         (ab-matches (apply #'list-matches
                            a-sequence b-sequence :order-up t keys))
         (distance (make-array b-length :initial-element 0))
         (pairs-back (make-array b-length :initial-element ()))
         (pair-count 0))
    (do ((a-position 0 (1+ a-position))
         (ab-common-remains ab-matches (rest ab-common-remains)))
        ((= a-position a-length)
         (values (unless (zerop b-length)	; Match Pair Sequence Found
                   (reverse (svref pairs-back (1- b-length))))
                 (cons a-length b-length)	; Input Sequence Length Pair
                 (if (zerop b-length)		; Common Subsequence Length
		     0 (svref distance (1- b-length)))
                 pair-count))			; Number of Log Entry Pairs
      (let ((diagonal-distance 0)
            (vertical-distance 0)
            (diagonal-pairs ())
            (vertical-pairs ()))
        (do ((b-position 0 (1+ b-position))
             (b-common-remains (first ab-common-remains)))
            ((= b-position b-length))
          (let ((horizontal-distance (svref distance b-position))
                (horizontal-pairs (svref pairs-back b-position))
                (matched (and b-common-remains
                              (= (first b-common-remains) b-position))))
            
            (if matched
		(let ((next-pair (cons a-position b-position)))
		  (setq b-common-remains (rest b-common-remains))
		  (setf (svref distance b-position) (1+ diagonal-distance))
		  (setf (svref pairs-back b-position)
			(list* next-pair diagonal-pairs))
		  (incf pair-count)
		  (when debug-log-entry
		    (format t "~&i =~3D, j =~3D, distance = ~D, pairs = ~S~%"
			    a-position
			    b-position
			    diagonal-distance
			    diagonal-pairs)
		    ))
		(when (< horizontal-distance vertical-distance)
		  (setf (svref distance b-position) vertical-distance)
		  (setf (svref pairs-back b-position) vertical-pairs)
		  ))
            
            (setf diagonal-distance horizontal-distance)
            (setf vertical-distance (svref distance b-position))
            
            (setf diagonal-pairs horizontal-pairs)
            (setf vertical-pairs (svref pairs-back b-position))
            ))
        ))
    ))

;;;
;;; Suspend Module Context:
;;;
