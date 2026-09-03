;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-hybrid.lisp  Module: compare	Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 1.1	 3-Nov-90	Added this header.
;;; Chris Hume	 1.0	 2-May-90	Gathered performance data.
;;;
;;; Purpose:	Upgrade "compare-space.lisp" to an Hybrid LCS algorithm.
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
;;;	common-length		a-sequence b-sequence &key
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

(defun COMMON-LENGTH (a-sequence b-sequence
		      &rest keys
		      &key
		      (length-pair nil length-pair-sp)
		      (ab-matches () ab-matches-sp)
		      (distance nil distance-sp)
		      &allow-other-keys)
  "Obtain the lengths of any longest common subsequence, quickly."
  (let* ((a-length (if length-pair-sp (car length-pair) (length a-sequence)))
         (b-length (if length-pair-sp (cdr length-pair) (length b-sequence)))
         (ab-matches (if ab-matches-sp
			 (list* "Too bad... Absolute positions need to be offset!"
				ab-matches)
			 (apply #'list-matches
				a-sequence b-sequence :order-up nil keys)))
         (distance (if distance-sp distance (make-array b-length)))
         (threshold (make-array (1+ a-length) :initial-element b-length)))
    (do (;;
         ;; After each A-POSITION iteration THRESHOLD[C-POSITION] contains
         ;; the smallest B-POSITION such that A-SEQUENCE[0:A-POSITION] and
         ;; B-SEQUENCE[0:B-POSITION] have a common C-POSITION subsequence.
         ;;
         (a-position 0 (1+ a-position))
         (ab-common-remains ab-matches (rest ab-common-remains)))
        ((= a-position a-length)		; Not: (ENDP AB-COMMON-REMAINS).
         (do ((b-index 0 (1+ b-index))
              (c-index 0))
             ((= b-index b-length) c-index)
           (let ((b-threshold (svref threshold c-index)))
             (unless (< b-index b-threshold) (incf c-index)))           
           (setf (aref distance b-index) c-index)
           ))					; Common Subsequence Length
      (do ((b-common-remains (first ab-common-remains) (rest b-common-remains))
           (c-limit (1+ a-position)))		; Expedite the binary searches.
          ((endp b-common-remains))
        (let* ((b-position (first b-common-remains))
               (c-position (binary-position b-position
                                            threshold
                                            #'<=
                                            :end c-limit)))
          (setq c-limit (1+ c-position))
          (when (< b-position (svref threshold c-position))
            (setf (svref threshold c-position) b-position)))
        ))
    ))

;;;
;;; Suspend Module Context:
;;;
