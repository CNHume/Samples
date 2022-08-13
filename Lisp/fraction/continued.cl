;;; -*- Mode: LISP; Syntax: Common-lisp; Package: fraction; Base: 10 -*-
;;;
;;; Function:
;;;
;;; 	The following includes Continued Fraction support.
;;;
;;; Author:
;;;
;;; 	Chris Hume	14-Jun-1988
;;;
;;; Use:
;;;

;;;
;;; Module Prologue:
;;;
(defpackage fraction)
;;;(in-package fraction)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Now for the Code:
;;;
(defun COERCE-RATIO (number &rest keys)
  "Represent any real valued input as a ratio."
  (let* ((expansion (apply #'continued number keys))
         (ratio (discontinued expansion)))
    ratio))

(defun CONTINUED (number &key (tolerance 0)
                         &allow-other-keys)
  "Represent any real valued input as a continued fraction."
  (let* ((threshhold (abs (/ (* tolerance number) 100)))
         (expansion (continued-threshhold number threshhold)))
    expansion))

(defun CONTINUED-THRESHHOLD (number &optional (threshhold 0))
  "Iteratively reduce a real valued input to a continued fraction."
  (do ((numerator number)
       (denominator 1)
       (expansion ())
       (continue-done nil))
      (continue-done expansion)
    
    (multiple-value-bind (quotient remainder)
        (floor numerator denominator)
      
      (setq expansion (nconc expansion (list quotient))
          numerator denominator
          denominator remainder))
    
    (let* ((ratio (discontinued expansion))
           (error (abs (- number ratio))))
      (setq continue-done (<= error threshhold)))
    ))

(defun DISCONTINUED (terms)
  "Convert a continued fraction to a rational number."
  (reduce #'(lambda (term reduction)
              (if reduction (+ term (/ reduction)) term))
          terms
          :from-end t))

;;;
;;; Module Prologue:
;;;
;;;(provide 'fraction)
;;;
;;; Example:
;;;
;;;(use-package :fraction)
;;;(coerce-ratio 1.05669 :tolerance 1)
