;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; Source: skd.lisp            Module: sk      Status: operational
;;;
;;; Author     Version  Edit Date       Purpose of Edit
;;; ------     -------  ---------       ---------------
;;; Chris Hume   3.5     5-Jul-21       Corrected abstractions of TAIL.
;;; Chris Hume   3.4     2-Aug-20       Distinguished Abstraction from Currying.
;;; Chris Hume   3.3    14-Dec-92       Added FBUTFIRSTN.
;;; Chris Hume   3.2     9-Dec-92       Added FBUTFIRSTM, FIB.
;;; Chris Hume   3.1     7-Dec-92       Added FIBEXP, FIBDAT.
;;; Chris Hume   3.0    30-May-92       Adjusted FACTORIAL, FOR, and TWICE.
;;; Chris Hume   2.4    31-Jan-92       Shortened SIEVE.
;;; Chris Hume   2.3    10-Jan-92       Cleaned up peripheral Combinator Names.
;;; Chris Hume   2.2    18-Dec-91       Renamed file from SK-DEMO to SKD.
;;; Chris Hume   2.1     6-Dec-91       Cleaned up some variable names.
;;; Chris Hume   2.0     1-Dec-91       SK Version 1.0, Released December 1991.
;;; Chris Hume   1.2     1-Dec-91       Moved TIMES into this file.
;;; Chris Hume   1.1     1-Dec-91       Demonstrated Lazy Evaluation.
;;; Chris Hume   1.0    30-Nov-91       Created file.
;;;
;;; Purpose:
;;;
;;;     Demonstrate correct operation of the S-K Reduction Engine.
;;;
;;; Usage:      This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:    (compile-file "SK/Source/skd.lisp")
;;;
;;; Contents:
;;;
;;;     This test file for the S-K Reduction Engine, contains
;;;     renditions of D.A. Turner's "SASL Test Programs".
;;;

;;;
;;; Module Prologue:
;;;
(in-package cl-user)

;;; No Requirements.
;;; No Shadows.
(eval-when (compile load) (use-package 'sk))
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Try: (beta (hanoi 4 a b c))
;;;
(defc HANOI ?n ?a ?b ?c
  (if (zerop n)
    nil (cons (hanoi (- n 1) a c b)
              (cons "Move a disc from"
                    (cons a
                          (cons "to"
                                (cons b (hanoi (- n 1) c b a)
                                      )))))
    ))

(defc FACTORIAL ?n (if (plusp n) (* (factorial (- n 1)) n) 1))

(defc FOR ?from ?to ?op
  (if (plusp (- from to))
    nil (cons (op from) (for (+ 1 from) to op))
    ))

;;;
;;; Try: (beta (twice twice twice (+ 1) 0))
;;;
(defc TWICE ?op ?arg (op (op arg)))

;;;
;;; Demonstrate Lazy Evaluation:
;;;
(defc FROM ?n (pair n (from (+ 1 n))))

(defc FILTER ?divisor ?dividends
  (?(head . tail)(if (zerop (rem head divisor))
                   (filter divisor tail) (pair head (filter divisor tail)))
         dividends))

;;;
;;; Try: (beta (head (tail (tail (tail (sieve (from 2)))))))
;;;
(defc SIEVE ?ns
  (?(divisor . dividends)(pair divisor (sieve (filter divisor dividends))) ns))

(defc FIBEXP ?n
  (if (zerop n)
    0 (?m (if (zerop m)
            1 (+ (fibexp m) (fibexp (- m 1))))
          (- n 1))))

(defc FIBDAT ?n
  (if (zerop n)
    0 (?m (if (zerop m)
            1 (?es (+ (head (tail es)) (head es))
                   (butfirstn (- m 1) (map fibdat (from 0)))
                   ))
          (- n 1))))

(defc FBUTFIRSTM ?n
  (if (zerop n)
    (pair 0 (fbutfirstm 1))
    (?m (if (zerop m)
          (pair 1 (fbutfirstm 2))
          (map2 + (fbutfirstm (- m 1)) (fbutfirstm m)))
        (- n 1))
    ))

(defc FBUTFIRSTN ?n     ; This is just a "fool proof" version of FBUTFIRSTM.
  (if (zerop n)
    (pair 0 (fbutfirstn 1))
    (?m (if (zerop m)
          (pair 1 (fbutfirstn 2))
          (?o (if (zerop o)
                (map2 + (fbutfirstn 0) (fbutfirstn 1))
                (butfirstn o (fbutfirstn 2)))
              (- m 1)))
        (- n 1))))

(defc FIB ?n (nth n (fbutfirstm 0)))
