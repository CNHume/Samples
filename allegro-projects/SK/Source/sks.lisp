;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;
;;; Source: sks.lisp		Module: sk	Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 1.1	26-Sep-93	First Working Version
;;; Chris Hume	 1.0	29-Aug-93	Created file.
;;;
;;; Purpose:
;;;
;;;	Illustrate extension of the S-K Reduction Engine, in SASL itself.
;;;
;;; Usage:	This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:	(compile-file "SK/Source/sks.lisp")
;;;
;;; Contents:
;;;
;;;	This file demonstrates uses of a Continuation
;;;	Passing Style (CPS) of Functional Programming.
;;;

;;; No Requirements.
;;; No Shadows.
(eval-when (compile) (use-package 'sk))
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Now for the Code:
;;;
(defc EQLP [left][right](zerop (- left right)))

(defc SAME-FRINGEP (everyp2-tree eqlp))

(defc EOF [con](con nil FALSE beyond-the-fringe))

(defc GEN-TREE [tree][consumer][genrest]
  (if (consp tree)
    (Y [yme][branch][consume](if (endp branch)
                               (genrest consume)
                               (gen-tree (head branch)
                                         consume
                                         [con](yme (tail branch) con)))
       tree
       consumer)
    (consumer tree TRUE genrest)
    ))

(defc EVERYP2-TREE [fun][xtree][ytree]
  (Y [yme][xgen][ygen]
     (xgen [xtree][more-x][xg]
           (ygen [ytree][more-y][yg]
                 (if (or2 more-x more-y)
                   (and2 (and2 more-x more-y)
                         (and2 (fun xtree ytree) (yme xg yg)))
                   TRUE)
                 ))
     [con](gen-tree xtree con eof)
     [con](gen-tree ytree con eof)
     ))

;;;
;;; Try: (same-fringep
;;;         (pair 1 (pair (pair 2 (pair 3 nil)) (pair 4 nil)))
;;;         (pair (pair 1 (pair 2 nil)) (pair (pair 3 (pair 4 nil)) nil)))
;;;
