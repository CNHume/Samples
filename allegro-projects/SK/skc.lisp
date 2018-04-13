;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;
;;; Source: skc.lisp		Module: sk	Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 3.0	 1-Jun-92	Removed the Boolean Primitives, to SKT.
;;; Chris Hume	 2.2	27-Mar-92	Included Ackermann's Function.
;;; Chris Hume	 2.1	26-Mar-92	Added Church Pairing Combinator.
;;; Chris Hume	 2.0	25-Mar-92	Implemented TRUE and FALSE, internally.
;;; Chris Hume	 1.6	19-Jan-92	Added T and P.
;;; Chris Hume	 1.5	19-Jan-92	Added Q and R.
;;; Chris Hume	 1.4	13-Jan-92	Straightened out CASE.
;;; Chris Hume	 1.3	10-Jan-92	Cleaned up peripheral Combinator Names.
;;; Chris Hume	 1.2	19-Dec-91	Demonstrated Integer Arithmetic.
;;; Chris Hume	 1.1	19-Dec-91	Defined Primitive Combinators in USER.
;;; Chris Hume	 1.0	18-Dec-91	Created file.
;;;
;;; Purpose:
;;;
;;;	Demonstrate operation of the S-K Reduction Engine.
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	(compile-file "sk:sk;skc.lisp")
;;;
;;; Contents:
;;;
;;;	This file is a Laboratory for experimental Combinators.
;;;
;;; No Requirements.
;;; No Shadows.
(eval-when (compile) (use-package 'sk))
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Now for the Code:
;;;
(define-primitives)

;;;
;;; Y-Combinator Laboratory:
;;;
;;; [p.222 in Ch.10 "Stack-based implementations - the SECD machine",
;;;  of "Functional Programming" by Anthony Field and Peter Harrison,
;;;  Imperial College of London Press, 1988]
;;;
(defc Y-CURRY1 [h]([z](h (z z)) [z](h (z z))))
(defc Y-CURRY2 [h]([z](h [x](z z x)) [z](h [x](z z x))))
(defc Y-GNIRUT [h]([z](h (z z h)) [z](h (z z h))))
;;;(defc Y-TURING ([z][h](h (z z h)) [z][h](h (z z h))))
(defc Z [z][h](h (z z h)))
(defc Y-TURING (Z Z))

;;;
;;; Church Pairing Combinator:
;;;
(defc dc [x][y][z](z x y))
(defc hd [u](u [a][b]a))
(defc tl [u](u [a][b]b))

;;;
;;; Pairing Combinator [for Recursions below]:
;;;
(defc DASE [x][y][z](z (k y) x))

;;;
;;; Distribution Operators:
;;;
(defc PHI [f][g][h][x](f (g x) (h x)))
(defc PSI [f][g][m][n](f (g m) (g n)))

;;;
;;; Integer Arithmetic
;;;
;;; Note: A is addition, B is multiplication, and E is exponentiation.
;;;
(defc A [x][y][z][u](x z (y z u)))

(defc ZERO [x][y]y)
(defc SUCC [x][y][z](y (x y z)))

(defc XZERO [x](dase (succ (x zero)) (x zero)))
(defc PRED [y](y xzero (k zero) (succ zero)))
(defc DEMINUS [x][y](y pred x))

(defc Q [u][v](dase (succ (v zero))
                    (u (v zero) (v (succ zero)))
                    ))

(defc R [x][y][z](z (q y) (dase zero x) (succ zero)))

(defc T [x](dase zero
                 [u][v](u (x (succ v)) u (succ v))
                 ))

(defc P [x][y](t x (x y) (t x) y))

;;;
;;; Ackermann's Total [yet non-Primitive] Recursive Function:
;;;
(defc AM [x](an x x))

(defc AN [x][y](zerop x (inc y)
                      (zerop y (an (dec x) 1)
                             (an (dec x) (an x (dec y)))
                             )))

(defc DEC [z](- z 1))
(defc INC [z](+ z 1))
