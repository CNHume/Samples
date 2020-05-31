;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;
;;; Source: skt.lisp		Module: sk	Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 3.6	 9-Dec-92	Added MAP2.
;;; Chris Hume	 3.5	 1-Jun-92	Incorporated Boolean Primitives.
;;; Chris Hume	 3.4	 1-Jun-92	Added MASK, POSITIONS, and MAP*.
;;; Chris Hume	 3.3	31-May-92	Added SUBSEQ, FIRSTN and BUTFIRSTN.
;;; Chris Hume	 3.2	30-May-92	Added NTH, MAX and MIN.
;;; Chris Hume	 3.1	30-May-92	Added NEXT, explored its application.
;;; Chris Hume	 3.0	27-Mar-92	Added LENGTH.
;;; Chris Hume	 2.6	18-Dec-91	Removed Combinator Laboratory to SKC.
;;; Chris Hume	 2.5	16-Dec-91	Added Boolean Primitives.
;;; Chris Hume	 2.4	10-Dec-91	Added Y-Combinator Laboratory.
;;; Chris Hume	 2.3	10-Dec-91	Added Conditionality Implementation.
;;; Chris Hume	 2.2	 6-Dec-91	Renamed List Operations.
;;; Chris Hume	 2.1	 2-Dec-91	Added REVERSE.
;;; Chris Hume	 2.0	 1-Dec-91	SK Version 1.0, Released December 1991.
;;; Chris Hume	 1.5	 1-Dec-91	Removed TIMES from this file.
;;; Chris Hume	 1.4	 1-Dec-91	Cleaned up REDUCEL vs. REDUCER.
;;; Chris Hume	 1.3	29-Nov-91	Reduced REDUCER.
;;; Chris Hume	 1.2	27-Nov-91	Added REDUCER.
;;; Chris Hume	 1.1	24-Nov-91	Reduced TIMES.
;;; Chris Hume	 1.0	11-Nov-91	Created file.
;;;
;;; Purpose:
;;;
;;;	Illustrate extension of the S-K Reduction Engine, in SASL itself.
;;;
;;; Usage:	This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:	(compile-file "SK/Source/skt.lisp")
;;;
;;; Contents:
;;;
;;;	This file demonstrates a number of higher order
;;;	utility functions for the S-K Reduction Engine.
;;;

;;; No Requirements.
;;; No Shadows.
(eval-when (compile) (use-package 'sk))
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Now for the Code:
;;;
(defc HEAD [head tail]head)
(defc TAIL [head tail]tail)

(defc NEXT [id][fn][args](if (endp args) id (fn args)))

(defc UNTIL0
  (next nil [head tail](if (zerop head) nil (pair head (until0 tail)))))

(defc NTH [n](next nil [head tail](if (zerop n) head (nth (- n 1) tail))))

(defc PREFER [old][fn]
  (next old [new tail](prefer (if (fn (- new old)) new old) fn tail)))

(defc MAX [head tail](prefer head plusp tail))
(defc MIN [head tail](prefer head minusp tail))

(defc BUTFIRSTN [n][args]
  (next nil [head tail](if (zerop n) args (butfirstn (- n 1) tail)) args))

(defc REDUCENR [id][fn][n]
  (next id [head tail](if (zerop n)
                        id
                        (fn head (reducenr id fn (- n 1) tail))
                        )))

(defc FIRSTN (reducenr nil pair))

(defc SUBSEQ [start][stop][args](firstn (- stop start) (butfirstn start args)))

(defc REDUCEL [id][fn](next id [head tail](reducel (fn id head) fn tail)))
(defc REDUCER [id][fn](next id [head tail](fn head (reducer id fn tail))))

;;;
;;; Try: (beta (difference (pair 5 (pair 2 (pair 4 nil)))))
;;;
(defc DIFFERENCE [head tail](reducel head - tail))

(defc SUM (reducer 0 +))

(defc LENGTH (reducer 0 [head](+ 1)))

(defc EVALIST (reducer nil cons))
(defc REVERSE (reducel nil [tail][head](pair head tail)))

(defc APPEND [prefix][suffix](reducer suffix pair prefix))

;;;
;;; Boolean Primitives:
;;;
(defc AND2 [one][two](if one two false))
(defc  OR2 [one][two](if one true two))

(defc  OR* (reducel false or2))
(defc AND* (reducel true and2))

(defc MAP [fn](reducer nil [head](pair (fn head))))

(defc MAP2 [fn][args1][args2]
  (if (or2 (endp args1) (endp args2))
    nil
    ([head1 tail1][head2 tail2](pair (fn head1 head2) (map2 fn tail1 tail2))
            args1 args2)
    ))

(defc MAP* [fn][arglists]
  (if (or (map endp arglists))
    nil
    ([heads][tails](pair (fn heads) (map* fn tails))
                   (map head arglists) (map tail arglists))
    ))

(defc DOUBLES (map (* 2)))

(defc MASK (masker 0))
(defc MASKER [n][args]
  (next nil
        [head tail]([ep](pair ep (masker (+ n 1) (if ep tail args)))
                        (zerop (- head n)))
        args))

(defc POSITIONS (positioner 0))
(defc POSITIONER [n](next nil [head tail]([ns](if head (pair n ns) ns)
                                              (positioner (+ n 1) tail))))
