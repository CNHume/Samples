;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SK; Base: 10 -*-
;;;
;;; Source: skb.lisp            Module: sk              Status: operational
;;;
;;; Author     Version  Edit Date       Purpose of Edit
;;; ------     -------  ---------       ---------------
;;; Chris Hume   3.3     9-Jan-94       Added KETA.
;;; Chris Hume   3.2    26-Sep-93       Added CONSP (fixing 29-Aug-93 version).
;;; Chris Hume   3.1    29-Mar-92       Added H.
;;; Chris Hume   3.0    25-Mar-92       Added TRUE and FALSE, obviating IF.
;;; Chris Hume   2.9    11-Jan-92       Added W.
;;; Chris Hume   2.8    10-Jan-92       Cleaned up peripheral Combinator Names.
;;; Chris Hume   2.7    31-Dec-91       Prepared for :STRONG Abstraction.
;;; Chris Hume   2.6    19-Dec-91       Corrected Atomic Definition Dispatch.
;;; Chris Hume   2.5    19-Dec-91       Don't count Atomic Term as Reduction.
;;; Chris Hume   2.4    16-Dec-91       Don't count Combination as Reduction.
;;; Chris Hume   2.3     7-Dec-91       Don't count Association as Reduction.
;;; Chris Hume   2.2     6-Dec-91       Count Reduction Recursion Levels.
;;; Chris Hume   2.1     5-Dec-91       Bracketed Reduction Traces.
;;; Chris Hume   2.0     1-Dec-91       SK Version 1.0, Released December 1991.
;;; Chris Hume   1.25    1-Dec-91       Provided Eager CONS, leaving PAIR Lazy.
;;; Chris Hume   1.24   30-Nov-91       Abstract via LAMBDA* Macro.
;;; Chris Hume   1.23   30-Nov-91       Count Abstractions and Reductions.
;;; Chris Hume   1.22   29-Nov-91       Threw in :NORMAL for Beta Normal Form.
;;; Chris Hume   1.21   29-Nov-91       Fixed inappropriate List Modification.
;;; Chris Hume   1.20   29-Nov-91       Cleaned up QUOTE, in support of Lists.
;;; Chris Hume   1.19   28-Nov-91       Added optimal (circular) Y-combinator.
;;; Chris Hume   1.18   27-Nov-91       Added *, /, DIV, REM, ISQRT, and ENDP.
;;; Chris Hume   1.17   26-Nov-91       Reworked Reduction Policies extensively.
;;; Chris Hume   1.16   25-Nov-91       Fixed Forwarding Pointers.
;;; Chris Hume   1.15   24-Nov-91       Fixed I-Combinator reduction sharing.
;;; Chris Hume   1.14   24-Nov-91       Added elementary Integer Arithmetic.
;;; Chris Hume   1.13   23-Nov-91       Added E-Combinator.
;;; Chris Hume   1.12   23-Nov-91       Added V-Combinator for: (S I I).
;;; Chris Hume   1.11   23-Nov-91       Fixed KNIL, PAIR, and U-combinators.
;;; Chris Hume   1.10   23-Nov-91       Averted apply of CBOUNDP to CL Term.
;;; Chris Hume   1.9    12-Nov-91       Reduced PAIR-combinator (as CONS).
;;; Chris Hume   1.8    11-Nov-91       Added User Defined Combinators.
;;; Chris Hume   1.7     9-Nov-91       Added QUOTE.
;;; Chris Hume   1.6     6-Nov-91       Rough cut at uncurry, U-combinator.
;;; Chris Hume   1.5     5-Nov-91       Included non-circular Y-combinator.
;;; Chris Hume   1.4     5-Nov-91       Rendered Operational.
;;; Chris Hume   1.3     5-Nov-91       Avoided unsafe resource management.
;;; Chris Hume   1.2     3-Nov-91       Found the end of the tunnel.
;;; Chris Hume   1.1     2-Nov-91       Reworked control, substantially.
;;; Chris Hume   1.0    31-Oct-91       Removed Beta Reduction to this File.
;;;
;;; Purpose:
;;;
;;;     Implement elements of an elementary S-K Reduction Engine.
;;;
;;; Usage:      This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:    (compile-file "SK/Source/skb.lisp")
;;;
;;; Contents:
;;;
;;;     This file implements the S-K Reduction Interface.
;;;
;;; Reduction Operator:
;;;
;;;     (beta expr)
;;;
;;; Local Procedures:
;;;
;;;     sk-eval                 lefts
;;;                             &keys count trace warn normal optimize strong
;;;

;;;
;;; Module Prologue:
;;;
(in-package sk)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; The Reduction Operation:
;;;
(defmacro BETA (expr &rest keys &key (count *count*) &allow-other-keys)
  "Reduce, under the SK Evaluation Model."
  (declare (special *count*))
  `(let ((*reduction-count* 0)
         (*reduction-level* 0))
     (declare (special *reduction-count* *reduction-level*))
     (prog1
       (sk-eval ',expr . ,keys)
       ,(when count
          `(format t "~&[Performed ~D reduction~:P.]~%" *reduction-count*))
       )))

;;;
;;; Primitive Combinators:
;;;
;;;     (K t1 t2)               => t1
;;;     (S t1 t2 t3)            => (t1 t3 (t2 t3))
;;;
;;; Experimental Primitive:
;;;
;;;     (KETA t1 t2 t3)         => (t1 t3)
;;;
;;; Elementary "Optimizers:"
;;;
;;;     (B t1 t2 t3)            => (t1 (t2 t3))
;;;     (C t1 t2 t3)            => (t1 t3 t2)
;;;     (E t1 t2)               => (t2 t1)
;;;     (H t1 t2)               => t2
;;;     (I t1)                  => t1
;;;     (V t1)                  => (t1 t1)
;;;     (W t1 t2)               => (t1 t2 t2)
;;;
;;; Un-implemented A-Combinator [for addition of Church Numerals]:
;;;
;;;     (A t1 t2 t3 t4)         => (t1 t3 (t2 t3 t4))
;;;
;;; Uncurry, the U-Combinator, in conjuction with the PAIR-combinator:
;;;
;;;     (U f (PAIR t1 t2))      => (f t1 t2)
;;;
;;; Optimized Y-combinator:
;;;
;;;     (Y t1)                  => (t1 (Y t1))
;;;
;;; Functional Booleans:
;;;
;;;     (FALSE t1 t2)           => t2
;;;      (TRUE t1 t2)           => t1
;;;
;;; Superfluous Conditional Combinator [kept for backward compatibility]:
;;;
;;;     (IF FALSE t1 t2)        => t2
;;;     (IF  TRUE t1 t2)        => t1
;;;
;;; List Predicates:
;;;
;;;       CONSP list            == (consp  list)
;;;       ENDP list             == (endp   list)
;;;
;;; Integer Predicates:         [Actually, Rationals are allowed.]
;;;
;;;      ZEROP integer          == (zerop  integer)
;;;      PLUSP integer          == (plusp  integer)
;;;     MINUSP integer          == (minusp integer)
;;;
;;; NOTE: The following "Integer" Operations actually support Rationals.
;;;       All of these operators map Integers into Integers, apart from
;;;       / which MAY produce a Rational.  DIV "truncates" towards Zero.
;;;
;;; Integer Operations:
;;;
;;;     +   integer1 integer2   = (+ integer1 integer2)
;;;     -   integer1 integer2   = (- integer1 integer2)
;;;     *   integer1 integer2   = (* integer1 integer2)
;;;    REM  integer1 integer2   = (rem integer1 integer2)
;;;    DIV  integer1 integer2   = (truncate (/ integer1 integer2))
;;;   ISQRT integer             = (truncate (sqrt integer))
;;;
(defun SK-EVAL (expr &rest keys
                     &key
                     (print-circle *default-print-circle*)
                     ;;;#+:ccl
                     (print-level *default-print-level*)
                     (count *count*) (trace *trace*) (warn *warn*)
                     (normal *normal*)          ; Interior Redex Disposition
                     &allow-other-keys)
  "Evaluate the specified CL Term, fully, employing normal order reduction."
  (declare (special *reduction-count* *reduction-level* *default-print-circle*
                    ;;;#+:ccl
                    *default-print-level*
                    *count* *trace* *warn* *normal*))
  (when *reduction-level*
    (incf *reduction-level*)
    (when trace
      (format t "~%[Begin a reduction at level ~D.]" *reduction-level*)))
  ;;
  ;; Elements left on the Left Ancestors Stack: LEFTS, correspond
  ;; to compound terms awaiting reduction of their own Left Terms.
  ;;
  (do ((lefts ())                               ; The empty list is written as
       (stop nil))                              ;() vs. NIL for Boolean Values.
      (stop
       ;;
       ;; At this point, the terms left stacked on LEFTS have been
       ;; determined to be irreducible.  Each of their Right Terms,
       ;; however, may be reducible.  Recurse to reduce each RIGHT
       ;; sub-expression.
       ;;
       (dolist (pop-expr lefts)
         (let ((ap-first expr)
               (ap-second (if (or (not normal) (eq (second pop-expr) pop-expr))
                            (second pop-expr)   ; Note Y-Combinator vestige.
                            (apply #'sk-eval (second pop-expr) keys))))
           (setq expr pop-expr)
           (setf (first expr) (if (consp ap-first) (first ap-first) ap-first)
                 (rest expr) `(,@(when (consp ap-first) (rest ap-first))
                               ,ap-second))
           ))
       ;;
       ;; Trace Recursive Beta Reductions:
       ;;
       (when *reduction-level*
         (when trace
           (format t "~&[End the reduction at level ~D.]" *reduction-level*)
           (let ((*print-circle* print-circle)  ; Cf. Diagnostics below.
                 ;;;#+:ccl
                 (*print-level* print-level))
             (pprint expr)))
         (decf *reduction-level*))
       ;;
       ;; Return the Beta Reduction:
       ;;
       expr)
    ;;
    ;; Diagnostic Section:
    ;;
    (when *reduction-count* (incf *reduction-count*))
    (when trace                                 ; Trace Reductions.
      (when count
        (format t "~&[")
        (when *reduction-count* (format t "Redex ~2D, " *reduction-count*))
        (format t "Left ~2D]" (length lefts)))  ; Show stack depth.
      (let ((*print-circle* print-circle)       ; Show circularity.
            ;;;#+:ccl                           ; Allegro CL needed this:
            (*print-level* print-level))        ; Handle (Y QUOTE).
        (pprint `(beta ,expr))
        ))
    ;;
    ;; Redex Dispatcher:
    ;;
    (if (or (atom expr)
            (not (listp (rest expr))))          ; Catch ill-formed CL-Terms.
      (progn
        (unless (atom expr) (warn "Improperly formed CL-Term: ~S." expr))
        (setq stop t)
        (when *reduction-count*                 ; Doesn't count as Reduction.
          (decf *reduction-count*)))
      (let ((ap-first (first expr))
            (ap-rest (rest expr)))
        (if #+sk-traverse (endp ap-rest) #-sk-traverse nil
          ;;
          ;; A CL-Term is not supposed to be wrapped in gratuitous
          ;; parentheses, but traverse any that may be encountered:
          ;;
          (progn
            (when warn (warn "Beta Parenthesized CL-Term: ~S." expr))
            (setq expr ap-first)
            (when *reduction-count*             ; Doesn't count as Reduction.
              (decf *reduction-count*)))            
          (let ((ap-second (second expr))
                (ap-more (rest ap-rest)))
            (if ap-more
              ;;
              ;; This performs Implicit Left Association, but
              ;; without diagnosing ill-formed (dotted) tails:
              ;;
              (let ((ap (reduce #'list expr)))
                (setf (first expr) (first ap)     ; Maintain Shared Evaluation.
                      (rest expr) (rest ap))
                (when *reduction-count*           ; Doesn't count as Reduction.
                  (decf *reduction-count*)))
              (case ap-first
                ;;
                ;; If the head corresponds to a known combinator
                ;; ensure there are enough elements on the Left
                ;; Ancestors Stack, LEFTS, that reduction can be
                ;; performed.  Where there are not sufficient
                ;; ancestors stacked, to supply the arguments an
                ;; outer reduction requires, internal reductions
                ;; will be performed only if NORMAL was asserted:
                ;;
                ((S B C KETA)
                  (if (endp (rest lefts))        ; Require 3 Arguments.
                    (progn (push expr lefts) (setq expr (first expr)))
                    (let ((term-2 (second (pop lefts))))
                      (setq expr (pop lefts))
                      (let ((last-3 (rest expr)))
                        ;;
                        ;; In order to benefit from evaluation of common
                        ;; sub-terms, the LAST-3 CONS must survive until
                        ;; the first such evaluation occurs.
                        ;;
                        (setf (first expr) (case ap-first
                                            ((S C) `(,ap-second . ,last-3))
                                            (otherwise ap-second)))
                        (unless (eq ap-first 'KETA)
                          (setf (rest expr) `(,(if (eq ap-first 'C)
                                                term-2 `(,term-2 . ,last-3))))
                          ))
                      )))
                ;;
                ;; I-combinators will be handled below, as "Forwarding
                ;; Pointers".  The top-level CONS must remain consistent,
                ;; in case the S-combinator introduces shared sub-terms.
                ;;
                ((E FALSE H K KNIL TRUE)
                  (if (endp lefts)                ; Require 2 Arguments.
                    (progn (push expr lefts) (setq expr (first expr)))
                    (progn
                      (setq expr (pop lefts))     ; KNIL will only discard NIL.
                      (if (or (not (eq ap-first 'KNIL))
                              (null (apply #'sk-eval (second expr) keys)))
                        (setf (first expr) (if (eq ap-first 'E) (second expr) 'i)
                              (rest expr) (case ap-first
                                            ((FALSE H) (rest expr))
                                            (otherwise ap-rest)))
                        (progn (push expr lefts)
                              (setq expr (first expr))
                              (push expr lefts)
                              (setq expr (first expr))
                              ))
                      )))
                (W  (if (endp lefts)              ; Require 2 Arguments.
                      (progn (push expr lefts) (setq expr (first expr)))
                      (setf (first expr) (second expr)
                            (rest expr) (rest (first lefts))
                            )))
                ((PAIR CONS)                      ; PAIR is Lazy & CONS is Eager.
                  (if (endp lefts)                ; Require 2 Arguments.
                    (progn (push expr lefts) (setq expr (first expr)))
                    (progn
                      (setq expr (pop lefts))
                      (let ((arg-1 ap-second) (arg-2 (second expr)))
                        (if (eq ap-first 'PAIR)
                          (setq expr `(,arg-1 . ,arg-2) stop t)
                          (setf (first expr) 'quote
                                (rest expr) `((,(apply #'sk-eval arg-1 keys) .
                                              ,(apply #'sk-eval arg-2 keys)))
                                )))
                      )))
                ((QUOTE) (setq expr ap-second     ; Leave value on Stack.
                                stop t))
                ((I IF)
                  (if (endp lefts)
                    (setq expr ap-second)         ; Leave value on Stack or
                    (progn                        ;elide Forwarding Pointer.
                      (setq expr (pop lefts))
                      (setf (first expr) ap-second))
                    ))
                (V (setf (first expr) ap-second))
                (Y (setf (first expr) ap-second   ; The Fixed Point Combinator
                          (rest expr) `(,expr)))
                (U (if (endp lefts)               ; Require 2 Arguments.
                      (progn (push expr lefts) (setq expr (first expr)))
                      (progn
                        (setq expr (pop lefts))
                        (let ((args (apply #'sk-eval (second expr) keys)))
                          (if (atom args)
                            (progn (push expr lefts)
                                  (setq expr (first expr))
                                  (push expr lefts)
                                  (setq expr (first expr)))
                            ;;
                            ;; Pop an argument and apply AP-SECOND to it:
                            ;;
                            (let ((arg (pop args)))
                              (setf (first expr) ap-second
                                    (rest expr) `(,arg ,args))
                              )))
                        )))
                (LAMBDA*                          ; Reduce Internal Abstractions.
                  (if (endp lefts)                ; Require 2 Arguments.
                    (setq stop t)
                    (progn
                      (setq expr (pop lefts))
                      (setf (first expr) 'i       ; Preserve Abstraction.
                            (rest expr)           ; Implement via LAMBDA* Macro.
                            `(,(eval `(lambda* ,ap-second ,(second expr) . ,keys)
                                    )))
                      )))
                ;;
                ;; An "Interpretive Domain" of elementary operations
                ;; in Integer Arithmetic is included, to demonstrate
                ;; practical application of the S-K Reduction Engine:
                ;;
                ((ZEROP PLUSP MINUSP CONSP ENDP)  ;(Using Common LISP symbols.)
                  (let ((arg (apply #'sk-eval ap-second keys)))
                    (if (or (eq ap-first 'CONSP)
                            (funcall (case ap-first
                                      (ENDP #'listp)
                                      (otherwise #'rationalp))
                                    arg))
                      (setf (first expr) 'i       ; Form Combinatory Equivalent:
                            (rest expr) `(,(if (funcall ap-first arg)
                                            'TRUE 'FALSE)))
                      (progn (push expr lefts) (setq expr (first expr)))
                      )))
                ((ISQRT)                          ;(Using Common LISP symbols.)
                  (let ((arg (apply #'sk-eval ap-second keys)))
                    (if (and (rationalp arg) (not (minusp arg)))
                      (setf (first expr) 'i       ; Preserve Evaluated Atom.
                            (rest expr) `(,(isqrt (floor arg))))
                      (progn (push expr lefts) (setq expr (first expr)))
                      )))
                ((+ - * / DIV REM)
                  (if (endp lefts)                ; Require 2 Arguments.
                    (progn (push expr lefts) (setq expr (first expr)))
                    (progn
                      (setq expr (pop lefts))
                      (let ((arg-1 (apply #'sk-eval ap-second keys))
                            (arg-2 (apply #'sk-eval (second expr) keys)))
                        (if (and (rationalp arg-1)
                                (rationalp arg-2)
                                (not (case ap-first ((/ DIV REM) (zerop arg-2))
                                            )))
                          (setf (first expr) 'i   ; Preserve Evaluated Atom.
                                (rest expr) `(,(funcall (if (eq ap-first 'DIV)
                                                          #'truncate ap-first)
                                                        arg-1 arg-2)))
                          (progn (push expr lefts)
                                (setq expr (first expr))
                                (push expr lefts)
                                (setq expr (first expr)))
                          )))
                    ))
                (otherwise                        ; Reduce a compound Left Term.
                  (if (and (symbolp ap-first) (cboundp ap-first))
                    ;;
                    ;; Attempts to redefine Primitive Combinators were ignored.
                    ;;
                    (setf (first expr) (cdefinition ap-first))
                    (progn (push expr lefts) (setq expr ap-first)))
                  (when *reduction-count*         ; Doesn't count as Reduction.
                    (decf *reduction-count*)))
                ))
          ))
        ))                                      ; End of Redex Dispatcher
    ))

;;;
;;; Module Epilogue:
;;;
(provide 'sk)                                   ; Common LISP is losing this!
