;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SK; Base: 10 -*-
;;;
;;; Source: ska.lisp            Module: sk              Status: operational
;;;
;;; Author     Version  Edit Date       Purpose of Edit
;;; ------     -------  ---------       ---------------
;;; Chris Hume   2.10    9-Jan-94       Added KETA (:EXPERIMENAL optimization).
;;; Chris Hume   2.9    25-Nov-93       Added ?-READER.
;;; Chris Hume   2.8    24-Nov-93       Corrected use of W in ABBREV-S.
;;; Chris Hume   3.1    29-Mar-92       Added H.
;;; Chris Hume   3.0    25-Mar-92       Added TRUE and FALSE, obviating IF.
;;; Chris Hume   2.8    11-Jan-92       Added W.
;;; Chris Hume   2.7    10-Jan-92       Cleaned up peripheral Combinator Names.
;;; Chris Hume   2.6    31-Dec-91       Prepared for :STRONG Abstraction.
;;; Chris Hume   2.5    20-Dec-91       Added DEFINE-PRIMITIVES.
;;; Chris Hume   2.4    19-Dec-91       SK Version 1.0.3, beyond ASU's CSE 555.
;;; Chris Hume   2.3     7-Dec-91       Don't count Association as Abstraction.
;;; Chris Hume   2.2     6-Dec-91       Count Reduction Recursion Levels.
;;; Chris Hume   2.1     4-Dec-91       DIV is more Interface, than Combinator.
;;; Chris Hume   2.0     1-Dec-91       SK Version 1.0, Released December 1991.
;;; Chris Hume   1.18    1-Dec-91       Added Combinator Abstraction Warning.
;;; Chris Hume   1.17   30-Nov-91       Count Abstractions and Reductions.
;;; Chris Hume   1.16   28-Nov-91       Added recursion support to DEFC.
;;; Chris Hume   1.15   28-Nov-91       Added SK-IMPLEMENTATION-VERSION.
;;; Chris Hume   1.14   27-Nov-91       Added DIV and [] Syntax Diagnostic.
;;; Chris Hume   1.13   24-Nov-91       Allowed for :STANDARD abbreviation.
;;; Chris Hume   1.12   23-Nov-91       Added E, finishing ABBREV-S series.
;;; Chris Hume   1.11   23-Nov-91       Added V-Combinator for: (S I I).
;;; Chris Hume   1.10   23-Nov-91       Reworked CDEFINITION into Accessor.
;;; Chris Hume   1.9    11-Nov-91       Added Combinator Definition Macros.
;;; Chris Hume   1.8     9-Nov-91       Added QUOTE.
;;; Chris Hume   1.7     7-Nov-91       Generalized abstraction, suitably.
;;; Chris Hume   1.6     6-Nov-91       Messed about with the Keyword Passing.
;;; Chris Hume   1.5     5-Nov-91       Renamed a few symbols for consistency.
;;; Chris Hume   1.4    31-Oct-91       Removed Beta Reduction from this file.
;;; Chris Hume   1.3    25-Oct-91       Coded the Combinators: (S K I B C).
;;; Chris Hume   1.2    24-Oct-91       Began Reduction.
;;; Chris Hume   1.1    22-Oct-91       Finished Abstraction.
;;; Chris Hume   1.0    20-Oct-91       Created file.
;;;
;;; Purpose:
;;;
;;;     Implement elements of an elementary S-K Reduction Engine.
;;;     This code was developed in connection with ASU's CSE 555
;;;     "Automata Theory" taught by Prof. Ed Ashcroft, Fall 1991.
;;;
;;; Usage:      This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:    (compile-file "SK/Source/ska.lisp")
;;;
;;; Contents:
;;;
;;;     This file implements the S-K Reduction Abstraction Interfaces.
;;;
;;; References:
;;;
;;;     "A New Implementation Technique for Applicative Languages" by
;;;     D.A. Turner, published 1979, Software-Practice and Experience
;;;     [vol.9, pp.31-49] John Wiley & Sons, Ltd.
;;;
;;; External Macros:
;;;
;;;     defc                    c def   
;;;
;;; The Abstraction Operator:
;;;
;;;     [var] expr              = (lambda* var expr)
;;;
;;; External Procedures:
;;;
;;;     cboundp                 c
;;;     cdefinition             c
;;;     define-primitives       &optional package
;;;
;;; Internal Procedures:
;;;
;;;     abstract                var expr &keys count trace warn optimize strong
;;;     abbrev-s                term-1 term-2
;;;
;;;     [-reader                stream char
;;;     ]-reader                stream char
;;;

;;;
;;; Module Prologue:
;;;
(in-package sk)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.

;;;
;;; Exported Interfaces:
;;;
(eval-when (compile load eval)
  (defparameter *COMBINATORS*
    ;; Abstractions are in terms of the following Primitives,
    ;; which are internal (not EXPORTed):
    '(b c e h i k keta knil pair s u v w y false true)
    "The Combinatory Primitives")
  
  (defparameter *INTERFACES*
    '(beta cboundp cdefinition defc define-primitives div lambda*
      sk-implementation-version)
    "The S-K Reduction Engine Interfaces")
  
  (defparameter *PARAMETERS*
    '(*count* *normal* *optimize* *strong* *trace* *warn*)
    "The Keyword Defaults"))

(eval-when (compile load)
  (export '(*combinators* *interfaces* *parameters*)))

;;;
;;; NOTE: To support Abstraction of Variables that happen to name
;;;       Primitive Combinators the symbols associated with these
;;;       primitives should not be EXPORTed!  Seperate definition
;;;       of Primitive Combinators, in a User Specifiable Package,
;;;       should be obtained via: (DEFINE-PRIMITIVES).  See below.
;;;
(export *interfaces*)
(export *parameters*)

;;;
;;; Special Variables and Constants:
;;;
(defparameter *VERSION-MAJOR* 1                 ; Obtain the SK Version via
  "SK Major Version Number")                    ;SK-IMPLEMENTATION-VERSION.
(defparameter *VERSION-MINOR* 0
  "SK Minor Version Number")
(defparameter *VERSION-EDIT* 5
  "SK Version Edit Number")

(defparameter *ABSTRACTION-COUNT* nil           ; Bind to a Number.
  "The Abstraction Counter")

(defparameter *REDUCTION-COUNT* nil             ; Bind to a Number.
  "The Reduction Counter")

(defparameter *REDUCTION-LEVEL* nil             ; Bind to a Number.
  "The Reduction Recursion Depth")

(defparameter *DEFAULT-PRINT-CIRCLE* t          ; Takes NIL, or T.
  "Tracer *PRINT-CIRCLE* Default")

;;;
;;; This code was originally implemented on Allegro CL for the Apple Macintosh
;;; and later ported to Allegro Common Lisp for Windows when *print-level*
;;; needed to be placed under the #+:ccl conditional.  This was when the ANSI
;;; Common Lisp Standard was being defined.
;;;
;;; Franz now uses the #+:allegro conditional for Allegro specific features;
;;; but *print-level* should be supported by any ANSI Common Lisp compliant
;;; implementations.
;;;
;;;#+:ccl
(defparameter *DEFAULT-PRINT-LEVEL* 16          ; Takes NIL, or an Integer.
  "Tracer *PRINT-LEVEL* Default")

(defparameter *COUNT* nil                       ; Takes NIL, or T.
  "The Default Counter Display Setting")

(defparameter *TRACE* nil                       ; Takes NIL, or T.
  "The Default Tracer Setting")

(defparameter *WARN* t                          ; Takes NIL, or T.
  "The Default Warnings Setting")

(defparameter *NORMAL* nil                      ; Takes NIL, or T.
  "Disposition of Operand Redices, interior to an Irreducible Redex")

(defparameter *OPTIMIZE* t                      ; Takes NIL, :STANDARD, or T.
  "The Default Optimizer Setting")

(defparameter *STRONG* nil                      ; Takes NIL, or T.
  "The Abstraction Strength")

;;;
;;; The following Macro implements the Abstraction Interface:
;;;
(defmacro LAMBDA* (var expr
                       &rest keys
                       &key (count *count*) (warn *warn*)
                       &allow-other-keys)
  "Abstract."
  (declare (special *count* *warn*))
  `(let ((*abstraction-count* 0))
     (declare (special *abstraction-count* *combinators*))
     (when (and ,warn (member ',var *combinators*))
       (warn "Abstraction of a Primitive Combinator: ~S." ',var))
     (prog1
       (abstract ',var ',expr . ,keys)
       ,(when count
          `(format t "~&[Processed ~2D term~A abstracting: ~S.]"
                   *abstraction-count*
                   (if (= *abstraction-count* 1) " " "s")
                   ',var))
       )))

;;;
;;; Combinator Definition Macros:
;;;
(defsetf CDEFINITION (c) (def)
  "Set the definition of a Combinator."
  `(locally
    (declare (special *combinators* *warn*))
    (when (and *warn* (member ,c *combinators*))
      (warn "Attempting to redefine a Primitive Combinator: ~S." ,c))
    (setf (get ,c 'combinatorp) ,def)
    ))

(defmacro DEFC (c def)
  "Define a Combinator."
  `(progn (setf (cdefinition ',c) '(y (lambda* ,c ,def)))
          ',c))

;;;
;;; Now for the Code:
;;;
(defun SK-IMPLEMENTATION-VERSION ()             ; Anticipate Support.
  "Obtain the SK Version string."
  (format nil "Version ~D.~D~@[.~D~]"
          *version-major*
          *version-minor*
          (when *version-edit* (unless (zerop *version-edit*) *version-edit*))
          ))

(defun CBOUNDP (c)
  "Does the specified symbol name a Combinator?"
  (if (not (symbolp c))
    (error "~S is not a valid argument to ~S." c 'cboundp)
    (let ((plist (symbol-plist c)))             ; This awkward interface
      (multiple-value-bind                      ;admits NIL CDEFINITIONs.
        (indicator value prest) (get-properties plist '(combinatorp))
        (declare (ignore indicator value))
        (not (endp prest))
        ))
    ))

(defun CDEFINITION (c)
  "Get the definition of a Combinator."
  (if (cboundp c)
    (get c 'combinatorp)
    (error "Undefined combinator: ~S." c)
    ))

(defun DEFINE-PRIMITIVES (&optional (package nil package-sp))
  "Define the Primitive Combinators, in a User Specified Package."
  (declare (special *combinators*))
  (dolist (sk-symbol *combinators*)
    (let ((user-symbol (apply #'intern
                              (symbol-name sk-symbol)
                              (when package-sp (list package))
                              )))
      (setf (cdefinition user-symbol) sk-symbol)
      )))

(defun ABSTRACT (var expr
                     &key
                     (count *count*) (trace *trace*) (warn *warn*)
                     (optimize *optimize*) (strong *strong*)
                     &allow-other-keys)
  "Abstract the specified Variable from the Curried Expression."
  (declare (special *abstraction-count* *count* *trace* *warn*
                    *optimize* *strong*))
  (let ((*count* count) (*trace* trace) (*warn* warn)
        (*optimize* optimize) (*strong* strong))
    ;;
    ;; Diagnostic Section:
    ;;
    (when *abstraction-count* (incf *abstraction-count*))
    (when trace
      (when count (format t "~&[Term ~2D]" *abstraction-count*))
      (pprint `(lambda* ,var ,expr)))
    ;;
    ;; Abstraction Dispatcher:
    ;;
    (cond                                       ; Generalize Abstraction.
     ((null var) `(knil ,expr))
     ((consp var) `(u (lambda* ,(first var) (lambda* ,(rest var) ,expr))))
     (t (cond
         ((consp expr)                          ; NIL is handled as an Atom.
          (let ((ap-first (first expr))
                (ap-rest (rest expr)))
            (if (endp ap-rest)
              ;;
              ;; A CL-Term is not supposed to be wrapped in gratuitous
              ;; parentheses, but traverse any that may be encountered.
              ;;
              (progn
                (when warn (warn "Parenthesized CL-Term: ~S." expr))
                (when *abstraction-count*       ; Doesn't count as Abstraction.
                  (decf *abstraction-count*))
                (abstract var ap-first))
              (let ((ap-second (second expr))
                    (ap-more (rest ap-rest)))
                (cond
                 ((eq ap-first 'lambda*)        ; Resolve Inner Abstractions.
                  (abstract var (eval expr)))   ; Recurse from LAMBDA* Macro.
                 ;;
                 ;; Perform Implicit Left Association, AFTER
                 ;; recognizing Internal Abstractions (above).
                 ;;
                 (ap-more
                  (when *abstraction-count*     ; Doesn't count as Abstraction.
                    (decf *abstraction-count*))
                  (abstract var (reduce #'list expr)))
                 ;;
                 ;; The S-K implementation of QUOTE is "curried", and
                 ;; can be abbreviated just like any other combinator.
                 ;;
                 ((eq ap-first 'quote) (if (eq *optimize* :experimental)
                                         `(keta ,expr) `(k ,expr)))
                 ;;
                 ;; Abstract the Application:
                 ;;
                 (t (let ((term-1 (abstract var ap-first))
                          (term-2 (abstract var ap-second)))
                      (if optimize
                        (abbrev-s term-1 term-2) `(s ,term-1 ,term-2))
                      ))
                 )))
            ))
         ((eql var expr) 'i)                    ; Allow Numeric Variables.
         ((and *optimize* (not (eq *optimize* :standard)) (eq expr 'i)) 'h)
         ((eq *optimize* :experimental) `(keta ,expr))
         (t `(k ,expr))
         ))
     )))

(defun ABBREV-S (term-1 term-2)
  "Produce an S-Combinator, or an Abbreviation, given its first two Terms."
  (declare (special *optimize*))
  (let ((anyp (not (eq *optimize* :standard)))  ; Permit any abbreviation?
        (term-1-listp (listp term-1))
        (term-2-listp (listp term-2)))
    (let ((term-1-kp (and term-1-listp (eq (first term-1) 'k)))
          (term-2-kp (and term-2-listp (eq (first term-2) 'k)))
          (term-1-ip (eq term-1 'i))
          (term-2-ip (eq term-2 'i)))
      (let ((rest-1 (when term-1-kp (rest term-1)))
            (rest-2 (when term-2-kp (rest term-2))))
        (let ((first-1 (unless (rest rest-1) (first rest-1))))
          (cond ((and term-1-kp term-2-kp)
                 `(k (,@(if (consp first-1) first-1 rest-1) . ,rest-2)))
                ((and term-1-kp term-2-ip) (or first-1 rest-1))
                ((and (eq term-1 's) (eq term-2 'h)) 'w)
                ;;
                ;; Two extensions to the optimization model of
                ;; Turner, V and E, are optionally included:
                ;;
                ((and anyp term-1-ip term-2-kp) `(e . ,rest-2))
                ((and anyp term-1-ip term-2-ip) 'v)
                (term-2-kp `(c ,term-1 . ,rest-2))
                (term-1-kp `(b ,@rest-1 ,term-2))
                ;;
                ;; This expression has not been abbreviated:
                ;;
                (t `(s ,term-1 ,term-2))
                ))
        ))))

(defun \?-READER (stream char)
  "Read in an Abstraction Variable."
  (declare (ignore char))
  (let ((var (read stream t :eof t)))
    (let ((expr (read stream t nil t)))
      `(lambda* ,var ,expr)
      )))

;;;
;;; The following conforms to Common Lisp, but variation in local
;;; policies regarding modification of the Reader's Syntax Tables
;;; may lead to unanticipated environmental consequences.
;;;
;;; Read "[var] expr" as "(lambda* var expr)":
;;;
(defun \[-READER (stream char)
  "Read in a Generalized Abstraction Variable."
  (declare (ignore char))
  (let ((var (read-delimited-list #\] stream t)))
    ;;
    ;; Note that specification of more than one variable on var
    ;; corresponds to "a suitably generalized Law of Abstraction."
    ;; Cf. the "uncurry", U-combinator of [Turner 1979].
    ;;
    (if (endp var)
      (error "Abstraction variable missing in '[]'.")
      (let ((expr (read stream t nil t)))
        `(lambda* ,(reduce #'cons var :from-end t) ,expr)
        ))
    ))

(defun \]-READER (stream char)
  "Indicate an Abstraction Operator Syntax Error."
  (declare (ignore stream char))
  (error "Unmatched ']'."))

;;;
;;; Augment the Reader:
;;;
(set-macro-character #\? #'\?-reader)

(set-macro-character #\[ #'\[-reader)
(set-macro-character #\] #'\]-reader)

;;;
;;; Suspend Module Context:
;;;
