;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-form.lisp	Module: compare		Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 1.2	 9-Nov-90	Cleaned up.
;;; Chris Hume	 1.1	 8-Nov-90	Added the FORM-TOKENS complement.
;;; Chris Hume	 1.0	 8-Nov-90	Created file.
;;;
;;; Purpose:	Tokenize a Form for Sequential Comparison.
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	(compile-file "compare-form")
;;;
;;; Contents:
;;;
;;;	The enclosed function converts any nested form into a list of
;;;	"tokens", denoting the begining and end of sub-forms by means
;;;	of the symbols |(| and |)|, respectively.
;;;
;;; External Interfaces:
;;;
;;;	tokenize-form		form
;;;
;;;	form-tokens		tokens
;;;
;;; Local Interfaces:
;;;
;;;	basic-form-tokens	tokens
;;;

;;;
;;; Establish Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.

;;;
;;; The Primary Interfaces:
;;;
(export '(tokenize-form form-tokens |(| |)|))

(defun TOKENIZE-FORM (form)
  "Tokenize the form."
  (flet ((FORM-REDUCER (subform reduction)
	   "Reduce the next subform."
	   (concatenate 'list subform reduction)))
    (typecase form
      (list `(|(| ,.(reduce #'form-reducer
			    (mapcar #'tokenize-form form)
			    :from-end t) |)|))
      (otherwise (list form)))
    ))

(defun BASIC-FORM-TOKENS (tokens)
  "Build an sub-form out of a list of tokens."
  (let ((form ()))
    (do ((token-remains tokens (rest token-remains)))
	((endp token-remains) (values (nreverse form) ()))
      (let ((token (first token-remains)))
	(case token
	  (|(| (multiple-value-setq (token token-remains)
		 (basic-form-tokens (rest token-remains))
		 ))
	  (|)| (return (values (nreverse form) token-remains))
	       ))
	(push token form)
	))
    ))

(defun FORM-TOKENS (tokens)
  "Build a form out of a list of tokens."
  (multiple-value-bind (form remainder)
      (basic-form-tokens tokens)
    (let ((form-length (length form)))
      (if (and (null remainder) form-length (= form-length 1))
	  (values (first form) remainder)
	  (values form remainder)
	  ))
    ))
