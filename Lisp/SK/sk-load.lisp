;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; Module Prologue:
;;;
(in-package cl-user)
;; (eval-when (:load-toplevel) #-sk-traverse(push :sk-traverse *features*))

(defpackage sk)
(eval-when (:compile-toplevel :load-toplevel) (use-package 'sk))

(dolist (lisp (directory "D:/Documents/Sourcetree/Samples/Lisp/SK/Source/*.lisp"))
  (format t "~&Loading \"~A\"~%" (enough-namestring (truename lisp) ".lisp"))
  (load lisp))

;;; Enable to print infinite Y-combinator recursions.
;;; However, disable for HANOI!
(setq *print-circle* t)
