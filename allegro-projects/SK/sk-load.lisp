;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CG-USER; Base: 10 -*-
;;;
;;; Module Prologue:
;;;
(in-package cg-user)
(defpackage sk)

(eval-when (compile load) (use-package 'sk))
(eval-when (load) #-sk-traverse(push :sk-traverse *features*))

(dolist (lisp (directory "D:/Documents/Sourcetree/Samples/allegro-projects/SK/Source/*.lisp"))
  (format t "~&Loading \"~A\"~%" (enough-namestring (truename lisp) ".lisp"))
  (load lisp))

(shiftf *print-circle* t)
