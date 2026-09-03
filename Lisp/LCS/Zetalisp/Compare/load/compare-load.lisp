;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; compare-load.lisp -- load the compiled Compare fasls, iteratively.
;;; Intended for Franz Allegro Common Lisp (ANSI).
;;;
;;; The Compare root is derived from this file's own location (*load-truename*),
;;; so it works wherever the tree lives.  fasls are loaded from the Compare
;;; root, where Allegro's compile-file places them alongside the sources.
(let ((root (merge-pathnames (make-pathname :directory '(:relative :up))
                             *load-truename*)))
  (dolist (fasl (directory (merge-pathnames "*.fasl" root)))
    (format t "~&Loading \"~A\"~%" (namestring (truename fasl)))
    (load fasl)))

(use-package 'compare)
