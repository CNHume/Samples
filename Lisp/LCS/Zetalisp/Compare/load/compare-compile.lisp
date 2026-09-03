;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; compare-compile.lisp -- compile the portable Compare sources, iteratively.
;;; Intended for Franz Allegro Common Lisp (ANSI).
;;;
;;; The Compare root is derived from this file's own location (*load-truename*),
;;; so it works wherever the tree has been placed (C:\compare\, D:\compare\,
;;; /path/Compare/, ...) with no path editing.  Compiles every .lisp directly
;;; in the Compare root (portable core, edit, test); System/ and lab/ are
;;; excluded.
(let ((root (merge-pathnames (make-pathname :directory '(:relative :up))
                             *load-truename*)))
  (dolist (lisp (directory (merge-pathnames "*.lisp" root)))
    (format t "~&Compiling \"~A\"~%" (namestring (truename lisp)))
    (compile-file lisp)))
