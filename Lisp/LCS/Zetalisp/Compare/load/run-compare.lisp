;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; run-compare.lisp -- one-shot build & self-test for the Compare facility,
;;; intended for Franz Allegro Common Lisp (ANSI).
;;;
;;; The Compare root is derived from this file's own location (*load-truename*),
;;; so this loader works wherever the Compare tree has been placed (C:\compare\,
;;; D:\compare\, /path/Compare/, ...) with no path editing.
;;;
;;; The original System/compare.lisp defines the package with the Genera-only
;;; (:size ...) defpackage option and defsystem, neither of which is ANSI.  So
;;; this file:
;;;
;;;   1. defines the COMPARE package portably,
;;;   2. loads the portable Compare sources (interpreted),
;;;   3. runs (diagnose-compare), which returns NIL iff every test passes,
;;;   4. exits with status 0 on success and status 1 on failure (Allegro only),
;;;      so the result can be checked from a shell script.
;;;
;;; Usage (Allegro IDE Debug window, or headless alisp.exe):
;;;   (load "C:/compare/load/run-compare.lisp")
;;; Headless:  alisp -batch -e '(load "…/run-compare.lisp")' -kill

;;; 1. Establish the package.
(defpackage "COMPARE" (:use "CL") (:nicknames "CMP"))

;;; 2. Locate the Compare root (the parent of this file's load/ directory).
(defparameter *compare-root*
  (merge-pathnames (make-pathname :directory '(:relative :up)) *load-truename*))

;;; 3. Load the portable sources.  System/ (Genera defsystem) and lab/
;;;    (experimental algorithms) are excluded.
(dolist (file '("compare-face.lisp"
                "compare-interval.lisp"
                "compare-body.lisp"
                "compare-scan.lisp"
                "compare-io.lisp"
                "compare-delta.lisp"
                "compare-format.lisp"
                "compare-merge.lisp"
                "compare-parallel.lisp"
                "compare-review.lisp"
                "compare-series.lisp"
                "compare-edit.lisp"
                "compare-test.lisp"))
  (let ((path (merge-pathnames file *compare-root*)))
    (format t "~&Loading \"~A\"~%" (namestring path))
    (load path)))

;;; 4. Run the self-test and report pass/fail.
(in-package compare)

(format t "~&Diagnosing Compare...~%")
(let ((failure (diagnose-compare)))
  (if (null failure)
      (format t "~&Compare: all diagnostics passed.~%")
      (progn
        (format t "~&Compare: FAILED at ~S~%" failure)
        #+allegro (excl:exit 1))))
