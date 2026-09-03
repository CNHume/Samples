;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;
;;; Source: compare.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Define the Sequence Comparison Utility's Package and Systems.
;;;
;;; The COMPARE system is currently spread across eleven ".lisp" files:
;;;
;;;	compare-face, compare-interval, compare-body, compare-scan,
;;;	compare-io, compare-delta, compare-format, compare-merge,
;;;	compare-parallel, compare-review, and compare-series.
;;;
;;; There is also a subsystem which demonstrates how the output format provided
;;; by compare-delta might be applied, within the file:
;;;
;;;	compare-edit.
;;;
;;; There is a test subsystem within the file:
;;;
;;;	compare-test.
;;;
;;; Finally, a pair of "private patch" files which install this facility
;;; into Genera (superseding the standard SRCCOM facility) are provided:
;;;
;;;	srccom-patch, and cp-patch.
;;;
(defpackage COMPARE				; Apparently, SRCCOM assumes ZL.
  (:nicknames CMP)
  (:size 768.))

(defsystem COMPARE
    (:pretty-name "Sequence Comparison Utility"
     :default-pathname "COMPARE:COMPARE;"
     :default-package COMPARE
     :patchable t
     :bug-reports ("Hume" "Report problems with Compare.")
     :source-category :basic
     :distribute-sources t
     :distribute-binaries t)
  (:module help "compare-usage" (:type :text))
  (:module lore "compare-history" (:type :text))
  (:module face ("compare-face" "compare-io" "compare-format"))
  (:module body ("compare-interval" "compare-body" "compare-scan"))
  (:module form ("compare-delta" "compare-merge" "compare-parallel"
		 "compare-review" "compare-series"))
  (:module edit "compare-edit" (:type :system))
  (:module test "compare-test" (:type :system))
  (:module wrap "compare-wrap" (:type :system)))

(defsubsystem COMPARE-EDIT
    (:pretty-name "Source File Delta Editor"
     :default-pathname "COMPARE:COMPARE;"
     :default-package COMPARE
     :source-category :basic
     :distribute-sources t
     :distribute-binaries t)
  (:module test "compare-edit"))

(defsubsystem COMPARE-TEST
    (:pretty-name "Sequence Comparison Utility Diagnostics"
     :default-pathname "COMPARE:COMPARE;"
     :default-package COMPARE
     :source-category :optional
     :distribute-sources t
     :distribute-binaries t)
  (:module help "compare-algorithm" (:type :text))
  (:module test "compare-test"))

(defsubsystem COMPARE-WRAP
    (:pretty-name "Sequence Comparison Utility Wrapper"
     :default-pathname "COMPARE:COMPARE;"
     :default-module-type :lisp-example
     :source-category :optional
     :distribute-sources t
     :distribute-binaries t)
  (:module edit "srccom-patch")
  (:module show "cp-patch"))
