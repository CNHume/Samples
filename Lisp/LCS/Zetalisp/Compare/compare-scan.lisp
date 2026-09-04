;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-scan.lisp        Module: compare                Status:        operational
;;;
;;; History:        Please record your edits in "compare-history.text".
;;;
;;; Purpose:        Implement the "(Traditional) Windowed Scan" algorithm.
;;;
;;; Usage:      This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:        Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;           level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;        scan-common-pairs        a-sequence b-sequence &key transfix
;;;                                ignore-case-and-style ignore-whitespace
;;;
;;;        handle-difference        a-position b-position a-remains b-remains
;;;                                sequence-pair length-pair listp-pair
;;;                                &optional scan-window matcher check-count
;;;

;;;
;;; Resume Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Now for the Code:
;;;
(defun SCAN-COMMON-PAIRS (a-sequence b-sequence
                          &rest keys
                          &key
                          (transfix 0)
                          (debug-method nil)
                          &allow-other-keys)
  "Match elements common to two sequences, the old fashioned way."
  (declare (values matched-pairs length-pair lcs-length
                   pair-count check-count work-done))
  (flet ((EQUIVALENT (a-element b-element)
           (declare (special *compare-normalizer*))
           "Test normalized element pairs for equality."
           (equal (apply *compare-normalizer* a-element keys)
                  (apply *compare-normalizer* b-element keys))
           ))
    (let* ((sequence-pair (cons a-sequence b-sequence))
           (a-length (length a-sequence))
           (b-length (length b-sequence))
           (length-pair (cons a-length b-length))
           (progress-total (+ a-length b-length))
           (a-listp (listp a-sequence))
           (b-listp (listp b-sequence))
           (listp-pair (cons a-listp b-listp))
           (scan-window (1+ transfix))
           (work-done :checked))
      (noting-progress ("Scanning Common Subsequence")
        (block COMPARE
          (do ((a-position 0 (1+ a-position))
               (b-position 0 (1+ b-position))
               (a-remains (when a-listp a-sequence) (rest a-remains))
               (b-remains (when b-listp b-sequence) (rest b-remains))
               (pair-stack () (acons a-position b-position pair-stack))
               (pair-count 0 (1+ pair-count))
               (check-count 0 (1+ check-count)))
              ((or (>= a-position a-length) (>= b-position b-length))
               (let ((common-length (length pair-stack)))
                 (values (nreverse pair-stack)        ; Match Pair Sequence Found
                         length-pair                ; Input Sequence Length Pair
                         common-length                ; Common Subsequence Length
                         pair-count                ; Number of Log Entry Pairs
                         check-count                ; Number of Pairs Compared
                         work-done)))                ; The Work Done
            (let ((a-element (if a-listp
                                 (first a-remains)
                                 (aref a-sequence a-position)))
                  (b-element (if b-listp
                                 (first b-remains)
                                 (aref b-sequence b-position))))
              (when debug-method
                (format t "~&Comparing: ~S and ~S.~%" a-element b-element))

              (unless (equivalent a-element b-element)
                (let ((done-p nil))
                  (multiple-value-setq
                    (a-position b-position a-remains b-remains
                                done-p check-count)
                    (handle-difference a-position
                                       b-position
                                       a-remains
                                       b-remains
                                       sequence-pair
                                       length-pair
                                       listp-pair
                                       scan-window
                                       #'equivalent
                                       check-count))
                  (when done-p
                    (let ((common-length (length pair-stack)))
                      (return-from compare        ; End on a difference!
                        (values (nreverse pair-stack)
                                length-pair
                                common-length
                                pair-count
                                check-count
                                work-done))
                      ))
                  ))
              (note-progress (+ a-position b-position) progress-total)
              ))
          ))
      )))

(defun HANDLE-DIFFERENCE (a-position
                          b-position
                          a-remains
                          b-remains
                          sequence-pair
                          length-pair
                          listp-pair
                          &optional
                          (scan-window 1)
                          (matcher #'equal)
                          (check-count 0))
  "Attempt to resynchronize, after encountering a difference."
  (declare (values a-position b-position a-remains b-remains
                   done-p check-count)
           #+ansi-cl
           (dynamic-extent matcher)
           #+(and (not ansi-cl) symbolics)
           (sys:downward-funarg matcher))
  (let ((a-sequence (car sequence-pair))
        (b-sequence (cdr sequence-pair))
        (a-length (car length-pair))
        (b-length (cdr length-pair))
        (a-listp (car listp-pair))
        (b-listp (cdr listp-pair))
        (done-p nil))

    (block SCAN
      (do ((a-cross (1+ a-position) (1+ a-cross))
           (b-cross (1+ b-position) (1+ b-cross))
           (a-tail (rest a-remains) (rest a-tail))
           (b-tail (rest b-remains) (rest b-tail)))
          ((and (>= a-cross a-length) (>= b-cross b-length))
           (setq done-p t))

        ;;
        ;; Scan for some previous SCAN-WINDOW subsequence which
        ;; corresponds to the SCAN-WINDOW subsequence following
        ;; a position in the other subsequence, "cross" from it.
        ;;
        (when (< b-cross b-length)
          (let ((a-bound (min (1+ a-cross) a-length)))
            (do ((a-index a-position (1+ a-index))
                 (a-scan a-remains (rest a-scan)))
                ((>= a-index a-bound))
              (let ((match-range (min scan-window
                                      (- a-length a-index)
                                      (- b-length b-cross)
                                      ))
                    (a-rest (when a-listp a-scan))
                    (b-rest (when b-listp b-tail)))
                (do ((match-index 0 (1+ match-index)))
                    ((= match-index match-range)
                     (when a-listp (setq a-remains a-scan))
                     (when b-listp (setq b-remains b-tail))
                     (setq a-position a-index
                           b-position b-cross)
                     (return-from scan))
                  (let ((a-element
                          (if a-listp
                              (pop a-rest)
                              (aref a-sequence (+ a-index match-index))
                              ))
                        (b-element
                          (if b-listp
                              (pop b-rest)
                              (aref b-sequence (+ b-cross match-index))
                              )))
                    (incf check-count)
                    (unless (funcall matcher a-element b-element)
                      (return))
                    ))
                ))
            ))

        (when (< a-cross a-length)
          (let ((b-bound (min (1+ b-cross) b-length)))
            (do ((b-index b-position (1+ b-index))
                 (b-scan b-remains (rest b-scan)))
                ((>= b-index b-bound))
              (let ((match-range (min scan-window
                                      (- a-length a-cross)
                                      (- b-length b-index)
                                      ))
                    (a-rest (when a-listp a-tail))
                    (b-rest (when b-listp b-scan)))
                (do ((match-index 0 (1+ match-index)))
                    ((= match-index match-range)
                     (when a-listp (setq a-remains a-tail))
                     (when b-listp (setq b-remains b-scan))
                     (setq a-position a-cross
                           b-position b-index)
                     (return-from scan))
                  (let ((a-element
                          (if a-listp
                              (pop a-rest)
                              (aref a-sequence (+ a-cross match-index))
                              ))
                        (b-element
                          (if b-listp
                              (pop b-rest)
                              (aref b-sequence (+ b-index match-index))
                              )))
                    (incf check-count)
                    (unless (funcall matcher a-element b-element)
                      (return))
                    ))
                ))
            ))
        ))

    (values a-position b-position a-remains b-remains done-p check-count)
    ))

;;;
;;; Suspend Module Context:
;;;
