;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-space.lisp  Module: compare                Status:        operational
;;;
;;; Author     Version        Edit Date        Purpose of Edit
;;; ------     -------        ---------        ---------------
;;; Chris Hume         1.1         3-Nov-90        Added this header.
;;; Chris Hume         1.0        12-May-90        Gathered performance data.
;;;
;;; Purpose:        Implement the Linear Space LCS algorithm [by D.S. Hirschberg].
;;;
;;; Usage:        This file is intended to be portable
;;;                to any COMMON LISP Environment.
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
;;;        common-length                a-sequence b-sequence &key
;;;                                ignore-case-and-style ignore-whitespace
;;;
;;;        basic-common-pairs        a-sequence b-sequence &key
;;;                                ignore-case-and-style ignore-whitespace
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

(defun COMMON-LENGTH (a-sequence b-sequence
                      &rest keys
                      &key
                      (length-pair nil length-pair-sp)
                      (ab-matches () ab-matches-sp)
                      (distance nil distance-sp)
                      &allow-other-keys)
  "Obtain the lengths of any longest common subsequence, simply."
  (let* ((a-length (if length-pair-sp (car length-pair) (length a-sequence)))
         (b-length (if length-pair-sp (cdr length-pair) (length b-sequence)))
         (ab-matches (if ab-matches-sp
                         (list* "Too bad... Absolute positions need to be offset!"
                                ab-matches)
                         (apply #'list-matches
                                a-sequence b-sequence :order-up t keys)))
         (distance (if distance-sp
                       (do ((index 0 (1+ index))) ((= index b-length) distance)
                         (setf (aref distance index) 0))
                       (make-array b-length :initial-element 0))))
    (do ((a-position 0 (1+ a-position))
         (ab-common-remains ab-matches (rest ab-common-remains)))
        ((= a-position a-length)                ; Not: (ENDP AB-COMMON-REMAINS).
         (if (zerop b-length)
             0 (aref distance (1- b-length))))        ; Common Subsequence Length
      (let ((diagonal-distance 0)
            (vertical-distance 0))
        (do ((b-position 0 (1+ b-position))
             ;;
             ;; An appropriate MEMBER of the list will need to be found
             ;; (so that AB-MATCHES may be supplied, above.)
             ;;
             (b-common-remains (first ab-common-remains)))
            ((= b-position b-length))
          (let ((horizontal-distance (aref distance b-position))
                (matched (and b-common-remains
                              (= (first b-common-remains) b-position))))
            
            (if matched
                (progn
                  (setq b-common-remains (rest b-common-remains))
                  (setf (aref distance b-position) (1+ diagonal-distance)))
              
                (when (< horizontal-distance vertical-distance)
                  (setf (aref distance b-position) vertical-distance)
                  ))
            
            (setf diagonal-distance horizontal-distance)
            (setf vertical-distance (aref distance b-position))
            ))
        ))
    ))

(defun BASIC-COMMON-PAIRS (a-sequence b-sequence
                           &rest keys
                           &key
                           (length-pair nil length-pair-sp)
                           (debug-log-entry nil)
                           &allow-other-keys)
  "Match elements of a longest common subsequence, while conserving memory."
  (let ((a-length (if length-pair-sp (car length-pair) (length a-sequence)))
        (b-length (if length-pair-sp (cdr length-pair) (length b-sequence))))
    
    (cond
      ;;
      ;; Step 1:  Check for trivial (recursion termination) cases.
      ;;
      ((or (zerop a-length) (zerop b-length))
       (values ()                                ; Match Pair Sequence Found
               (cons a-length b-length)                ; Input Sequence Length Pair
               0                                ; Common Subsequence Length
               0))                                ; Number of Log Entry Pairs
     
      ((= a-length 1)
       (let ((a-normal (apply #'normalize-element (elt a-sequence 0) keys)))
         (flet ((TESTER (element)
                  "Normalize and test each element for admissability."
                  (equal a-normal (apply #'normalize-element element keys))
                  ))                                ; Pick ANY match, in this case.
           (let* ((b-match (position-if #'tester b-sequence))
                  (match-pairs (when b-match (list (cons 0 b-match))))
                  (length-pair (cons a-length b-length))
                  (lcs-length (length match-pairs))
                  (pair-count lcs-length))
            
             (values match-pairs                ; Match Pair Sequence Found
                     length-pair                ; Input Sequence Length Pair
                     lcs-length                        ; Common Subsequence Length
                     pair-count)))                ; Number of Log Entry Pairs
         ))
     
      ((= b-length 1)
       (let ((b-normal (apply #'normalize-element (elt b-sequence 0) keys)))
         (flet ((TESTER (element)
                  "Normalize and test each element for admissability."
                  (equal b-normal (apply #'normalize-element element keys))
                  ))                                ; Pick ANY match, in this case.
           (let* ((a-match (position-if #'tester a-sequence))
                  (match-pairs (when a-match (list (cons a-match 0))))
                  (length-pair (cons a-length b-length))
                  (lcs-length (length match-pairs))
                  (pair-count lcs-length))
            
             (values match-pairs                ; Match Pair Sequence Found
                     length-pair                ; Input Sequence Length Pair
                     lcs-length                        ; Common Subsequence Length
                     pair-count)))                ; Number of Log Entry Pairs
         ))
     
      ;;
      ;; Step 2.
      ;;
      (t (let ((mid-point (truncate a-length 2))
               (max-point nil)
               (max-length nil)
               ;;
               ;; Step 3.
               ;;
               (prefix-distance (make-array b-length))
               (suffix-distance (make-array b-length)))
          
           (apply #'common-length
                  (subseq a-sequence 0 mid-point)
                  b-sequence
                  :length-pair (cons mid-point b-length)
                  :distance prefix-distance
                  keys)
          
           (apply #'common-length
                  (nreverse (subseq a-sequence mid-point))
                  (reverse b-sequence)
                  :length-pair (cons (- a-length mid-point) b-length)
                  :distance suffix-distance
                  keys)
          
           ;;
           ;; Step 4.
           ;;
           (do ((b-position -1 (1+ b-position)))
               ((= b-position b-length) max-length)
             (let ((next-length (+ (if (< b-position 0)
                                       0 (svref prefix-distance b-position))
                                   (if (= b-position (1- b-length))
                                       0 (svref suffix-distance
                                                (- b-length 2 b-position))))
                                ))
               (when (or (null max-point) (< max-length next-length))
                 (setq max-point (1+ b-position)
                       max-length next-length))
               ))
          
           (when debug-log-entry
             (format t "~&mid-point = ~3D, max-point = ~3D, max-length = ~D~%~%"
                     mid-point
                     max-point
                     max-length))
          
           ;;
           ;; Step 5.
           ;;
           (multiple-value-bind (prefix-pairs prefix-length-pair)
               (apply #'common-pairs
                      (subseq a-sequence 0 mid-point)
                      (subseq b-sequence 0 max-point)
                      #||
                      :length-pair (cons mid-point max-point)
                      ||#
                      keys)
             (multiple-value-bind (suffix-pairs suffix-length-pair)
                 (apply #'common-pairs
                        (subseq a-sequence mid-point)
                        (subseq b-sequence max-point)
                        #||
                        :length-pair (cons
                                       (- a-length mid-point)
                                       (- b-length max-point))
                        ||#
                        keys)
               (declare (ignore suffix-length-pair))
               (flet ((OFFSETTER (offset-pair pairs)
                        "Displace position pairs by an offset pair."
                        (let ((a-offset (car offset-pair))
                              (b-offset (cdr offset-pair)))
                          (mapcar #'(lambda (pair)
                                      (let ((a-position (car pair))
                                            (b-position (cdr pair)))
                                        (cons (+ a-position a-offset)
                                              (+ b-position b-offset))
                                        ))
                                  pairs))
                        ))
                 (let* ((match-pairs
                          (nconc prefix-pairs
                                 (offsetter prefix-length-pair suffix-pairs)))
                        (length-pair (cons a-length b-length))
                        (lcs-length (+ (length prefix-pairs) (length suffix-pairs)))
                        (pair-count lcs-length))
                  
                   (when debug-log-entry
                     (format t "~&Finishing a = ~S, b = ~S, match-pairs = ~S~%"
                             a-sequence
                             b-sequence
                             match-pairs))
                  
                   ;;
                   ;; Step 6.
                   ;;
                   (values match-pairs                ; Match Pair Sequence Found
                           length-pair                ; Input Sequence Length Pair
                           lcs-length                ; Common Subsequence Length
                           pair-count))                ; Number of Log Entry Pairs
                 )))
           ))
      (t (warn "Unanticipated recursion case: a = ~S, b = ~S."
               a-sequence b-sequence)))
    ))

;;;
;;; Suspend Module Context:
;;;
