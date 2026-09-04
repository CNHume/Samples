;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-test.lisp  Module: compare                Status:        operational
;;;
;;; History:    Please record your edits in "compare-history.text".
;;;
;;; Purpose:    Test the Sequence Comparison Utility.
;;;
;;; Usage:      This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:    Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;        This file exercises the Sequence Comparison Utility with a variety
;;;        of test cases.  The DIAGNOSE-COMPARE function returns NIL if every
;;;        test succeeds, and a non-NIL (failed test name) value otherwise.
;;;
;;; External Interfaces:
;;;
;;;        diagnose-compare        &rest keys
;;;
;;; Local Interfaces:
;;;
;;;        make-random-sequence        type size number
;;;
;;;        permutation-lcs-lengths        sequence &rest keys
;;;
;;;        permutations                sequence
;;;
;;;        permute-list                elements
;;;

;;;
;;; Module Prologue:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.

;;;
;;; The primary interfaces:
;;;
(export '(diagnose-compare))

;;;
;;; Now for the Code:
;;;
(defun DIAGNOSE-COMPARE (&rest keys)
  "Diagnose the Sequence Comparison Utility."
  (let* ((null-pair (apply #'common-pairs () () keys))
         (null-left (apply #'common-pairs '(left) () :symmetry nil keys))
         (null-right (apply #'common-pairs () '(right) :symmetry nil keys))
         (a-example "abcbdda")
         (b-example "badbabd")
         (forward-example (apply #'common-sequence
                                 a-example b-example :symmetry nil keys))
         (reverse-example (apply #'common-sequence
                                 b-example a-example :symmetry nil keys))
         (desired-example "abbd")
         (a-symmetric "aooboocoo dooeoo")
         (b-symmetric "foogoo hooioo")
         (forward-symmetric (apply #'common-sequence
                                   a-symmetric b-symmetric keys))
         (reverse-symmetric (apply #'common-sequence
                                   b-symmetric a-symmetric keys))
         (desired-symmetric "oooo oooo")
         (a-normal '("zero" "ONE" " two" " THREE "))
         (b-normal '("zero" "one" "two " "Three"))
         (0-normal (apply #'common-sequence a-normal b-normal
                          :ignore-whitespace nil :ignore-case-and-style nil
                          keys))
         (1-normal (apply #'common-sequence a-normal b-normal
                          :ignore-whitespace nil :ignore-case-and-style t
                          keys))
         (2-normal (apply #'common-sequence a-normal b-normal
                          :ignore-whitespace t :ignore-case-and-style nil
                          keys))
         (3-normal (apply #'common-sequence a-normal b-normal
                          :ignore-whitespace t :ignore-case-and-style t
                          keys))
         (desired-0-normal '("zero"))
         (desired-1-normal '("zero" "one"))
         (desired-2-normal '("zero" "two "))
         (desired-3-normal '("zero" "one" "two " "Three"))
         (permutation-lcs-lengths (apply #'permutation-lcs-lengths '(a b c d)
                                         keys))
         (desired-lcs-lengths
           '(4 3 3 3 3 2 3 2 3 3 2 2 3 2 2 2 2 2 3 2 2 2 2 1))
         (a-quadratic
           "0123456701234567012345670123456701234567012345670123456701234567")
         (b-quadratic (reverse a-quadratic))
         (forward-quadratic (apply #'common-sequence
                                   a-quadratic b-quadratic :symmetry nil keys))
         (reverse-quadratic (apply #'common-sequence
                                   b-quadratic a-quadratic :symmetry nil keys))
         (desired-quadratic-length 15.)
         (a-affix '(m a d e i n))
         (b-affix '(t a i w a n))
         (correspondence-pairs (apply #'correspondence-sequences
                                      a-affix b-affix :prefix -1 :suffix 1 keys))
         (a-correspondences (mapcar #'car correspondence-pairs))
         (b-correspondences (mapcar #'cdr correspondence-pairs))
         (desired-a-correspondences '((d) () ()))
         (desired-b-correspondences '(() (w) ()))
         (difference-pairs (apply #'difference-sequences
                                  a-affix b-affix :prefix -1 :suffix 1 keys))
         (a-differences (mapcar #'car difference-pairs))
         (b-differences (mapcar #'cdr difference-pairs))
         (desired-a-differences '((a) (e i) (n)))
         (desired-b-differences '((a) (i) (a n)))
         (alphabet-number 20.)
         (improbability 2)                        ; Provide room for "absorption".
         (short-improbable-length 20.)
         (long-improbable-length (* alphabet-number
                                    improbability
                                    short-improbable-length))
         (short-improbable (make-random-sequence 'list
                                                 short-improbable-length
                                                 alphabet-number))
         (long-improbable (make-random-sequence 'list
                                                long-improbable-length
                                                alphabet-number))
         (forward-improbable (apply #'common-sequence
                                    short-improbable long-improbable
                                    :symmetry nil keys))
         (reverse-improbable (apply #'common-sequence
                                    long-improbable short-improbable
                                    :symmetry nil keys)))
    
    ;;
    ;; All the test data is collected in the bindings above.  Now,
    ;; check the values produced against those that were expected:
    ;;
    (cond ((or null-pair null-left null-right)
           ;;
           ;; It is unlikely that anything other than NIL would be produced
           ;; by any of these "Null Tests".  However, these tests can cause
           ;; certain "edge case" errors to make themselves apparent.
           ;;
           :null-sequence)
          ((/= (length desired-example) (length forward-example))
           ;;
           ;; "Forward Example" from the [Hunt and Szymanski, Communications
           ;; of the ACM, May 1977] paper did not produce the desired length.
           ;;
           :forward-example-length)
          ((/= (length desired-example) (length reverse-example))
           ;;
           ;; "Reverse Example" from the [Hunt and Szymanski, Communications
           ;; of the ACM, May 1977] paper did not produce the desired length.
           ;;
           :reverse-example-length)
          ((not (equal desired-example forward-example))
           ;;
           ;; "Forward Example" from the [Hunt and Szymanski, Communications
           ;; of the ACM, May 1977] paper did not produce the desired result.
           ;;
           :forward-example-result)
          ((not (equal desired-example reverse-example))
           ;;
           ;; "Reverse Example" from the [Hunt and Szymanski, Communications
           ;; of the ACM, May 1977] paper did not produce the desired result.
           ;;
           :reverse-example-result)
          ((/= (length desired-symmetric) (length forward-symmetric))
           ;;
           ;; "Forward Symmetric Test" did not produce the desired length.
           ;;
           :forward-symmetric-length)
          ((/= (length desired-symmetric) (length reverse-symmetric))
           ;;
           ;; "Reverse Symmetric Test" did not produce the desired length.
           ;;
           :reverse-symmetric-length)
          ((not (equal desired-symmetric forward-symmetric))
           ;;
           ;; "Forward Symmetric Test" did not produce the desired result.
           ;;
           :forward-symmetric-result)
          ((not (equal desired-symmetric reverse-symmetric))
           ;;
           ;; "Reverse Symmetric Test" did not produce the desired result.
           ;;
           :reverse-symmetric-result)
          ((not (equal desired-0-normal 0-normal))
           ;;
           ;; "0th Normalizer" :ignore-whitespace nil :ignore-case-and-style nil
           ;; did not produce the desired result.
           ;;
           :0-normal-result)
          ((not (equal desired-1-normal 1-normal))
           ;;
           ;; "1st Normalizer" :ignore-whitespace nil :ignore-case-and-style t
           ;; did not produce the desired result.
           ;;
           :1-normal-result)
          ((not (equal desired-2-normal 2-normal))
           ;;
           ;; "2nd Normalizer" :ignore-whitespace t :ignore-case-and-style nil
           ;; did not produce the desired result.
           ;;
           :2-normal-result)
          ((not (equal desired-3-normal 3-normal))
           ;;
           ;; "3rd Normalizer" :ignore-whitespace t :ignore-case-and-style t
           ;; did not produce the desired result.
           ;;
           :3-normal-result)
          ((not (equal desired-lcs-lengths permutation-lcs-lengths))
           ;;
           ;; 4 Element "Permutation Test" did not produce the desired lengths.
           ;;
           :permutation-lcs-lengths)
          ((/= desired-quadratic-length (length forward-quadratic))
           ;;
           ;; "Forward Quadratic Test" did not produce the desired length.
           ;;
           :forward-quadratic-length)
          ((/= desired-quadratic-length (length reverse-quadratic))
           ;;
           ;; "Reverse Quadratic Test" did not produce the desired length.
           ;;
           :reverse-quadratic-length)
          ((not (equal desired-a-correspondences a-correspondences))
           ;;
           ;; "Left Correspondence Affices" did not produce the desired result.
           ;;
           :a-correspondences-result)
          ((not (equal desired-b-correspondences b-correspondences))
           ;;
           ;; "Right Correspondence Affices" did not produce the desired result.
           ;;
           :b-correspondences-result)
          ((not (equal desired-a-differences a-differences))
           ;;
           ;; "Left Difference Affices" did not produce the desired result.
           ;;
           :a-differences-result)
          ((not (equal desired-b-differences b-differences))
           ;;
           ;; "Right Difference Affices" did not produce the desired result.
           ;;
           :b-differences-result)
          ((/= (length forward-improbable) (length reverse-improbable))
           ;;
           ;; "Improbable Test" did not produce a correct length.
           ;;
           :improbable-length)
          ((/= short-improbable-length (length forward-improbable))
           ;;
           ;; "Improbable Test" did not produce the expected length.
           ;;
           (warn "Shorter sequence only partly (~D of ~D elements) absorbed."
                 (length forward-improbable) short-improbable-length)
           :short-improbable-length)
          ((not (equal short-improbable forward-improbable))
           ;;
           ;; "Improbable Test" did not produce the expected result.
           ;;
           :short-improbable-result))
    ))

(defun MAKE-RANDOM-SEQUENCE (type size &optional (number 26.))
  "Make a random sequence of the specified type (over a finite alphabet.)"
  (let ((sequence (make-sequence type size)))
    (map type
         #'(lambda (element) (declare (ignore element)) (random number))
         sequence)))

(defun PERMUTATION-LCS-LENGTHS (sequence &rest keys)
  "Find Longest Common Sequence lengths for each permutation of a sequence."
  (mapcar #'(lambda (permutation)
              (length (apply #'common-pairs sequence permutation keys)))
          (permutations sequence)))

(defun PERMUTATIONS (sequence)
  "Produce all permutations of the sequence of elements."
  (let ((sequence-type (sequence-type-of sequence))
        (elements (coerce sequence 'list)))
    (mapcar #'(lambda (permutation) (coerce permutation sequence-type))
            (permute-list elements))
    ))

;;;
;;; NOTE: The following permutes "alphabets" of unique characters.
;;;          Repeated elements are not distinguished and will thus
;;;       effectively be ignored.
;;;
(defun PERMUTE-LIST (elements)
  "Produce all permutations of the list of elements."
  (if (endp elements)
      '(())
      (mapcan #'(lambda (element)
                  (mapcar #'(lambda (permutation) (cons element permutation))
                          (permute-list (remove element elements))))
              elements)))
