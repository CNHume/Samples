;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-series.lisp        Module: compare                Status:        operational
;;;
;;; History:        Please record your edits in "compare-history.text".
;;;
;;; Purpose:        Provide the Sequence Comparison Utility with a
;;;                "Series Compare Formatter".
;;;
;;; Usage:      This file is intended to be portable to any COMMON LISP Environment.
;;;
;;; Compile:        Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;        This file provides the COMPARE-IN-SERIES "Compare Formatter".
;;;
;;; NOTE!  Please consult "compare-face.lisp" for definition of the higher
;;;           level interfaces visible to Users.
;;;
;;; Local Interfaces:
;;;
;;;        compare-in-series        differ-p a-sequence b-sequence &key
;;;                                header-type-pair source-pair output-stream width
;;;
;;;        write-pair-in-series        in-seq-pair ex-seq-pair &optional interval-pair
;;;                                exterval-pair interval-index diagnostic
;;;                                &key header-type-pair source-pair name-pair
;;;                                output-stream width object-pair type-pair
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
(defun COMPARE-IN-SERIES (differ-p
                          a-sequence
                          b-sequence
                          &rest keys
                          &key
                          (source-pair nil)
                          (output-stream *standard-output*)
                          (width nil width-sp)
                          (header-type-pair nil header-type-pair-sp)
                          &allow-other-keys)
  "Perform a Formatted Compare in Series."
  (multiple-value-bind (in-seq-pairs interval-pairs length-pair)
      (apply #'basic-compare-sequences differ-p a-sequence b-sequence keys)
    (let* ((exterval-pairs (complement-intervals interval-pairs length-pair))
           (ex-seq-pairs
             (subseq-intervals a-sequence b-sequence exterval-pairs)))

      (when interval-pairs
        (let* ((interval-index 0)
               (name-pair (stream-name-pair source-pair))
               (a-source (car source-pair))
               (b-source (cdr source-pair))
               (first-interval-pair (first interval-pairs))
               (first-exterval-pair (first exterval-pairs))
               (interval-led
                 (interval-led-p first-interval-pair first-exterval-pair))
               (final-pair (final-interval interval-pairs length-pair))
               (header-keys (when header-type-pair-sp
                              (list :header-type-pair header-type-pair)))
               (width-keys (when width-sp (list :width width)))
               (format-keys (nconc header-keys width-keys)))

          ;;
          ;; Set up the "header record" logic, below.
          ;;
          (when interval-led
            (push '((0 0) . (0 0)) exterval-pairs)
            (push '(() . ()) ex-seq-pairs))
          
          (multiple-value-bind (a-object a-type)
              (stream-presentation a-source)
            (multiple-value-bind (b-object b-type)
                (stream-presentation b-source)
              (let ((object-pair (cons a-object b-object))
                    (type-pair (cons a-type b-type)))

                (fresh-line output-stream)
                (do ((interval-pair-remains interval-pairs
                                            (rest interval-pair-remains))
                     (exterval-pair-remains exterval-pairs
                                            (rest exterval-pair-remains))
                     (in-seq-pair-remains in-seq-pairs
                                          (rest in-seq-pair-remains))
                     (ex-seq-pair-remains ex-seq-pairs
                                          (rest ex-seq-pair-remains)))
                    ((endp in-seq-pair-remains))
                  (incf interval-index)
                  (let* ((in-seq-pair (first in-seq-pair-remains))
                         (ex-seq-pair (first ex-seq-pair-remains))
                         (interval-pair (first interval-pair-remains))
                         (exterval-pair (first exterval-pair-remains))
                         (diagnostic (diagnose-pair differ-p in-seq-pair)))

                    ;;
                    ;; Write each Pair "in Series:"
                    ;;
                    (apply #'write-pair-in-series
                           in-seq-pair
                           ex-seq-pair
                           interval-pair
                           exterval-pair
                           interval-index
                           diagnostic
                           :source-pair source-pair
                           :name-pair name-pair
                           :object-pair object-pair
                           :type-pair type-pair
                           :output-stream output-stream
                           format-keys)
                    ))
          
                ;;
                ;; Write the "Finish Line" when SOME output has preceded:
                ;;
                (incf interval-index)
                (apply #'write-pair-in-series
                       nil
                       '(() . ())                ;**Disable**
                       final-pair
                       final-pair                ;**Ignored**
                       interval-index
                       'finish
                       :source-pair source-pair
                       :name-pair name-pair
                       :object-pair object-pair
                       :type-pair type-pair
                       :output-stream output-stream
                       width-keys)
                )))
          ))
      
      (when interval-pairs t))
    ))

(defun WRITE-PAIR-IN-SERIES (in-seq-pair
                             ex-seq-pair
                             &optional
                             (interval-pair nil)
                             (exterval-pair nil)
                             (interval-index nil)
                             diagnostic
                             &key
                             (header-type-pair nil header-type-pair-sp)
                             (source-pair nil)
                             (name-pair nil)
                             (output-stream *standard-output*)
                             (width nil width-sp)
                             (object-pair nil)
                             (type-pair nil)
                             &allow-other-keys)
  "Format a matched record pair vertically and send it to the output stream."
  (let ((a-index 0)
        (b-index 1)
        (a-source (car source-pair))
        (b-source (cdr source-pair))
        (a-object (car object-pair))
        (b-object (cdr object-pair))
        (a-type (car type-pair))
        (b-type (cdr type-pair))
        (a-records (car in-seq-pair))
        (b-records (cdr in-seq-pair))
        (initial-pair (when header-type-pair-sp
                        (initial-header header-type-pair in-seq-pair)))
        (head-width (if width-sp width (stream-width output-stream)))
        (width-keys (when width-sp (list :width width)))
        (prognostic 'header))
    (let ((a-initial-p (car initial-pair))
          (b-initial-p (cdr initial-pair)))
      (multiple-value-bind (header-seq-pair headerval-pair)
          (when (and header-type-pair-sp
                     (not (and a-initial-p b-initial-p)))
            (find-header header-type-pair ex-seq-pair exterval-pair))
        (let ((a-headerval (unless a-initial-p (car headerval-pair)))
              (b-headerval (unless b-initial-p (cdr headerval-pair))))

          ;;
          ;; Write out the pair of halves: one after the other,
          ;; including their internal "headers", and any records
          ;; printed here receive their Series Format headers.
          ;;
          (when a-headerval
            (let ((a-headers (car header-seq-pair)))
              (write-half-head a-index
                               headerval-pair
                               interval-index
                               name-pair
                               prognostic
                               output-stream
                               head-width
                               a-object
                               a-type)
              
              (apply #'write-frame
                     a-headers output-stream a-source a-headerval width-keys)
              ))
    
          (write-half-head a-index
                           interval-pair
                           interval-index
                           name-pair
                           diagnostic
                           output-stream
                           head-width
                           a-object
                           a-type)
    
          (unless (nonep a-records)
            (apply #'write-frame
                   a-records
                   output-stream
                   a-source
                   (car interval-pair)
                   width-keys))
    
          (when b-headerval
            (let ((b-headers (cdr header-seq-pair)))
              (write-half-head b-index
                               headerval-pair
                               interval-index
                               name-pair
                               prognostic
                               output-stream
                               head-width
                               b-object
                               b-type)
            
              (apply #'write-frame
                     b-headers output-stream b-source b-headerval width-keys)
              ))
    
          (write-half-head b-index
                           interval-pair
                           interval-index
                           name-pair
                           diagnostic
                           output-stream
                           head-width
                           b-object
                           b-type)
          
          (unless (nonep b-records)
            (apply #'write-frame
                   b-records
                   output-stream
                   b-source
                   (cdr interval-pair)
                   width-keys))
    
          )))
    (values)))

;;;
;;; Suspend Module Context:
;;;
