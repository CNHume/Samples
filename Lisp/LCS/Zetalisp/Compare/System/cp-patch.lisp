;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;;
;;; Source: cp-patch.lisp        Module: compare                Status:        operational
;;;
;;; Author     Version        Edit Date        Purpose of Edit
;;; ------     -------        ---------        ---------------
;;; Chris Hume         1.8        16-Jan-91        Added ZWEI:COM-SET-COMPARE-FORMAT.
;;; Chris Hume         1.7         1-Jan-91        Adjusted documentation.
;;; Chris Hume         1.6         3-Jul-90        Added Merge Format.
;;; Chris Hume         1.5         6-Jun-90        Added Patch-File Attribute.
;;; Chris Hume         1.4        29-May-90        Added the :AFFIX keyword.
;;; Chris Hume         1.3        21-May-90        Moved SOURCE-COMPARE to "srccom-patch".
;;; Chris Hume         1.2        20-May-90        Prepared for an :AFFIX keyword.
;;; Chris Hume         1.1        26-Apr-90        Added formatting keywords.
;;; Chris Hume         1.0        26-Apr-90        Created file.
;;;
;;; Purpose:        Patch the Sequence Comparison Utility into CP.
;;;
;;; Usage:        This file assumes the Symbolics Common Lisp Environment.
;;;
;;; Compile:        (compile-file "compare:compare;cp-patch")
;;;
;;; Contents:
;;;
;;;        This file supersedes the definition of SI:COM-SHOW-DIFFERENCES.
;;;
;;; (Redefined) External Interfaces:
;;;
;;;        com-show-differences        File-1 File-2
;;;                                :Affix
;;;                                :Ignore (Indentation Case-and-Style)
;;;                                :Format (Delta Merge Parallel Review Series)
;;;                                :Width
;;;
;;; (Original) External Interfaces:
;;;
;;;        com-set-compare-format
;;;

;;;
;;; Module Prologue:
;;;
;;; Stay in the Default Package.
(require 'srccom-patch "srccom-patch")

;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.
;;; Nothing to Export.

;;;
;;; Presentation Types:
;;;
(scl:define-presentation-type SRCCOM:COMPARE-FORMAT ()
   :printer ((format stream) (princ (string-capitalize format) stream))
   :description "a Compare Format"
   :parser ((stream)
            (locally
              (declare (special srccom:*compare-format-keywords*))
              (dw:complete-from-sequence srccom:*compare-format-keywords*
                                         stream
                                         :type 'srccom:compare-format
                                         :name-key #'string-capitalize)
              )))

;;;
;;; Now for the Code:
;;;
(cp:define-command (SI:COM-SHOW-DIFFERENCES :command-table "Global")
    ((file-1 'fs:pathname :confirm t :prompt "first file"
             :documentation "First file to be compared")
     (file-2 'fs:pathname :confirm t :prompt "to file"
             :documentation "Second file to be compared")
     &key
     ((:ignore normalizations) '((scl:subset :indentation :case-and-style))
      :default nil
      :prompt nil
      :documentation "Aspects to ignore when searching for file differences:
        Indentation        whitespace surrounding lines
        Case-and-Style        alphabetic case and character style")
     (affix 'number
            :default nil
            :documentation "The amount of record context \"affixed\" to any difference")
     (format 'srccom:compare-format
             :default nil
             :prompt nil
             :documentation "File differences can be presented in the following formats:
        Delta                as an automated source code maintenance system \"delta\"
        Merge                full review of the \"later\" file-2, with differences in series
        Parallel        in two adjacent columns, displaying differences side by side
        Review                with change bars, fully reviewing the \"later\" file-2
        Series                as full width halves, interleaving display of differences")
     (width 'number
            :default nil
            :documentation "A width within which formatted output will be constrained"))
   ;;
   ;; In spite of :confirm t, file-2 can be nil when using noun-verb order,
   ;; so read the argument here:
   ;;
   (unless file-2
     (setq file-2 (accept 'fs:pathname
                          :default (send file-1 :new-version :newest)
                          :confirm t
                          :prompt "compare to file")))
   (let ((format-keys (nconc (when affix (list :affix affix))
                             (when format (list :format format))
                             (when width (list :width width)))
                      ))
     (labels
       ((COMPARE (file &optional deleted)
          (condition-case (error)
               (condition-case-if (not deleted) ()
                    (apply #'srccom:source-compare
                           file
                           (send file-1 :translate-wild-pathname
                                 (fs:merge-pathnames file-2 file-1)
                                 file)
                           *standard-output*
                           t
                           :ignore-case-and-style (member :case-and-style normalizations)
                           :ignore-whitespace (member :indentation normalizations)
                           :deleted deleted
                           format-keys)
                  (fs:file-not-found (compare file t)))
             (fs:file-operation-failure (format t "~&Error: ~~A~" error))
             )))
       (if (send file-1 :wild-p)
           (condition-case (err-or-files)
                (rest (fs:directory-list file-1 :sorted))
              (fs:file-operation-failure (format t "~&Error: ~~A~" err-or-files))
              (:no-error (loop for (file) in err-or-files
                               do (compare file))))
           (compare file-1)
           ))
     ))

(zwei:defcom ZWEI:COM-SET-COMPARE-FORMAT
             "Changes the Output Format used by m-X Source Compare."
             ()
  (declare (special srccom:*compare-format-selected*
                    srccom:*compare-format-keywords*))
  (let ((compare-format
          (zwei:typein-line-accept
            'srccom:compare-format
            :prompt
            (format nil
                    "Enter a Compare Format (~@[present value ~A, ~]use ~C for choices)"
                    (when srccom:*compare-format-selected*
                      (string-capitalize srccom:*compare-format-selected*))
                    #\Help))
          ))
    (setq srccom:*compare-format-selected*
          (find compare-format srccom:*compare-format-keywords*)))
  zwei:dis-none)

;;;
;;; Module Epilogue:
;;;
(provide 'cp-patch)
