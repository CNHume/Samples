;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMPARE; Base: 10 -*-
;;;
;;; Source: compare-edit.lisp	Module: compare		Status:	operational
;;;
;;; History:	Please record your edits in "compare-history.text".
;;;
;;; Purpose:	Provide the Sequence Comparison Utility with a basic
;;;		Automated Source Control "Delta" Applicator.
;;;
;;; Usage:	This file is intended to be portable
;;;		to any COMMON LISP Environment.
;;;
;;; Compile:	Cf. "compare:compare;compare.lisp"
;;;
;;; Contents:
;;;
;;;	This file provides the DELTA-FILE and DELTA-EDIT Macros.
;;;
;;;	A LISP Reader can be used to process Delta Format Output conveyed
;;;	via the macros below.  (The Hash Code prevents an edit from being
;;;	applied to any inappropriate interval of text.)
;;;
;;; NOTE!  In the interest of efficiency: DELTA-EDIT record intervals
;;;	   are presumed to occur in (ascending) order.  It is further
;;;	   required that these intervals DO NOT OVERLAP.
;;;
;;; Externally Visible (Application) Macros:
;;;
;;;	delta-file		delta-edits &key input-file output-file
;;;				output-hash hash-version
;;;
;;;	delta-edit		inserted-records &key start end hash index
;;;
;;; Condition System Interface Macros:
;;;
;;;	sequence-index-error-f	sequence index
;;;
;;; Local Interfaces:
;;;
;;;	edit-file		delta-edits &key input-file output-file
;;;				output-hash hash-version
;;;
;;;	edit-stream		delta-edits input-stream &optional
;;;				output-stream &key output-hash hash-version
;;;
;;;	nsplice			inserts edits start &optional end
;;;

;;;
;;; Resume Module Context:
;;;
(in-package compare)

;;; No Requirements.
;;; No Shadows.
;;; No Unusual Packages.
;;; Nothing to Import.

;;;
;;; Exported Interfaces:
;;;
(export '(delta-file delta-edit merge-file merge-edit))

;;;
;;; Special Variables and Constants:
;;;
(defparameter *EDIT-RECORD-WRITER* 'write-records
  "The Standard Edit Record Writer")

;;;
;;; NOTE:  The DELTA-FILE form is used to specify the name of a file to
;;;	   which the subsequent DELTA-EDIT forms are intended to apply.
;;;
;;;	   The MERGE-FILE form is used to specify the name of a file to
;;;	   which the subsequent MERGE-EDIT forms are intended to apply.
;;;
;;;	   The "Merge Interfaces" are issued where DIFFER-P is NIL.
;;;	   It is not obvious whether or not useful application for
;;;	   this somewhat obscure usage will be found.  Therefore,
;;;	   macro expansion of these interfaces has not been provided.
;;;
;;; Macros:
;;;
(defmacro DELTA-FILE (delta-edits &rest keys)
  "Apply a Delta to the Input File, producing the Output File."
  ;;
  ;; These Delta Macros allow a Lisp Reader to process the Lisp-like
  ;; Syntax of the Delta Output Format.  It is not clear, however,
  ;; that this "language" should support Lisp-like Term Evaluation.
  ;;
  `(apply #'edit-file `(,,.delta-edits) ',keys))

(defmacro DELTA-EDIT (inserted-records &rest keys)
  "Produce a Delta Editor argument list for this Edit."
  `'(,inserted-records . ,keys))

(defmacro SEQUENCE-INDEX-ERROR-F (sequence index)
  "Generate a Range Error Signaller, for Sequences."
  #-symbolics
  `(error "The index ~S (~S) is not in range for the sequence ~S."
	  ,index ',index ,sequence)
  #+symbolics
  `(cli::sequence-index-error ,sequence ,index ',index))

;;;
;;; Now for the Code:
;;;
(defun EDIT-FILE (delta-edits
		  &key
		  input-file
		  (output-file nil output-file-sp)
		  (output-hash nil output-hash-sp)
		  (hash-version nil hash-version-sp))
  "Apply each Edit to the Input File, producing the Output File."
  (let ((hash-keys (nconc (when output-hash-sp
			    (list :output-hash output-hash))
			  (when hash-version-sp
			    (list :hash-version hash-version)))
		   ))
    (with-open-file (input-stream input-file)
      (if output-file-sp
	  (with-open-file (output-stream output-file :direction :output)
	    (apply #'edit-stream
		   delta-edits input-stream output-stream hash-keys))
	  (apply #'edit-stream
		 delta-edits input-stream *standard-output* hash-keys)))
    output-file))

(defun EDIT-STREAM (delta-edits
		    input-stream
		    &optional
		    (output-stream *standard-output*)
		    &key
		    output-hash
		    (hash-version nil))
  "Apply each Edit to the Input Stream, producing the Output Stream."
  (declare (special *record-reader* *edit-record-writer*))
  (let ((edits (coerce (funcall *record-reader* input-stream) 'list))
	(edit-last ())
	(position 0)
	(new-index 0))
    (flet
      ((EDITOR (inserts &key start end hash index)
	 "Perform an Edit in the enclosing Record Context."
	 (let* ((edit-rest (if edit-last (rest edit-last) edits))
		(start (- start position))
		(end (- end position))
		(old-hash hash)
		(new-hash (when old-hash
			    (hash-records edit-rest 0 hash-version
					  :start start :end end)
			    ))
		(old-index index))
	   (incf new-index)
	   (unless (or (null old-index) (= new-index old-index))
	     (cerror "Proceed with this edit, counting from recorded index."
		     "Recorded edit index (~D) departs from count (~D)."
		     old-index
		     new-index)
	     ;;
	     ;; Attempt to re-synchronize:
	     ;;
	     (setf new-index old-index))
	   
	   (if (or (null old-hash) (= new-hash old-hash))
	       (multiple-value-bind (new-edits new-last)
		   ;;
		   ;; Protecting the Insert List permits wider
		   ;; application than that of Delta Macros.
		   ;;
		   (nsplice #-symbolics (copy-list inserts)
			    ;; Advise Lisp Machine: RPLACD of LAST is imminent.
			    #+symbolics (scl:copy-list* inserts)
			    edit-rest start end)
		 ;;
		 ;; Incremental editing requires proper
		 ;; maintenance of some external state:
		 ;;
		 (when (zerop start)
		   (if edit-last
		       (setf (rest edit-last) new-edits)
		       (setq edits new-edits)))
		 (when new-last (setq edit-last new-last))
		 (incf position end))
	       ;;
	       ;; Either the :HASH value is incorrect, or the "A Records" were
	       ;; other than those anticipated.  One possible reason for this
	       ;; is that the :START or :END index could be incorrect, so no
	       ;; attempt is made to "ratchet forward" where this error occurs.
	       ;;
	       (cerror "Attempt to proceed with the delta, omitting this edit."
		       "Deleted record hash (~D) different than expected (~D)."
		       new-hash
		       old-hash)
	       ))
	 ))

      ;;
      ;; Perform the Edits here:
      ;;
      (dolist (edit-args delta-edits) (apply #'editor edit-args)))

    (let* ((old-hash output-hash)
	   (new-hash (when old-hash
		       (hash-records edits 0 hash-version)
		       )))
      (unless (or (null old-hash) (= new-hash old-hash))
	(cerror "Write out the edited file result anyway."
		"File result hash (~D) different than expected (~D)."
		new-hash
		old-hash)))

    (funcall *edit-record-writer* edits output-stream)
    ))

;;;
;;; Another flavor of SPLICE, effecting a COPY-LIST[*]
;;; of Inserts internally, might have been preferable.
;;;
;;; The function below, granted license to modify the
;;; Insert List, has been renamed NSPLICE:
;;;
(defun NSPLICE (inserts edits start &optional end)
  "Splice some Inserts (destructively), where they go: into the Edits."
  (declare (values new-edits new-last))
  ;;
  ;; Ensure well formed Range Specifications:
  ;;
  (when (and end (minusp end))
    (sequence-index-error-f edits end))
  (when (or (minusp start) (and end (> start end)))
    (sequence-index-error-f edits start))

  (multiple-value-bind (edit-rest prefix-last)
      ;;
      ;; Counting LENGTH CONSes is avoided.  Restarting from
      ;; any of the Index Errors discovered within this COND
      ;; would therefore require restarting the COND.
      ;;
      (cond ((plusp start)
	     (let ((prefix-last (nthcdr (1- start) edits)))
	       (unless prefix-last (sequence-index-error-f edits start))
	       (let ((edit-last (and end
				     (nthcdr (- end start) prefix-last)
				     )))
		 (unless (or edit-last (null end))
		   (sequence-index-error-f edits end))
		 (values (rest edit-last) prefix-last)
		 )))
	    ;;
	    ;; In the following cases: no "prefix" was saved,
	    ;; and so PREFIX-LAST will simply be bound to ().
	    ;;
	    ((and end (plusp end))
	     (let ((edit-last (nthcdr (1- end) edits)))
	       (unless edit-last (sequence-index-error-f edits end))
	       (rest edit-last)
	       ))
	    (t edits))

    (let ((insert-last (last inserts))
	  (append-rest (or inserts edit-rest)))
      ;;
      ;; Following any new inserts (which will be appended below,
      ;; at START index) will be pre-existing EDIT-REST elements.
      ;; These are appended here:
      ;;
      (when insert-last (setf (rest insert-last) edit-rest))

      ;;
      ;; Insert the concatenation of any new INSERTS and any
      ;; pre-existing "prefix" elements, at the START index:
      ;;
      (when (plusp start) (setf (rest prefix-last) append-rest))

      (values (if (plusp start) edits append-rest)
	      ;;
	      ;; Ratchet forward: maintaining progress along EDITS.
	      ;;
	      (or insert-last prefix-last))
      )))

;;;
;;; Suspend Module Context:
;;;
