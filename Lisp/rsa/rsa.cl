;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;
;;; Source: rsa.lisp		Library: network	Status:	operational
;;;
;;; Author     Version	Edit Date	Purpose of Edit
;;; ------     -------	---------	---------------
;;; Chris Hume	 1.3	19-Aug-91	Divided OBVIOUS-COMPOSITE by 91.
;;; Chris Hume	 1.2	 7-Aug-89	More clean up.
;;; Chris Hume	 1.1	29-Jun-88	Dressed up for Distribution.
;;; Chris Hume	 1.0	 8-Jun-88	Created file.
;;;
;;; Purpose:	RSA Public Key Encryption Laboratory
;;;
;;; Usage:	This file is intended to be portable to any COMMON LISP
;;;		Environment.
;;;
;;; Compile:	(compile-file "rsa")
;;;
;;; Contents:
;;;
;;;	mod-expt		base exponent modulus
;;;	jacobi			a b
;;;	ss-iteration		a b
;;;	ss-test			candidate :trial-limit
;;;	prime-test		candidate :trial-limit
;;;	generate-prime		:trial-limit :decimal-length
;;;	generate-next-prime	number :trial-limit
;;;	random-decimal-length	:variation-center :variation-percent
;;;	generate-p-and-q	:trial-limit :decimal-length
;;;	mod-inverse		number modulus
;;;	generate-d-and-e	:trial-limit
;;;	pack-reducer		reduction term
;;;	pack			string
;;;	unpack-reducer		reduction term
;;;	unpack			number
;;;	encode			clear-text
;;;	decode			code-text
;;;	string-append		&rest strings
;;;
;;; References:	Detailed description of the RSA algorithm can be found in
;;;		the paper "A Method for Obtaining Digital Signatures and
;;;		Public-Key Cryptosystems" by R.L. Rivest, A. Shamir, and
;;;		L. Adleman published in the February 1978 Communications
;;;		of the ACM [Volume 21, Number 2, pp. 120-126].  The paper
;;;		was republished in the more valuable January 1983 25th
;;;		Anniversary Issue [Volume 26, Number 1, pp. 96-99] where
;;;		a number of similarly useful articles have been collected
;;;		(although in a half-scale "landscape" format.)
;;;

(defconstant GRAPHIC-BIAS (char-code #\Space)
  "Space: The Smallest Graphic ASCII Character Code")

(defconstant CHARACTER-MODULUS 100
  "The number of characters available for composition of clear text")

(defparameter LENGTH-DEFAULT 100
  "The default digit length for randomly generated numbers")

(defparameter TRIAL-LIMIT-DEFAULT 48
  "The default number of times to run the primality test.")

(defparameter SMALL-PRIMES
  '(2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 67 71 73 79 83 89 97)
  "The first 24 primes")

(defparameter OBVIOUS-COMPOSITE
  (apply #'* small-primes)
  "A product of (the first 24) small primes")

(defvar P
  "The first prime factor of N")

(defvar Q
  "The second prime factor of N")

(defvar N
  "The publicly available composite modulus")

(defvar E
  "The publicly available encryption exponent")

(defvar D
  "The secretly held decryption exponent")

(defun MOD-EXPT (base exponent modulus)
  "Exponentiate by repeated squaring and multiplication."
  (do ((power-index (integer-length exponent) (1- power-index))
       (value 1))
      ((< power-index 0) value)
      (setq value (mod (* value value) modulus))
      (when (logbitp power-index exponent)
	    (setq value (mod (* value base) modulus))
	    )
      ))

(defun JACOBI (a b)
  "Apply the Jacobi Operator to a pair of values."
  (if (= a 1)
      1
    (if (evenp a)
	(* (jacobi (/ a 2) b)
	   (expt -1 (/ (1- (* b b)) 8)))
      (* (jacobi (mod b a) a)
	 (expt -1 (/ (* (1- a) (1- b)) 4)))
      )
    ))

(defun SS-ITERATION (a b)
  "Run one iteration of the Solovay and Strassen primality test."
  (and (= 1 (gcd a b))
       (= (mod (jacobi a b) b)
	  (mod-expt a (/ (1- b) 2) b))
       ))

(defun SS-TEST (candidate
		&rest keys
		&key (trial-limit trial-limit-default)
		&allow-other-keys)
  "Obtain a large random number, which is very probably Prime."
  (let ((prime-found nil)
        (prime-trials 0))
    (setq prime-found
          (cond ((> 2 candidate) nil)
                ((= 2 candidate) t)
                ((evenp candidate) nil)
                (t (do ((trial 0 (1+ trial))
                        (ss-iteration-held t))
                       ((or (not ss-iteration-held) (= trial trial-limit))
                        (setq prime-trials trial)
                        (if (< 0 trial)
                            (format t "Chance of ~A being composite less than ~A.~%"
                              candidate
                              (expt 2 (- trial))
                              ))
                        ss-iteration-held)
                     
                     (let ((a (1+ (random (1- candidate)))))
                       (if (not (ss-iteration a candidate))
                           (setq ss-iteration-held nil))
                       )
                     ))
                ))    
    (values prime-found prime-trials)))

(defun PRIME-TEST (candidate
		   &rest keys)
  "Perform a thorough and efficient primality test."
  (let ((obvious-factor (gcd candidate obvious-composite))
	(test-held nil))
    (if (/= 1 obvious-factor)
	(format t "Obvious factor found for ~A (e.g., ~A).~%"
		candidate
		obvious-factor)
      (multiple-value-bind (prime-found prime-trials)
			   (apply #'ss-test candidate keys)
			   (if prime-found
			       (setq test-held t)
			     (format t "~3A iteration~A to reject ~A.~%"
				     prime-trials
				     (if (= 1 prime-trials)
					 " " "s")
				     candidate)
			     ))
      )
    test-held))

(defun GENERATE-PRIME (&rest keys
			     &key (decimal-length length-default)
			     &allow-other-keys)
  "Generate a large prime number."
  (let* ((order (expt 10 (1- decimal-length)))
	 (displacement (random (* 9 order)))
	 (base (+ order displacement)))
    (if (evenp base) (setq base (1+ base)))
    (do  ((candidate base (+ 2 candidate))
	  (prime ())
	  (generation 1 (1+ generation)))
	 (prime prime)
	 (if (apply #'prime-test candidate keys)
	     (setq prime candidate))
	 (format t "[End of Generation ~A]~%" generation)
	 )))

(defun GENERATE-NEXT-PRIME (number
			    &rest keys)
  "Find the next prime P following a number such that P-1 will have a
   large prime factor."
  (do ((even-index 2 (+ 2 even-index))
       (candidate ())
       (prime-found nil))
      (prime-found candidate)
      (setq candidate (1+ (* even-index number)))
      (if (apply #'prime-test candidate keys)
	  (setq prime-found t))
      ))

(defun RANDOM-DECIMAL-LENGTH (&rest keys
				    &key (variation-center length-default)
				    (variation-percent .07)
				    &allow-other-keys)
  "Generate a random number near the decimal length default."
  (let* ((variation-radius (floor (* variation-percent variation-center)
				  length-default))
	 (variation-diameter (* 2 variation-radius))
	 (variation-position (random (1+ variation-diameter))))
    (+ variation-position (- variation-center variation-radius))))

(defun GENERATE-P-AND-Q (&rest keys
			       &key (decimal-length (* 2 length-default))
			       (gcd-limit (* 2 3 5 7))
			       &allow-other-keys)
  "Generate two large primes whose product is nearly on the order specified."
  (let* ((p2-decimal-length (apply #'random-decimal-length
				   :variation-center (floor decimal-length 2)
				   keys))
	 (q2-decimal-length (- decimal-length p2-decimal-length)))
    
    (do ((attempt 1 (1+ attempt))
	 (gcd-allowed nil)
	 p2 q2
	 p1 q1
	 p0 q0)
	(gcd-allowed (setq p p0
			   q q0))
	
	;;
	;; Each of the two primes is carefully constructed
	;; (in three steps) so as to render the possibility
	;; of their discovery (through an attempt to factor
	;; their product: n) less likely.  Please refer to
	;; the article by Rivest, Shamir, and Adleman for
	;; further discussion of this "enhancement".
	;;
	(format t "~%[P2 and Q2 candidates on attempt ~A.]~%" attempt)
	
	(setq p2 (apply #'generate-prime
			:decimal-length (- p2-decimal-length 3)
			keys)
	      q2 (apply #'generate-prime
			:decimal-length (- q2-decimal-length 3)
			keys))
	
	(format t "~%[P1 and Q1 candidates on attempt ~A.]~%" attempt)
	
	(setq p1 (apply #'generate-next-prime p2 keys)
	      q1 (apply #'generate-next-prime q2 keys))
	
	(format t "~%[P0 and Q0 candidates on attempt ~A.]~%" attempt)
	
	(setq p0 (apply #'generate-next-prime p1 keys)
	      q0 (apply #'generate-next-prime q1 keys))
	
	(let ((gcd-value (gcd (1- p0) (1- q0))))
	  (if (>= gcd-limit gcd-value)
	      (setq gcd-allowed t)
	    (format t "The value of gcd( ~A, ~A ) = ~A ~
		    came in above the allowed limit ~A.~%"
		    (1- p0)
		    (1- q0)
		    gcd-value
		    gcd-limit))
	  )
	)
    
    (setq n (* p q))
    ))

(defun MOD-INVERSE (number modulus)
  "Obtain the inverse of a number with respect to its modulus."
  (do ((numerator modulus)
       (denominator number)
       (prev-b 0)
       (b 1))
      ((= 0 denominator) prev-b)
    
    (multiple-value-bind (quotient remainder)
                         (floor numerator denominator)
      (psetq prev-b b
             b (mod (- prev-b (* quotient b))
                    modulus)
             numerator denominator
             denominator remainder)
      #+:test-mod
      (format t "~&N =~5D, D =~5D, Q =~5D, B'=~5D, B =~5D~%"
              numerator denominator quotient prev-b b))
    ))

(defun GENERATE-D-AND-E (&rest keys)
  "Obtain D and E via P and Q."
  (let* ((e-limit (+ (log p 2) (log q 2)))
	 (totient (* (1- p) (1- q)))
	 (max-p-and-q (max p q))
	 (d-decimal-length (ceiling (log max-p-and-q 10))))
    
    (do ((attempt 1 (1+ attempt))
	 (e-allowed nil)
	 d-candidate
	 e-candidate)
	(e-allowed (setq d d-candidate
			 e e-candidate))
	
	(format t "~%[D and E candidates on attempt ~A.]~%" attempt)
	
	(setq d-candidate (apply #'generate-prime
				 :decimal-length d-decimal-length
				 keys)
	      e-candidate (mod-inverse d-candidate totient))
	
	(if (>= e-candidate e-limit)
	    (setq e-allowed t)
	  (format t "The inverse ~A of ~A was below the limit: ~A.~%"
		  e-candidate d-candidate e-limit))
	)
    ))

(defun PACK-REDUCER (reduction term)
  "Reduce ASCII Characters into Integers (within the character modulus.)"
  (let ((code (cond ((char= term #\Space) 99)
		    ((char= term #\Tab) 98)
		    ((char= term #\Newline) 97)
		    ((char= term #\Page) 96)
		    ((char= term #\Rubout) 95)
		    ((and (standard-char-p term) (graphic-char-p term))
		     (- (char-code term) graphic-bias))
		    (t 0))
	      ))
    
    (unless :debugging-reduction
	    (format t "(code ~A) = ~A~%" term code)
	    (format t "reduction = ~A~%" reduction)
	    )
    
    (if reduction (+ code (* character-modulus reduction)) code)
    ))

(defun PACK (string)
  "Pack an ASCII string into an Integer."
  (reduce #'pack-reducer string :initial-value 0))

(defun UNPACK-REDUCER (reduction term)
  "Reduce Integers (within the character modulus) into ASCII Characters."
  (let ((char (cond ((= term 99) #\Space)
		    ((= term 98) #\Tab)
		    ((= term 97) #\Newline)
		    ((= term 96) #\Page)
		    ((= term 95) #\Rubout)
		    ((= term 0) #\Null)
		    (t (code-char (+ term graphic-bias)))
		    )))
    
    (unless :debugging-reduction
	    (format t "(char ~A) = ~A~%" term char)
	    (format t "reduction = ~A~%" reduction)
	    )
    
    (if reduction (string-append reduction (string char)) (string char))
    ))

(defun UNPACK (number)
  "Unpack an Integer back into an ASCII string."
  (let ((expansion ()))
    (do ((numerator number)
	 (denominator character-modulus))
	((= numerator 0))
	
	(multiple-value-bind (quotient remainder)
			     (floor numerator denominator)
			     
			     (setq expansion (cons remainder expansion)
				   numerator quotient)
			     ))
    
    (reduce #'unpack-reducer expansion :initial-value "")
    ))

(defun ENCODE (clear-text
	       &rest keys
	       &key (exponent e) (modulus n)
	       &allow-other-keys)
  "Perform the RSA encryption algorithm."
  (let ((clear-base (pack clear-text)))
    (if (>= clear-base modulus)
	(warn "Clear Text must be reblocked to prevent degeneration.")
      (let ((code-base (mod-expt clear-base exponent modulus)))
	(unpack code-base))
      )))

(defun DECODE (code-text
	       &rest keys
	       &key (exponent d) (modulus n)
	       &allow-other-keys)
  "Perform the RSA decryption algorithm."
  (let ((code-base (pack code-text)))
    (if (>= code-base modulus)
	(warn "Code Text exceeds anticipated range."))
    
    (let ((clear-base (mod-expt code-base exponent modulus)))
      (unpack clear-base))
    ))

(defun STRING-APPEND (&rest strings)
  "Append string appends into CLtL."
  (apply #'concatenate 'string strings))
