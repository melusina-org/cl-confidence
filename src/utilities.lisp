;;;; utilities.lisp — Utilities for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.confidence)


;;;;
;;;; RANDOM-STRING
;;;;

(defparameter *alphabet-hexadecimal* "0123456789abcdef"
  "The set of hexadecimal characters.")

(defparameter *alphabet-base36* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The set of characters used for base 36 encoding.")

(defparameter *alphabet-base64* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz/-"
  "The set of characters used for base 64 encoding.")

(defun random-string (&optional (length 32) (alphabet *alphabet-base36*))
  "Prepare a random alphabetic string of given LENGTH.

The returned string contains LENGTH characters chosen from
the vector ALPHABET.  When ALPHABET is one of the keywords

  :HEXADECIMAL :BASE36 and :BASE64

the corresponding alphabet is used.

This uses a very weak method that does not try to avoid collisions.x"
  (flet ((actual-alphabet (alphabet)
	   (case alphabet
	     (:hexadecimal
	      *alphabet-hexadecimal*)
	     (:base36
	      *alphabet-base36*)
	     (:base64
	      *alphabet-base64*)
	     (t
	      alphabet))))
    (loop :with id = (make-string length)
	  :with actual-alphabet = (actual-alphabet alphabet)
          :with alphabet-length = (length actual-alphabet)
          :for i :below length
          :do (setf (aref id i)
                    (aref actual-alphabet (random alphabet-length)))
          :finally (return id))))


;;;;
;;;; TESTSUITE-IDENTIFICATION
;;;;

(defparameter *testsuite-name* "TESTSUITE"
  "The name for the testsuite.

Usually TESTSUITE but common values are ACCEPTANCE, INTEGRATION, PREFLIGHT, etc.")

(defparameter *testsuite-id* (random-string 7 :base36)
  "A random identfier for the current testsuite run batch.")




;;;;
;;;; STRINGCAT
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun stringcat (&rest strings)
    "Concatenate STRINGS.
This is a shortand which is useful in combination to reader macros."
    (apply #'concatenate 'string strings)))


;;;;
;;;; STRING-MATCH
;;;;

(defun string-match (pattern text)
  "Predicate recognising TEXT matching a globbing PATTERN."
  (let ((text-length (length text))
        (pattern-length (length pattern)))
    (labels
        ((match-step (i j)
           (case (when (and (<= j text-length) (< i pattern-length))
		   (elt pattern i))
             ((nil)
              (eq j text-length))
             (#\?
              (and (< j text-length) (match-step (1+ i) (1+ j))))
             (#\*
	      (or (match-step (1+ i) j) (match-step i (1+ j))))
             (t
              (when (< j text-length)
                (and (char= (elt pattern i) (elt text j))
                     (match-step (1+ i) (1+ j))))))))
      (match-step 0 0))))

;;;; End of file `utilities.lisp'
