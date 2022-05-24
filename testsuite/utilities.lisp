;;;; utilities.lisp — Utilities for Confidence tests

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.confidence/testsuite)

(defun ensure-unwrap (form)
  (labels ((unwrap-record (form)
	     (if (and (eq 2 (length form))
		      (eq 'confidence::record-testcase-result (first form)))
		 (unwrap-supervise (second form))
		 form))
	   (unwrap-supervise (form)
	     (if (and (eq 2 (length form))
		      (eq 'confidence::supervise-assertion (first form)))
		 (second form)
		 form)))
    (unwrap-record form)))

(dolist (ensure-macro '(ensure-success ensure-failure ensure-condition))
  (setf (get ensure-macro :org.melusina.confidence/testcase) t))

(define-testcase ensure-success-1 (result)
  (assert-type result 'assertion-success))

(defmacro ensure-success (form)
  "Ensure that FORM yield an assertion success."
  `(ensure-success-1 (confidence::supervise-assertion ,(ensure-unwrap form))))

(define-testcase ensure-failure-1 (result &optional description-pattern)
  (assert-type result 'assertion-failure)
  (when (and description-pattern (typep result 'assertion-failure))
    (assert-string-match (slot-value result 'confidence::description) description-pattern)))
  
(defmacro ensure-failure (form &optional description-pattern)
  "Ensure that FORM yield an assertion failure.
When DESCRIPTION-PATTERN is provided, it also ensures that the error
description satisfies this pattern."
  `(ensure-failure-1
    (confidence::supervise-assertion ,(ensure-unwrap form))
    ,description-pattern))

(define-testcase ensure-condition-1 (result &optional condition-type)
  (assert-type result 'assertion-condition)
  (when (and condition-type (typep result 'assertion-condition))
    (assert-type (slot-value result 'confidence::condition) condition-type)))
  
(defmacro ensure-condition (form &optional (condition-type 'condition))
  "Ensure that FORM yield an assertion condition.
When CONDITION-TYPE is provided, it also ensures that the signalled condition
has the required type."
  `(ensure-condition-1
    (confidence::supervise-assertion ,(ensure-unwrap form))
    ,condition-type))


;;;;
;;;; Testing STRING-MATCH
;;;;

(define-testcase testsuite-string-match ()
  (assert-t (confidence::string-match "" ""))
  (assert-t (confidence::string-match "a" "a"))
  (assert-t (confidence::string-match "a*" "a"))
  (assert-t (confidence::string-match "a*" "ab"))
  (assert-t (confidence::string-match "a*" "abc"))
  (assert-t (confidence::string-match "a?" "ab"))
  (assert-t (confidence::string-match "a*a" "aba"))
  (assert-t (confidence::string-match "a*a" "abca"))
  (assert-t (confidence::string-match "a?a" "aba"))
  (assert-nil (confidence::string-match "*a" "b"))
  (let ((pattern
	  "*The parameter STRING1 is expected to have type STRING but actually has type*")
	(text
	  #.(concatenate 'string
			 "The assertion (ASSERT-STRING= STRING1 STRING2) is true, iff STRING1 "
			 "and STRING2" '(#\Newline) "satisfy the STRING= predicate." '(#\Newline)
			 "This assertion supports the same keyword parameters as STRING=."
			 '(#\Newline) "The parameter STRING1 is expected to have type STRING "
			 "but actually has type BOOLEAN.")))
    (assert-t (confidence::string-match pattern text))))

(define-testcase testsuite-utilities ()
  (testsuite-string-match))

;;;; End of file `utilities.lisp'
