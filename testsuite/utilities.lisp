;;;; utilities.lisp — Utilities for Confidence tests

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.confidence/testsuite)

(defun ensure-unwrap (form)
  (if (and (eq 2 (length form))
	   (eq 'confidence::instrument-assertion (first form)))
      (second form)
      form))

(dolist (ensure-macro '(ensure-success ensure-failure ensure-condition))
  (setf (get ensure-macro :org.melusina.confidence/testcase) t))

(defmacro ensure-success (form)
  "Ensure that FORM yield an assertion success."
  `(handler-case
       ,form
     (confidence::assertion-success (condition)
       (signal 'confidence::assertion-success
	       :path (confidence:assertion-path condition)
	       :name (confidence:assertion-name condition)
	       :argument-names (confidence:assertion-argument-names condition)
	       :argument-values (confidence:assertion-argument-values condition)
	       :form (confidence:assertion-form condition)
	       :type (confidence:assertion-type condition)))
     (confidence::assertion-outcome (condition)
       (signal 'confidence::assertion-failure
	       :path (confidence:assertion-path condition)
	       :name (confidence:assertion-name condition)
	       :argument-names (confidence:assertion-argument-names condition)
	       :argument-values (confidence:assertion-argument-values condition)
	       :form (confidence:assertion-form condition)
	       :type (confidence:assertion-type condition)))))
  
(defmacro ensure-failure (form &optional description-pattern)
  "Ensure that FORM yield an assertion failure.
When DESCRIPTION-PATTERN is provided, it also ensures that the error
description satisfies this pattern."
  (alexandria:once-only (description-pattern)
    `(handler-case
	 ,form
       (confidence::assertion-failure (condition)
	 (if (or (not ,description-pattern)
		 (confidence::string-match ,description-pattern (confidence:assertion-description condition)))
	     (signal 'confidence::assertion-success
		     :path (confidence:assertion-path condition)
		     :name (confidence:assertion-name condition)
		     :argument-names (confidence:assertion-argument-names condition)
		     :argument-values (confidence:assertion-argument-values condition)
		     :form (confidence:assertion-form condition)
		     :type (confidence:assertion-type condition))
	     (signal 'confidence::assertion-failure
		     :path (confidence:assertion-path condition)
		     :name (confidence:assertion-name condition)
		     :argument-names (confidence:assertion-argument-names condition)
		     :argument-values (confidence:assertion-argument-values condition)
		     :form (confidence:assertion-form condition)
		     :type (confidence:assertion-type condition)
		     :description (format nil "The description~%~%  ~S~%~%does not match the pattern ~S." 
					  (confidence:assertion-description condition) ,description-pattern))))
       (confidence::assertion-outcome (condition)
	 (signal 'confidence::assertion-failure
		 :path (confidence:assertion-path condition)
		 :name (confidence:assertion-name condition)
		 :argument-names (confidence:assertion-argument-names condition)
		 :argument-values (confidence:assertion-argument-values condition)
		 :form (confidence:assertion-form condition)
		 :type (confidence:assertion-type condition))))))

(defmacro ensure-condition (form &optional (condition-type 'condition))
  "Ensure that FORM yield an assertion condition.
When CONDITION-TYPE is provided, it also ensures that the signalled condition
has the required type."
  (alexandria:once-only (condition-type)
    `(handler-case
	 ,form
       (confidence::assertion-condition (condition)
	 (if (typep (confidence:assertion-condition condition) ,condition-type)
	     (signal 'confidence::assertion-success
		     :path (confidence:assertion-path condition)
		     :name (confidence:assertion-name condition)
		     :argument-names (confidence:assertion-argument-names condition)
		     :argument-values (confidence:assertion-argument-values condition)
		     :form (confidence:assertion-form condition)
		     :type (confidence:assertion-type condition))
	     (signal 'confidence::assertion-failure
		     :path (confidence:assertion-path condition)
		     :name (confidence:assertion-name condition)
		     :argument-names (confidence:assertion-argument-names condition)
		     :argument-values (confidence:assertion-argument-values condition)
		     :form (confidence:assertion-form condition)
		     :type (confidence:assertion-type condition)
		     :description (format nil "The form yielded a condition~%~%  ~S~%~%which is not a subtype of condition ~S." 
					  (confidence:assertion-condition condition) ,condition-type))))
       (confidence::assertion-outcome (condition)
	 (signal 'confidence::assertion-failure
		 :path (confidence:assertion-path condition)
		 :name (confidence:assertion-name condition)
		 :argument-names (confidence:assertion-argument-names condition)
		 :argument-values (confidence:assertion-argument-values condition)
		 :form (confidence:assertion-form condition)
		 :type (confidence:assertion-type condition))))))

;;;;
;;;; WITH-TESTCASE-OUTCOME
;;;;

(defmacro with-testcase-outcome (var form &body body)
  "Bind the testcase result of FORM to VAR and execute BODY.
Unlike the normale behaviour of a defined testcase, the testcase
is not propagated."
  `(let ((,var
	   (let ((*standard-output*
		   (make-string-output-stream))
		 (confidence::*testcase-path*
		   '(with-testcase-outcome))
		 (confidence::*testcase-describe-failed-assertions*
		   nil)
		 (confidence::*current-testcase-outcome*
		   (confidence::make-testcase-outcome
		    :path nil
		    :name '(with-testcase-outcome)))
		 (confidence:*testcase-interactive-p*
		   t))
	     ,form)))
     ,@body))


;;;;
;;;; PERFORM-MANY-ASSERTIONS
;;;;

(define-testcase perform-many-assertions (&key success failure condition)
  (when success
    (dotimes (_ success)
      (assert-t t)))
  (when failure
    (dotimes (_ failure)
      (assert-t nil)))
  (when condition
    (dotimes (_ condition)
      (assert-t (error "An error condition was signalled.")))))


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
