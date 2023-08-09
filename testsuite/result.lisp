;;;; result.lisp — Result for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.confidence/testsuite)

(defun make-some-assertion-success ()
  (make-instance 'confidence::assertion-success
		 :name 'confidence:assert-t
		 :argument-values '(t)
		 :argument-names '(expr)
		 :form '(confidence:assert-t t)))

(defun make-some-assertion-failure ()
  (make-instance 'confidence::assertion-failure
		 :name 'confidence:assert-t
		 :argument-values '(nil)
		 :argument-names nil
		 :form '(confidence:assert-t nil)
		 :description "The assertion (ASSERT-T EXPR) is true, iff EXPR is T."))

(defun make-some-assertion-failure-with-keyword-argument ()
  (confidence::supervise-assertion
   (confidence:assert-float-is-essentially-equal 1.0 2.0 :inaccuracy 1)))

(defun make-some-assertion-condition ()
  (make-instance 'confidence::assertion-condition
		 :name 'confidence:assert-t
		 :argument-values nil
		 :argument-names nil
		 :form '(confidence:assert-t (error "Some error"))
		 :condition (make-instance 'simple-error :format-control "Some error")))

(defun make-some-testcase-result ()
  (make-instance 'confidence::testcase-result
		 :name 'make-some-testcase-result
		 :argument-values nil
		 :argument-names nil
		 :results (list
			   (make-some-assertion-success)
			   (make-some-assertion-failure)
			   (make-some-assertion-condition))))

(define-testcase validate-result-can-be-described ()
  (loop :for make-some-result
	:in '(make-some-assertion-success
	      make-some-assertion-failure
	      make-some-assertion-failure-with-keyword-argument
	      make-some-assertion-condition
	      make-some-testcase-result)
	:do
	(assert-string-match
	 (with-output-to-string (buffer)
	   (describe (funcall make-some-result) buffer))
	 "*Name: *")))

(define-testcase validate-result-total ()
  (assert-eq 3 (slot-value (make-some-testcase-result) 'confidence::total)))

(define-testcase testsuite-result ()
  (validate-result-can-be-described)
  (validate-result-total))

;;;; End of file `result.lisp'
