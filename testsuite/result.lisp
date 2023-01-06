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

(defun make-assertion-success-example ()
  (make-instance 'confidence::assertion-success
		 :name 'confidence:assert-t
		 :argument-values '(t)
		 :argument-names '(expr)
		 :form '(confidence:assert-t t)))

(defun make-assertion-failure-example ()
  (make-instance 'confidence::assertion-failure
		 :name 'confidence:assert-t
		 :argument-values '(nil)
		 :argument-names nil
		 :form '(confidence:assert-t nil)
		 :description "The assertion (ASSERT-T EXPR) is true, iff EXPR is T."))

(defun make-assertion-condition-example ()
  (make-instance 'confidence::assertion-condition
		 :name 'confidence:assert-t
		 :argument-values nil
		 :argument-names nil
		 :form '(confidence:assert-t (error "Some error"))
		 :condition (make-instance 'simple-error :format-control "Some error")))

(defun make-testcase-result-example ()
  (make-instance 'confidence::testcase-result
		 :name 'make-testcase-result-example
		 :argument-values nil
		 :argument-names nil
		 :results (list
			   (make-assertion-success-example)
			   (make-assertion-failure-example)
			   (make-assertion-condition-example))))

(define-testcase validate-result ()
  (assert-eq 3 (slot-value (make-testcase-result-example) 'confidence::total)))

(define-testcase testsuite-result ()
  (validate-result))

;;;; End of file `result.lisp'
