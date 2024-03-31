;;;; result.lisp — Result for Confidence

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

(define-testcase validate-outcome-can-be-described ()
  (let ((testcase-outcome
	  (confidence::make-testcase-outcome
	   :path '(some imaginative testcase path)
	   :name 'testcase-name
	   :argument-values '(1 :a 'b '(nil nil nil))
	   :total 100
	   :success 80
	   :failure 17
	   :condition 3)))
    (assert-type testcase-outcome 'confidence::testcase-outcome)
    (assert-string-match
     (with-output-to-string (buffer)
       (describe testcase-outcome buffer))
     "*Name: *")))

(define-testcase testsuite-result ()
  (validate-outcome-can-be-described))

;;;; End of file `result.lisp'
