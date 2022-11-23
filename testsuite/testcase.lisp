;;;; testcase.lisp — Testcases for Confidence

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

(defun make-actual-testcase-result ()
  (let ((confidence::*testcase-path*
	  nil)
	(confidence::*current-testcase-result*
	  nil)
	(confidence:*testcase-interactive-p* t))
    (funcall (intern "VALIDATE-SUPERVISE-ASSERTION"
		     (find-package "ORG.MELUSINA.CONFIDENCE/TESTSUITE")))))

(define-testcase validate-supervise-assertion ()
  ;; Mask the following assertion
  ;;  which is currently triggering a SIGILL (illegal CPU instruction)
  ;;  on some ARMs.
  #+nil
  (assert-type
   (confidence::supervise-assertion
    (assert-t t))
   'assertion-success)
  (assert-type
   (confidence::supervise-assertion
    (assert-t nil))
   'assertion-failure)
  (assert-type
   (confidence::supervise-assertion
    (assert-t (error "An intentional error condition")))
   'assertion-condition))

(define-testcase a-failing-argument-testsuite ()
  (assert-t nil)
  (assert-eq 0 (+ 1 1))
  (assert-t (error "An intentional error")))

(define-testcase a-failing-testcase-testsuite ()
  (error "An intentional error")
  (assert-t nil)
  (assert-eq 0 (+ 1 1)))

(define-testcase validate-define-testcase ()
  (let ((testcase-result
	  (make-actual-testcase-result)))
    (assert-eq 3 (length (slot-value testcase-result 'confidence::results)))))

(define-testcase testsuite-testcase ()
  (validate-supervise-assertion))

;;;; End of file `testcase.lisp'
