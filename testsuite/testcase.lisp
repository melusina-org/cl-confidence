;;;; testcase.lisp — Testcases for Confidence

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

(define-testcase a-successful-testsuite ()
  (assert-t t))

(define-testcase a-failing-argument-testsuite ()
  (assert-t nil)
  (assert-eq 0 (+ 1 1))
  (assert-t (error "An intentional error")))

(define-testcase a-failing-testcase-testsuite ()
  (error "An intentional error")
  (assert-t nil)
  (assert-eq 0 (+ 1 1)))

(define-testcase a-succesful-testsuite-with-function-calls ()
  (funcall 'a-successful-testsuite)
  (funcall #'a-successful-testsuite)
  (apply 'a-successful-testsuite nil)
  (apply #'a-successful-testsuite nil)
  (loop :for a :in '(1 2)
	:do (funcall #'a-successful-testsuite)))

(define-testcase validate-define-testcase ()
  (with-testcase-result testcase-result (validate-supervise-assertion)
    (assert-type testcase-result 'confidence:testcase-result)
    (assert-eq 2 (length (slot-value testcase-result 'confidence::results)))))

(define-testcase ensure-that-define-testcase-recognises-sharpsign-single-quote-in-function-names ()
  (with-testcase-result testcase-result (a-succesful-testsuite-with-function-calls)
    (assert-type testcase-result 'confidence:testcase-result)
    (assert-eq 6 (length (slot-value testcase-result 'confidence::results)))))

(define-testcase testsuite-testcase ()
  (validate-define-testcase)
  (ensure-that-define-testcase-recognises-sharpsign-single-quote-in-function-names)
  (validate-supervise-assertion))

;;;; End of file `testcase.lisp'
