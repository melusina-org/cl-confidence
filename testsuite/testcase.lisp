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
   '%assertion-success)
  #+nil
  (assert-type
   (confidence::supervise-assertion
    (assert-t nil))
   '%assertion-failure)
  (ensure-condition
      (confidence::supervise-assertion
       (assert-t (error "An intentional error condition")))
      'simple-error))

(define-testcase a-successful-testsuite ()
  (assert-t t))

(define-testcase a-failing-argument-testsuite ()
  (assert-t t)
  (assert-eq 0 0)
  (assert-t (error "An intentional error occuring when evaluating assertion arguments.")))

(define-testcase a-failing-testcase-testsuite ()
  (assert-t t)
  (assert-eq 0 0)
  (error "An intentional error occuring in the testsuite."))

(define-testcase a-compound-failing-testsuite ()
  (a-successful-testsuite)
  (a-successful-testsuite)
  (a-failing-argument-testsuite)
  (a-failing-argument-testsuite)
  (a-failing-testcase-testsuite))

(define-testcase a-successful-testsuite-with-function-calls ()
  (funcall 'a-successful-testsuite)
  (funcall #'a-successful-testsuite)
  (apply 'a-successful-testsuite nil)
  (apply #'a-successful-testsuite nil)
  (dotimes (_ 2)
    (funcall #'a-successful-testsuite)))

(define-testcase validate-define-testcase ()
  (with-testcase-outcome testcase-outcome (validate-supervise-assertion)
    (assert-type testcase-outcome 'confidence:testcase-outcome)
    (assert-eq 1 (confidence:testcase-total testcase-outcome))
    (assert-eq (confidence:testcase-total testcase-outcome) (confidence:testcase-success testcase-outcome))
    (assert-eq (confidence:testcase-total testcase-outcome)
	       (+ (confidence:testcase-success testcase-outcome)
		  (confidence:testcase-failure testcase-outcome)
		  (confidence:testcase-condition testcase-outcome)))))

(define-testcase ensure-that-define-testcase-recognises-sharpsign-single-quote-in-function-names ()
  (with-testcase-outcome testcase-outcome (a-successful-testsuite-with-function-calls)
    (assert-type testcase-outcome 'confidence:testcase-outcome)
    (assert-eq 6 (confidence:testcase-total testcase-outcome))
    (assert-eq (confidence:testcase-total testcase-outcome) (confidence:testcase-success testcase-outcome))
    (assert-eq (confidence:testcase-total testcase-outcome)
	       (+ (confidence:testcase-success testcase-outcome)
		  (confidence:testcase-failure testcase-outcome)
		  (confidence:testcase-condition testcase-outcome)))))

(define-testcase perform-many-assertions-wrapped-with-flet (&key success failure condition)
  (flet ((wrapper ()
	   (perform-many-assertions 
	    :success success
	    :failure failure
	    :condition condition)))
    (wrapper)))

(define-testcase ensure-that-testcase-is-reported-when-wrapped-in-flet ()
  (with-testcase-outcome testcase-outcome
      (perform-many-assertions-wrapped-with-flet :success 100 :failure 10 :condition 1)
    (assert-type testcase-outcome 'confidence:testcase-outcome)
    (assert-eq 111 (confidence:testcase-total testcase-outcome))
    (assert-eq 100 (confidence:testcase-success testcase-outcome))
    (assert-eq 10 (confidence:testcase-failure testcase-outcome))
    (assert-eq 1 (confidence:testcase-condition testcase-outcome))))

(define-testcase testsuite-testcase ()
  (validate-define-testcase)
  (ensure-that-define-testcase-recognises-sharpsign-single-quote-in-function-names)
  (ensure-that-testcase-is-reported-when-wrapped-in-flet)
  (validate-supervise-assertion))

;;;; End of file `testcase.lisp'
