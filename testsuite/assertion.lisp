;;;; assertion.lisp — Assertion Testsuite for Confidence

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

(define-condition expected-condition (simple-error)
  ((message :initarg :message)))

(define-testcase a-simple-failure ()
  (assert-t nil))

(define-testcase testsuite-define-assertion ()
  (assert-t (functionp #'assert-t)))

(define-testcase testsuite-basic-assertions ()
  (ensure-success (assert-t t))
  (ensure-failure (assert-t 1))
  (ensure-failure (assert-t nil))
  (ensure-success (assert-t* t))
  (ensure-success (assert-t* 1))
  (ensure-failure (assert-t* nil))
  (ensure-success (assert-nil nil))
  (ensure-failure (assert-nil 0))
  (ensure-success (assert-type t t))
  (ensure-success (assert-type 1 t))
  (ensure-success (assert-type 1 'integer))
  (ensure-failure (assert-type 1 'string)))

(define-testcase testsuite-assert-condition ()
  (ensure-success
   (assert-condition (error "A simple error") error))
  (ensure-success
   (assert-condition
       (error 'expected-condition :message "An expected message") expected-condition
       (message)
     (string= message "An expected message")))
  (ensure-failure
   (assert-condition (error "A simple error") nil)))

(define-testcase testsuite-assert-string* ()
  (ensure-failure
   (assert-string= t (string-upcase nil))
   "*The parameter STRING1 is expected to have type STRING but actually has type*")
  (ensure-failure
   (assert-string<= "AAB" "AAAA")
   "*Every character of STRING1 is less than or equal to the character of STRING2*")
  (ensure-failure
   (assert-string<= "AAB" "AAAA")
   "*upto index 2*"));, which are #?B and #?A*"))

(define-testcase testsuite-assert-list* ()
  (ensure-success
   (assert-list-equal '(0 1) '(0 1)))
  (ensure-failure
   (assert-list-equal '(0) '(0 1)))
  (ensure-success
   (assert-list-equal '("a" "b") '("a" "b") :test #'equal))
  (ensure-failure
   (assert-list-equal '("a" "b") '("a" "b"))))

(define-testcase testsuite-list-as-set ()
  (ensure-success
   (assert-subsetp '(a b) '(a b c)))
  (ensure-failure
   (assert-subsetp '(a b c) '(a b)))
  (ensure-success
   (assert-set-equal '(a b) '(a b)))
  (ensure-failure
   (assert-set-equal '(a b) '(a)))
  (ensure-success
   (assert-set-equal '((a . 1)(b . 2)) '((a . 3)(b . 4)) :key #'car)))

(define-testcase testsuite-assert-vector* ()
  (ensure-success
   (assert-vector-equal #(0 1) #(0 1)))
  (ensure-failure
   (assert-vector-equal #(0) #(0 1))))

(define-testcase testsuite-assert-float* ()
  (let ((*single-float-precision* 1))
    (ensure-success
     (assert-float-is-definitely-less-than 0.0 1.0))
    (ensure-success
     (assert-float-is-definitely-greater-than 1.0 0.0))
    (ensure-success
     (assert-float-is-approximately-equal 0.6 0.5))
    (ensure-success
     (assert-float-is-essentially-equal 0.35 0.4))))

(define-testcase testsuite-assertion ()
  (testsuite-basic-assertions)
  (testsuite-assert-condition)
  (testsuite-assert-string*)
  (testsuite-list-as-set)
  (testsuite-assert-vector*)
  (testsuite-assert-float*))

;;;; End of file `assertion.lisp'
