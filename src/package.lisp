;;;; package.lisp — Package for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.confidence
  (:use #:common-lisp)
  (:export
   #:quit
   ;; Results
   #:testcase-outcome
   #:testcase-path
   #:testcase-name
   #:testcase-total
   #:testcase-success
   #:testcase-failure
   #:testcase-condition
   #:testcase-argument-values
   ;; Testcases
   #:assertion-path
   #:assertion-name
   #:assertion-argument-names
   #:assertion-argument-values
   #:assertion-form
   #:assertion-type
   #:assertion-description
   #:assertion-condition
   #:define-testcase
   #:without-confidence
   #:*testcase-interactive-p*
   #:*testcase-describe-failed-assertions*
   #:*testsuite-name*
   #:*testsuite-id*
   #:*testsuite-last-result*
   #:*testcase-break-into-the-debugger-on-errors*
   #:testcase-break-into-the-debugger-on-errors
   #:list-testcases
   #:print-export-list-for-testcases
   ;; Assertions
   #:define-assertion
   #:list-assertions
   ;; Basic Assertions
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-type
   #:assert-eq
   #:assert-eql
   #:assert-equal
   #:assert-equalp
   #:assert=
   #:assert<
   #:assert<=
   #:assert>
   #:assert>=
   ;; Condition Assertions
   #:assert-condition
   ;; Character Assertions
   #:assert-char=
   #:assert-char-equal
   #:assert-char<
   #:assert-char<=
   #:assert-char>
   #:assert-char>=
   ;; String Assertions
   #:assert-string-match
   #:assert-string-equal
   #:assert-string=
   #:assert-string<
   #:assert-string<=
   #:assert-string>
   #:assert-string>=
   ;; List Assertions
   #:assert-list-equal
   #:assert-subsetp
   #:assert-set-equal
   ;; Vector Assertions
   #:assert-vector-equal
   ;; Floating Numbers Assertions
   #:*single-float-precision*
   #:*double-float-precision*
   #:assert-float-is-approximately-equal
   #:assert-float-is-definitely-greater-than
   #:assert-float-is-definitely-less-than
   #:assert-float-is-essentially-equal
  ))

(in-package #:org.melusina.confidence)

;;;; End of file `package.lisp'
