;;;; package.lisp — Package for Confidence tests

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.confidence/testsuite
  (:local-nicknames (#:confidence #:org.melusina.confidence))
  (:use #:common-lisp #:org.melusina.confidence)
  (:export
   #:a-compound-failing-testsuite
   #:a-failing-argument-testsuite
   #:a-failing-testcase-testsuite
   #:a-simple-failure
   #:a-successful-testsuite
   #:a-successful-testsuite-with-function-calls
   #:ensure-that-an-instrumented-assertion-returns-its-value
   #:ensure-that-define-testcase-recognises-sharpsign-single-quote-in-function-names
   #:ensure-that-testcase-is-reported-when-wrapped-in-flet
   #:interactive-assertion-count
   #:interactive-testcase-extensivity
   #:interactive-testcase-extensivity-1
   #:interactive-testcase-extensivity-2
   #:perform-many-assertions
   #:perform-many-assertions-wrapped-with-flet
   #:run-all-tests
   #:run-interactive-tests
   #:testsuite-assert-char*
   #:testsuite-assert-condition
   #:testsuite-assert-float*
   #:testsuite-assert-list*
   #:testsuite-assert-string*
   #:testsuite-assert-vector*
   #:testsuite-assertion
   #:testsuite-basic-assertions
   #:testsuite-define-assertion
   #:testsuite-list-as-set
   #:testsuite-result
   #:testsuite-string-match
   #:testsuite-testcase
   #:testsuite-utilities
   #:validate-define-testcase
   #:validate-outcome-can-be-described
   #:validate-supervise-assertion))

(in-package #:org.melusina.confidence/testsuite)

;;;; End of file `package.lisp'
