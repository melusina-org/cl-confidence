;;;; entrypoint.lisp — Entrypoint for Confidence

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

(defun list-available-tests ()
  (format t "~&Available assertions:")
  (loop :for assertion :in (list-assertions "ORG.MELUSINA.CONFIDENCE")
	:do (format t "~& ~S" assertion))
  (format t "~&~%Available testcases:")
  (loop :for testcase :in (list-testcases "ORG.MELUSINA.CONFIDENCE/TESTSUITE")
	:do (format t "~& ~S" testcase))
  (format t "~&"))

(define-testcase run-all-tests ()
  "Run all available tests."
  (testsuite-utilities)
  (testsuite-result)
  (testsuite-assertion)
  (testsuite-testcase))

;;;; End of file `entrypoint.lisp'
