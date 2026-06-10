;;;; atelier.lisp — Confidence's operator-style declarations for Atelier

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; Loaded by the org.melusina.confidence/atelier ancillary system to
;;;; teach Atelier's formatter that DEFINE-ASSERTION and DEFINE-TESTCASE
;;;; are defun-shaped — the body indents two columns under the operator
;;;; rather than under the column of the first argument.

(in-package #:org.melusina.confidence)

(atelier:declare-operator-styles
 '((define-assertion :like cl:defun)
   (define-testcase :like cl:defun)))

;;;; End of file `atelier.lisp'
