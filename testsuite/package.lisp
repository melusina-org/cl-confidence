;;;; package.lisp — Package for Confidence tests

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.confidence/testsuite
  (:local-nicknames (#:confidence #:org.melusina.confidence))
  (:use #:common-lisp #:org.melusina.confidence)
  (:export
   #:list-available-tests
  ))

(in-package #:org.melusina.confidence/testsuite)

;;;; End of file `package.lisp'
