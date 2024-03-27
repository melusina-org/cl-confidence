;;;; user.lisp — System for Confidence Users

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.confidence/user
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier)
   (#:confidence #:org.melusina.confidence)
   (#:testsuite #:org.melusina.confidence/testsuite)
   (#:development #:org.melusina.confidence/development)))

(in-package #:org.melusina.confidence/user)

;;;; End of file `user.lisp'
