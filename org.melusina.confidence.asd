;;;; org.melusina.confidence.asd — System definition for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem org.melusina.confidence
  :description "A Simple Interactive Test Framework for Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT"
  :depends-on (:alexandria)
  :components
  ((:module "src"
      :components ((:file "package")
                   (:file "utilities")
		   (:file "assertion")
		   (:file "result")
		   (:file "testcase")
		   (:file "entrypoint")))))

(asdf:defsystem org.melusina.confidence/testsuite
  :description "A Simple Interactive Test Framework for Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT"
  :depends-on (:alexandria :org.melusina.confidence)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "assertion")
		 (:file "result")
		 (:file "testcase")
		 (:file "entrypoint")))))

;;;; End of file `org.melusina.confidence.asd'
