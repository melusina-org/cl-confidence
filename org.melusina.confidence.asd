;;;; org.melusina.confidence.asd — System definition for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
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

(asdf:defsystem #:org.melusina.confidence/development
  :description "Development tools for Confidence"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

(asdf:defsystem #:org.melusina.confidence/user
  :description "System for Confidence Users"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.confidence
	       #:org.melusina.confidence/testsuite
	       #:org.melusina.confidence/development)
  :components
  ((:module "libexec"
    :components
    ((:module "lisp"
      :components
      ((:file "user")))))))

;;;; End of file `org.melusina.confidence.asd'
