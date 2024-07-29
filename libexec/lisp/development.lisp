;;;; development.lisp — Project Development for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:confidence/development
  (:use #:common-lisp)
  (:export
   #:lint
   #+quicklisp
   #:reload))

(in-package #:confidence/development)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defun system-relative-pathnames (&rest pathnames)
  (mapcar #'system-relative-pathname pathnames))

(defparameter *parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2019–2024")
	(:project-filename . "org.melusina.confidence")
        (:project-name . "Confidence")
	(:project-description . "A Simple Interactive Test Framework for Common Lisp")
	(:project-long-description .
	 #.(concatenate 'string
	    "Confidence is a test framework for Common Lisp that focuses on simplicty. "
	    "It avoids bureacracy and makes it easy to work interactively, without "
	    "a complicated setup, and with explicit functions and decisions."))
        (:homepage . "https://github.com/melusina-org/cl-confidence")
        (:license . :mit)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     (system-relative-pathnames
      #p"org.melusina.confidence.asd"
      #p"doc"
      #p"example"
      #p"src"
      #p"subr"
      #p"testsuite"
      #p"libexec/lisp/development.lisp"
      #p"libexec/lisp/user.lisp"))))

#+quicklisp
(defun reload ()
  (ql:quickload '("org.melusina.atelier"
		  "org.melusina.confidence"
		  "org.melusina.confidence/development")))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.confidence/development:reload)

;;;; End of file `development.lisp'
