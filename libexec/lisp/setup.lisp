;;;; setup.lisp — Setup for Confidence Atelier Project

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

;;;
;;; Atelier
;;;

(ql:quickload "org.melusina.atelier" :silent t)

(org.melusina.atelier:initialise)

(setf org.melusina.atelier:*parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2019–2022")
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
;;;
;;; Confidence
;;;

(ql:quickload "org.melusina.confidence/testsuite")
(setf org.melusina.confidence:*testcase-interactive-p* t)

;;;; End of file `setup.lisp'
