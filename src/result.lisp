;;;; result.lisp — Testcase Results for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.confidence)


;;;;
;;;; TESTCASE-OUTCOME
;;;;

(defclass testcase-outcome ()
  ((path
    :initarg :path
    :reader testcase-path
    :initform nil
    :documentation "The path of the result in the test hierarchy.
This is the stack of preceding testcases in the test hierarchy.")
   (name
    :initarg :name
    :reader testcase-name
    :initform (error "A TESTCASE-OUTCOME requires a :NAME."))
   (argument-values
    :initarg :argument-values
    :initform nil
    :reader testcase-argument-values
    :documentation
    "The list of evaluated arguments for the testcase.")
   (total
    :initarg :total
    :initform 0
    :reader testcase-total
    :documentation
    "The total number of assertions in the testcase and its descendants.")
   (success
    :initarg :success
    :initform 0
    :reader testcase-success
    :documentation
    "The total number of assertions that yielded a success in the testcase and its descendants.")
   (failure
    :initarg :failure
    :initform 0
    :reader testcase-failure
    :documentation
    "The total number of assertions that yielded a failure in the testcase and its descendants.")
   (condition
    :initarg :condition
    :initform 0
    :reader testcase-condition
    :documentation
    "The total number of assertions that yielded a condition in the testcase and its descendants."))
  (:documentation
   "A class capturing a testcase outcome."))

(defmethod print-object ((instance testcase-outcome) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream ":NAME ~S" (if (slot-boundp instance 'name)
			 (slot-value instance 'name)
			 "(no name)"))
    (loop :for slot :in '(total success failure condition)
	  :do (format stream " :~A ~A" (symbol-name slot) (slot-value instance slot)))))

(defmethod describe-object ((instance testcase-outcome) stream)
  (flet ((describe-path (path)
	   (when path
	     (format stream "~&Path:~%")
	     (loop :for path-element :in (reverse path)
		   :do (format stream "~S~%" path-element))))
	 (describe-result (&key total success failure condition)
	   (format stream "~&Total: ~D" total)
	   (when (> total 0)
	     (flet ((fraction (n)
		      (round (/ (* 100 n) total))))
	       (format stream "~&Success: ~D/~D (~D%)"
		       success total (fraction success))
	       (format stream "~&Failure: ~D/~D (~D%)"
		       failure total (fraction failure))
	       (format stream "~&Condition: ~D/~D (~D%)"
		       condition total (fraction condition)))
	     (if (< success total)
		 (format stream "~&Outcome: Failure")
		 (format stream "~&Outcome: Success")))))
  (with-slots (name path argument-values total success failure condition) instance
    (format stream "~&Name: ~S" name)
    (describe-path path)
    (cond
      ((> (length argument-values) 1)
       (format stream "~&Arguments:")
       (loop :for argument :in argument-values
	     :for i = 1 :then (1+ i)
	     :do (format stream "~& Argument #~A: ~S" i argument)))
      ((= (length argument-values) 1)
       (format stream "~&Argument: ~S" (first argument-values))))
    (describe-result :total total :success success :failure failure :condition condition))))

(defun make-testcase-outcome (&rest initargs &key path name argument-values total success failure condition)
  (declare (ignore path name argument-values total success failure condition))
  (apply #'make-instance 'testcase-outcome initargs))


;;;
;;; Current Testcase Outcome
;;;

(defparameter *current-testcase-outcome* nil
  "The outcome of the current testcase.")

(defvar *last-testsuite-outcome* nil
  "The outcome of the last testsuite.")

;;;; End of file `result.lisp'
