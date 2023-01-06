;;;; result.lisp — Testcase Results for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.confidence)


;;;
;;; RESULT
;;;

(defclass result nil
  ((path
    :initarg :path
    :documentation "The path of the result in the test hierarchy.
This is the stack of preceding testcases in the test hierarchy."))
  (:documentation "The abstract class of testcase results."))


;;;
;;; ASSERTION-RESULT
;;;

(defclass assertion-result (result)
  ((type
    :initarg :type
    :initform :function
    :documentation
    "One of the keywords :FUNCTION or :MACRO according to the nature of the assertion.")
   (name
    :initarg :name
    :initform (error "An ASSERTION-RESULT requires a :NAME.")
    :documentation
    "The symbol designating the assertion that yielded this result.
This is the first element of the FORM.")
   (argument-values
    :initarg :argument-values
    :initform (error "An ASSERTION-RESULT requires an :ARGUMENT-VALUES list.")
    :documentation
    "The list of evaluated arguments for the assertion.")
   (argument-names
    :initarg :argument-names
    :initform (error "An ASSERTION-RESULT requires an :ARGUMENT-NAMES list.")
    :documentation
    "The list of argument names for the assertion.")
   (form
    :initarg :form
    :initform (error "An ASSERTION-RESULT requires a :FORM.")
    :documentation
    "The form for the assertion invocation."))
  (:documentation
   "A class capturing an assertion result."))

(defmethod describe-object ((instance assertion-result) stream)
  (format stream "~&~A is an assertion result of type ~A." instance (type-of instance))
  (with-slots (path type name argument-values form) instance
    (format stream "~&Type: ~S" type)
    (format stream "~&Name: ~S" name)
    (when path
      (format stream "~&Path:~%")
      (loop :for path-element :in (reverse path)
	    :for path-level :from 1
	    :do (dotimes (_ (* 2 path-level)) (write-char #\Space stream))
	    :do (format stream "~S~%" path-element)))
    (cond
      ((> (length argument-values) 1)
       (format stream "~&Arguments:")
       (loop :for argument :in argument-values
	     :for i = 1 :then (1+ i)
	     :do (format stream "~& Argument #~A: ~S" i argument)))
      ((= (length argument-values) 1)
       (format stream "~&Argument: ~S" (first argument-values))))
    (format stream "~&Form: ~S" form))
  (values))


;;;
;;; ASSERTION-SUCCESS
;;;

(defclass assertion-success (assertion-result)
  nil
  (:documentation
   "A class capturing an assertion success."))

(defmethod describe-object :after ((instance assertion-success) stream)
  (declare (ignore instance))
  (format stream "~&Outcome: Success")
  (values))


;;;
;;; ASSERTION-FAILURE
;;;

(defclass assertion-failure (assertion-result)
  ((description
    :initarg :description
    :initform (error "An ASSERTION-FAILURE requires a :DESCRIPTION.")
    :documentation
    "A detailed description on why the assertion failed."))
  (:documentation
   "A class capturing an assertion failure."))

(defun describe-object-arguments (instance stream)
  (labels
      ((composed-argument-p (form)
	 (not (atom form)))
       (format-atom-argument (name value)
	 (format stream "~&  ~A: ~S~%~%" (symbol-name name) value))
       (format-condition-argument (name form value)
	 (format stream "~&  ~A: ~S => CONDITION" (symbol-name name) form)
	 (format stream "~&    The evaluation of this form yielded a condition~%~%")
	 (describe value stream))
       (format-composed-argument (name form value)
	 (format stream "~&  ~A: ~S => ~S~%~%" (symbol-name name) form value))
       (format-argument (name form value)
	 (cond
	   ((typep value 'condition)
	    (format-condition-argument name form value))
	   ((composed-argument-p form)
	    (format-composed-argument name form value))
	   ((atom form)
	    (format-atom-argument name value))
	   (t
	    (error "Cannot describe argument ~S => ~S" form value)))))
    (with-slots (name form argument-values argument-names) instance
      (when argument-names
	(format stream
		"~&  In this call, forms in argument position evaluate as:~%~%"))
      (loop :for name :in argument-names
	    :for form :in (rest form)
	    :for value :in argument-values
	    :do (format-argument name form value)))))  

(defmethod describe-object :after ((instance assertion-failure) stream)
  (format stream "~&Outcome: Failure")
  (with-slots (description) instance
    (format stream "~&Description: ~A" description)
    (describe-object-arguments instance stream))
  (values))


;;;
;;; ASSERTION-CONDITION
;;;

(defclass assertion-condition (assertion-result)
  ((condition
    :initarg :condition
    :initform (error "An ASSERTION-CONDITION requires a :CONDITION.")))
  (:documentation
   "A class capturing an assertion that signaled a condition instead
of returning normally."))

(defmethod describe-object :after ((instance assertion-condition) stream)
  (format stream "~&Outcome: Condition")
  (with-slots (condition argument-values) instance
    (if (member-if (lambda (argument-value)  (typep argument-value 'condition)) argument-values)
	(progn
	  (format stream "~&Description:")	  )
	(progn
	  (format stream "~&Condition: ~S" condition)
	  (describe condition stream)))
    (describe-object-arguments instance stream))
  (values))



;;;
;;; TESTCASE-RESULT
;;;

(defclass testcase-result (result)
  ((name
    :initarg :name
    :initform (error "A TESTCASE-RESULT requires a :NAME."))
   (argument-values
    :initarg :argument-values
    :initform nil
    :documentation
    "The list of evaluated arguments for the testcase.")
   (total
    :initform 0
    :documentation
    "The total number of assertions in the testcase and its descendants.")
   (success
    :initform 0
    :documentation
    "The total number of assertions that yielded a success in the testcase and its descendants.")
   (failure
    :initform 0
    :documentation
    "The total number of assertions that yielded a failure in the testcase and its descendants.")
   (condition
    :initform 0
    :documentation
    "The total number of assertions that yielded a condition in the testcase and its descendants.")
   (results
    :initarg :results
    :initform (error "A TESTCASE-RESULT requires a list of :RESULTS.")
    :documentation
    "The list of testcase results and assertions results yielded by descendants."))
  (:documentation
   "A class capturing a testcase result."))

(defmethod initialize-instance :after ((instance testcase-result) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (results total success failure condition) instance
    (loop :for result :in results
	  :do (etypecase result
		(assertion-success
		 (incf total)
		 (incf success))
		(assertion-failure
		 (incf total)
		 (incf failure))
		(assertion-condition
		 (incf total)
		 (incf condition))
		(testcase-result
		 (incf total (slot-value result 'total))
		 (incf success (slot-value result 'success))
		 (incf failure (slot-value result 'failure))
		 (incf condition (slot-value result 'condition)))))))

(defmethod print-object ((instance testcase-result) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream ":NAME ~S" (if (slot-boundp instance 'name)
			 (slot-value instance 'name)
			 "(no name)"))
    (loop :for slot :in '(total success failure condition)
	  :do (format stream " :~S ~A" slot (slot-value instance slot)))))

(defmethod describe-object ((instance testcase-result) stream)
  (with-slots (name argument-values total success failure condition results) instance
    (format stream "~&Name: ~S" name)
    (cond
      ((> (length argument-values) 1)
       (format stream "~&Arguments:")
       (loop :for argument :in argument-values
	     :for i = 1 :then (1+ i)
	     :do (format stream "~& Argument #~A: ~S" i argument)))
      ((= (length argument-values) 1)
       (format stream "~&Argument: ~S" (first argument-values))))
    (format stream "~&Total: ~D" total)
    (when (> total 0)
      (format stream "~&Success: ~D/~D (~D%)"
	      success total (round (/ (* 100 success) total)))
      (format stream "~&Failure: ~D/~D (~D%)"
	      failure total (round (/ (* 100 failure) total)))
      (format stream "~&Condition: ~D/~D (~D%)"
	      condition total (round (/ (* 100 condition) total))))
    (if (< success total)
	(format stream "~&Outcome: Failure")
	(format stream "~&Outcome: Success"))
    (when (< success total)
      (let ((description-separator
	      (make-string 80 :initial-element #\=)))
	(loop :for result :in results
	      :when (or (typep result 'assertion-failure)
			(typep result 'assertion-condition)
			(and (typep result 'testcase-result)
			     (< (slot-value result 'success)
				(slot-value result 'total))))
	      :do (progn
		    (format stream "~&~A~&" description-separator)
		    (describe result stream)))))
    (values)))



;;;
;;; Current Testcase Result
;;;

(defparameter *current-testcase-result* nil
  "The result of the current testcase.")

(defvar *last-testsuite-result* nil
  "The result of the last testsuite.")

(defgeneric record-result (new-result testcase-result)
  (:method ((new-result result) (accumulator testcase-result))
    (with-slots (results total) accumulator
      (setf results (nconc results (list new-result)))))
  (:method ((new-result assertion-result) (accumulator testcase-result))
    (declare (ignore new-result))
    (incf (slot-value accumulator 'total))
    (call-next-method))
  (:method ((new-result assertion-success) (accumulator testcase-result))
    (declare (ignore new-result))
    (incf (slot-value accumulator 'success))
    (call-next-method))
  (:method ((new-result assertion-failure) (accumulator testcase-result))
    (declare (ignore new-result))
    (incf (slot-value accumulator 'failure))
    (call-next-method))
  (:method ((new-result assertion-condition) (accumulator testcase-result))
    (declare (ignore new-result))
    (incf (slot-value accumulator 'condition))
    (call-next-method))
  (:method ((new-result testcase-result) (accumulator testcase-result))
    (dolist (slot '(success failure condition total))
      (incf (slot-value accumulator slot)
	    (slot-value new-result slot)))
    (call-next-method)))

(defun record-testcase-result (result &optional (testcase-result *current-testcase-result*))
  "Record RESULT in TESTCASE-RESULT."
  (unless testcase-result
    (return-from record-testcase-result result))
  (record-result result testcase-result))

;;;; End of file `result.lisp'
