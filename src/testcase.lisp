;;;; testcase.lisp — Testcases for Confidence

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

(defparameter *testcase-interactive-p*
  (let ((is-likely-to-run-in-a-slime-session
	  (member :swank *features*))
	(is-likely-to-run-in-a-sly-session
	  (member :slynk *features*)))
    (flet ((ensure-boolean (generalised-boolean)
	     (and generalised-boolean t)))
      (ensure-boolean
       (or is-likely-to-run-in-a-slime-session
	   is-likely-to-run-in-a-sly-session))))
  "Flag governing the interactive mode of testcases.
When the flag is a generalised boolean, a failed assertion can be retried.

The default value of the parameter is based on the :SWANK and :SLYNK features.")

(defparameter *testcase-describe-failed-assertions* t
  "Flag controlling whether a testcase should describe failed assertions or not.")

(defparameter *testcase-path* nil
  "The current path in the testcase hierarchy.
This is a list of symbols designating the argument-less testcases in the call stack.")

(defparameter *testsuite-name* "TESTSUITE"
  "The basename for the testsuite.

Usually TESTSUITE but commonly used values are ACCEPTANCE, INTEGRATION, PREFLIGHT, etc.")

(defparameter *testsuite-id* nil
  "A unique identfier for the current testsuite run batch.")

(defparameter *last-testsuite-result* nil
  "The results of the last testsuite that ran.")


;;;;
;;;; TESTSUITE-IDENTIFICATION
;;;;

(defun guess-cicdtool ()
  "Guess which CI/CD tool this process is running under.

The returned value is one of:

  NIL, :GOCD, :JENKINS, :GITHUB-ACTIONS, :CIRCLECI

References:

 * GoCD: https://docs.gocd.org/current/faq/dev_use_current_revision_in_build.html
 * Jenkins: https://www.jenkins.io/doc/book/pipeline/jenkinsfile/#using-environment-variables
 * GitHub Actions: https://docs.github.com/en/actions/learn-github-actions/environment-variables
 * CircleCI: https://circleci.com/docs/2.0/env-vars/"
  (cond
    ((uiop:getenv "GO_SERVER_URL")
     :gocd)
    ((uiop:getenv "JENKINS_URL")
     :jenkins)
    ((uiop:getenv "GITHUB_ACTIONS")
     :github-actions)
    ((uiop:getenv "CIRCLECI")
     :circleci)
    (t
     nil)))

(defun make-testsuite-id ()
  "Make a good value for *TESTSUITE-ID* based on *TESTSUITE-NAME* and CI/CD tool used."
  (let ((designator
	  (case (guess-cicdtool)
	    (:gocd
	     (uiop:getenv "GO_PIPELINE_LABEL"))
	    (:jenkins
	     (uiop:getenv "BUILD_TAG"))
	    (:github-actions
	     (uiop:getenv "GITHUB_RUN_NUMBER"))
	    (:circleci
	     (uiop:getenv "CIRCLE_WORKFLOW_ID"))
	    (t
	     (random-string 7 :base36)))))
    (concatenate 'string *testsuite-name* designator)))


;;;;
;;;; ASSERTION-OUTCOME
;;;;

(define-condition assertion-outcome ()
  ((path
    :initarg :path
    :initform nil
    :reader assertion-path
    :documentation "The path of the result in the test hierarchy.
This is the stack of preceding testcases in the test hierarchy.")
   (type
    :initarg :type
    :initform :function
    :reader assertion-type
    :documentation
    "One of the keywords :FUNCTION or :MACRO according to the nature of the assertion.")
   (name
    :initarg :name
    :initform (error "An ASSERTION-OUTCOME requires a :NAME.")
    :reader assertion-name
    :documentation
    "The symbol designating the assertion that yielded this result.
This is the first element of the FORM.")
   (argument-values
    :initarg :argument-values
    :initform (error "An ASSERTION-OUTCOME requires an :ARGUMENT-VALUES list.")
    :reader assertion-argument-values
    :documentation
    "The list of evaluated arguments for the assertion.")
   (argument-names
    :initarg :argument-names
    :initform (error "An ASSERTION-OUTCOME requires an :ARGUMENT-NAMES list.")
    :reader assertion-argument-names
    :documentation
    "The list of argument names for the assertion.")
   (form
    :initarg :form
    :initform (error "An ASSERTION-OUTCOME requires a :FORM.")
    :reader assertion-form
    :documentation
    "The form for the assertion invocation."))
  (:report describe-assertion-outcome)
  (:documentation
   "The condition signalling an assertion result."))

(defun describe-assertion-outcome (condition stream)
  (flet ((describe-path (path)
	   (when path
	     (format stream "~&Path:~%")
	     (loop :for path-element :in (reverse path)
		   :do (format stream "~S~%" path-element))))
	 (describe-argument-values (argument-values)
	   (cond
	     ((> (length argument-values) 1)
	      (format stream "~&Arguments:")
	      (loop :for argument :in argument-values
		    :for i = 1 :then (1+ i)
		    :do (format stream "~& Argument #~A: ~S" i argument)))
	     ((= (length argument-values) 1)
	      (format stream "~&Argument: ~S" (first argument-values))))))
    (format stream "~&Type: ~S" (assertion-type condition))
    (format stream "~&Name: ~S" (assertion-name condition))
    (describe-path (assertion-path condition))	 
    (describe-argument-values (assertion-argument-values condition))
    (format stream "~&Form: ~S" (assertion-form condition))
    (values)))


;;;;
;;;; ASSERTION-SUCCESS
;;;;

(define-condition assertion-success (assertion-outcome)
  nil
  (:report describe-assertion-success)
  (:documentation "The condition signaled by succesful assertions."))

(defun describe-assertion-success (condition stream)
  (describe-assertion-outcome condition stream)
  (format stream "~&Outcome: Success")
  (values))


;;;;
;;;; ASSERTION-FAILURE
;;;;

(define-condition assertion-failure (assertion-outcome)
  ((description
    :initarg :description
    :initform (error "An ASSERTION-FAILURE requires a :DESCRIPTION.")
    :reader assertion-description
    :documentation
    "A detailed description on why the assertion failed."))
  (:report describe-assertion-failure)
  (:documentation "The condition signaled by failed assertions."))

(defun describe-object-arguments (condition stream)
  (labels
      ((symbol-prefix (name)
	 (if (keywordp name) ":" ""))
       (composed-argument-p (form)
	 (not (atom form)))
       (format-atom-argument (name value)
	 (format stream "~&  ~A~A: ~S~%~%"
		 (symbol-prefix name) (symbol-name name) value))
       (format-condition-argument (name form value)
	 (format stream "~&  ~A~A: ~S => CONDITION"
		 (symbol-prefix name) (symbol-name name) form)
	 (format stream "~&    The evaluation of this form yielded a condition~%~%")
	 (describe value stream))
       (format-composed-argument (name form value)
	 (format stream "~&  ~A~A: ~S => ~S~%~%"
		 (symbol-prefix name) (symbol-name name) form value))
       (format-argument (name form value)
	 (cond
	   ((typep value 'condition)
	    (format-condition-argument name form value))
	   ((composed-argument-p form)
	    (format-composed-argument name form value))
	   ((atom form)
	    (format-atom-argument name value))))
       (format-positional-arguments (names forms values)
	 (loop :for name :in names
	       :for form :in forms
	       :for value :in values
	       :when (eq name '&key)
	       :return nil
	       :do (format-argument name form value)))
       (format-key-arguments (names forms values)
	 (let ((key-index (position '&key names)))
	   (unless key-index
	     (return-from format-key-arguments))
	   (loop :for (form-name form) :on (subseq forms key-index) :by #'cddr
		 :for (value-name value) :on (subseq values key-index) :by #'cddr
		 :for name = (if (eq form-name value-name)
			       form-name
			       (error "Value and form name differ."))
		 :do (format-argument name form value)))))
    (let ((form
	    (assertion-form condition))
	  (argument-values
	    (assertion-argument-values condition))
	  (argument-names
	    (assertion-argument-names condition)))
      (when argument-names
	(format stream
		"~&  In this call, forms in argument position evaluate as:~%~%"))
      (format-positional-arguments argument-names (rest form) argument-values)
      (format-key-arguments argument-names (rest form) argument-values))))

(defun describe-assertion-failure (condition stream)
  (describe-assertion-outcome condition stream)
  (format stream "~&Outcome: Failure")
  (format stream "~&Description: ~A" (assertion-description condition))
  (describe-object-arguments condition stream)
  (values))


;;;
;;; ASSERTION-CONDITION
;;;

(define-condition assertion-condition (assertion-outcome)
  ((condition
    :initarg :condition
    :initform (error "An ASSERTION-CONDITION requires a :CONDITION.")
    :reader assertion-condition
    :documentation "The condition signalled by the assertion."))
  (:report describe-assertion-condition)
  (:documentation "The condition signaled by assertions that trigger an unexpected condition
instead of returning normally."))

(defun describe-assertion-condition (condition stream)
  (describe-assertion-outcome condition stream)
  (format stream "~&Outcome: Condition")
  (let ((argument-values
	  (assertion-argument-values condition))
	(condition
	  (assertion-condition condition)))
    (if (member-if (lambda (argument-value) (typep argument-value 'condition)) argument-values)
	(progn
	  (format stream "~&Description:")	  )
	(progn
	  (format stream "~&Condition: ~S" condition)
	  (describe condition stream))))
  (describe-object-arguments condition stream)
  (values))

(define-condition assertion-skipped nil nil
  (:documentation "The condition signaled by assertions that are skipped."))


;;;;
;;;; TESTCASE-END
;;;;

(define-condition testcase-end ()
  ((outcome
    :initarg :outcome
    :initform (error "A TESTCASE-END requires an outcome.")
    :reader testcase-outcome
    :documentation
    "The outcome of the finished testcase."))
  (:report describe-testcase-end)
  (:documentation
   "The condition signalled at the end of a testcase."))

(defun describe-testcase-end (condition stream)
  (describe-object (testcase-outcome condition) stream))


;;;
;;; SUPERVISE-ASSERTION
;;;

(defun supervise-assertion-1 (&key name form type argument-names argument-lambdas assertion-lambda)
  "Supervise the execution of ARGUMENTS-LAMBDA and ASSERTION-LAMBDA."
  (labels ((evaluation-strategy-1 (argument-condition argument-values)
	     (when argument-condition
	       (signal
		'assertion-condition
		:path *testcase-path*
		:name name
		:argument-names argument-names
		:argument-values argument-values
		:form form
		:type type
		:condition (first argument-condition))
	       (return-from evaluation-strategy-1))
	     (multiple-value-bind (success-p description)
		 (handler-case (funcall assertion-lambda argument-values)
	     	   (serious-condition (unexpected-condition)
		     (signal
		      'assertion-condition
		      :path *testcase-path*
		      :name name
		      :argument-names argument-names
		      :argument-values argument-values
		      :form form
		      :type type
		      :condition unexpected-condition)
		     (return-from evaluation-strategy-1)))
	       (cond
		 ((not success-p)
		  (signal 'assertion-failure
			  :path *testcase-path*
			  :name name
			  :argument-names argument-names
			  :argument-values argument-values
			  :form form
			  :type type
			  :description description))
		 (t
		  (signal 'assertion-success
			  :path *testcase-path*
			  :name name
			  :argument-names argument-names
			  :argument-values argument-values
			  :form form
			  :type type)))))
	   (evaluation-strategy (argument-values)
	     (evaluation-strategy-1
	      (member-if (lambda (object) (typep object 'condition)) argument-values)
	      argument-values))
	   (supervise-evaluation-1 (argument-lambda)
	     (handler-case (funcall argument-lambda)
	       (t (unexpected-condition)
		 unexpected-condition)))
	   (supervise-evaluation (argument-lambdas)
	     (evaluation-strategy (mapcar #'supervise-evaluation-1 argument-lambdas))))
    (supervise-evaluation argument-lambdas)))

(defmacro supervise-assertion (form)
  "Supervise the execution of the assertion FORM and return ASSERTION evaluation details.
This makes sure that the returned type for FORM is an instance of RESULT and
guarantees that conditions triggered by the evaluation of arguments are recorded."
  (let* ((name
	   (first form))
	 (type
	   (if (macro-function (first form))
	       :macro
	       :function))
	 (argument-lambdas
	   (when (eq type :function)
	     (cons 'list
		   (loop :for argument-form :in (rest form)
			 :collect `(lambda () ,argument-form)))))
	 (argument-names
	   (get name :org.melusina.confidence/lambda-list))
	 (assertion-lambda
	   (ecase type
	     (:macro
	      `(lambda (argv) (declare (ignore argv)) ,form))
	     (:function
	      `(lambda (argv) (apply (function ,name) argv))))))
    `(supervise-assertion-1
      :name (quote ,name)
      :form (quote ,form)
      :type ,type
      :argument-names (quote ,argument-names)
      :argument-lambdas ,argument-lambdas
      :assertion-lambda ,assertion-lambda)))


;;;
;;; DEFINE-TESTCASE
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun set-testcase-properties (name)
    (setf (get name :org.melusina.confidence/testcase) t)))

(defun define-testcase/wrap-assertion-forms (body-forms)
  "Walks through BODY-FORMS and wrap assertion forms in a RESTART-CASE."
  (labels
      ((function-name (form)
	 (cond
	   ((symbolp form)
	    form)
	   ((and (listp form) (eq (first form) 'quote))
	    (second form))
	   ((and (listp form) (eq (first form) 'function))
	    (second form))))
       (is-funcall-p (form)
	 "Predicate recognising forms which are function calls.
When the form is a function call, the returned value is the
symbol of this function."
         (when (and (listp form) (not (null form)) (symbolp (first form)) (listp (rest form)))
           (case (first form)
             ((funcall apply)
              (function-name (second form)))
             (t (first form)))))
       (is-assert-name-p (symbol)
         (and
	  (symbolp symbol)
	  (>= (length (symbol-name symbol)) 7)
          (and (string= (symbol-name symbol) "ASSERT" :end1 6)
               (position (char (symbol-name symbol) 6) "-=<>"))
	  symbol))
       (is-assert-form-p (form)
         (get (is-assert-name-p (is-funcall-p form))
	      :org.melusina.confidence/assertion))
       (wrap-assertion-forms (form)
         (cond
	   ((eq 'without-confidence (is-funcall-p form))
	    `(progn ,@(rest form)))
	   ((is-assert-form-p form)
            `(supervise-assertion ,form))
           ((is-funcall-p form)
            (cons (first form) (mapcar #'wrap-assertion-forms (rest form))))
           (t
	    form))))
    (mapcar #'wrap-assertion-forms body-forms)))

(defun testcase-outcome-pathname (outcome)
  "The pathname used to write OUTCOME description."
  (merge-pathnames
   (make-pathname
    :name (string-downcase (symbol-name (slot-value outcome 'name)))
    :type "log")
   #p"obj/confidence/"))

(defun export-testcase-outcome (outcome)
  "Export OUTCOME description."
  (let ((pathname
	  (testcase-outcome-pathname outcome)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output :if-exists :supersede)
      (describe outcome stream))))

(defun run-testcase (&key testcase-name testcase-body)
  "Run TESTCASE-BODY"
  (check-type testcase-name symbol)
  (let* ((testsuite-p
	   (not *testsuite-id*))
	 (*testsuite-id*
	   (or *testsuite-id* (make-testsuite-id)))
	 (this-testcase-outcome
	   (make-testcase-outcome
	    :path *testcase-path*
	    :name testcase-name))
	 (*testcase-path*
	   (cons testcase-name *testcase-path*)))
    (flet ((install-signal-handlers-and-run-testcase ()
	     (handler-bind
		 ((assertion-success
		    (lambda (condition)
		      (declare (ignore condition))
		      (with-slots (total success) *current-testcase-outcome*
			(incf total)
			(incf success))))
		  (assertion-failure
		    (lambda (condition)
		      (when *testcase-describe-failed-assertions*
			(format *testcase-describe-failed-assertions* "~&Assertion Failure~%~A" condition))
		      (with-slots (total failure) *current-testcase-outcome*
			(incf total)
			(incf failure))))
		  (assertion-condition
		    (lambda (condition)
		      (when *testcase-describe-failed-assertions*
			(format *testcase-describe-failed-assertions* "~&Assertion Condition~%~A" condition))
		      (with-slots (total condition) *current-testcase-outcome*
			(incf total)
			(incf condition))))
		  (testcase-end
		    (lambda (condition)
		      (with-slots ((current-total total)
				   (current-success success)
				   (current-failure failure)
				   (current-condition condition))
			  *current-testcase-outcome*
			(with-slots ((testcase-total total)
				     (testcase-success success)
				     (testcase-failure failure)
				     (testcase-condition condition))
			    (testcase-outcome condition)
			  (incf current-total testcase-total)
			  (incf current-success testcase-success)
			  (incf current-failure testcase-failure)
			  (incf current-condition testcase-condition))))))
	       (let ((*current-testcase-outcome* this-testcase-outcome))
		 (funcall testcase-body)
		 (setf *last-testsuite-outcome* *current-testcase-outcome*))
	       (describe *last-testsuite-outcome*)
	       (terpri)))
	   (just-run-testcase ()
	     (let ((*current-testcase-outcome* this-testcase-outcome))
	       (funcall testcase-body))
	     (signal 'testcase-end :outcome this-testcase-outcome)))
      (if testsuite-p
	  (install-signal-handlers-and-run-testcase)
	  (just-run-testcase))
      (values this-testcase-outcome))))
  
(defmacro define-testcase (testcase-name testcase-args &body body)
  "Define a test case function TESTCASE-NAME, accepting TESTCASE-ARGS with BODY.

The BODY is examined and assertions spotted in it are wrapped with extra code
installing restarts and aggregating outcomes for assertions and nested testcases..

The return value of a testcase is a OUTCOME, holding a precise description of test that
ran and their outcomes."
  (set-testcase-properties testcase-name)
  (multiple-value-bind (remaining-forms declarations doc-string)
      (alexandria:parse-body body :documentation t)
    `(prog1
	 (defun ,testcase-name ,testcase-args
	   ,@(when doc-string (list doc-string))
	   ,@declarations
	   (declare (optimize (safety 3) (debug 3)))
	   (flet ((testcase-body ()
		    ,@(define-testcase/wrap-assertion-forms remaining-forms)))
	     (run-testcase
	      :testcase-name (quote ,testcase-name)
	      :testcase-body #'testcase-body)))
       (export (quote ,testcase-name))
       (set-testcase-properties ',testcase-name))))

(defun list-testcases (&optional package-designator)
  "List testcases exported by PACKAGE-DESIGNATOR."
  (loop :for s :being :the :external-symbols :of (find-package package-designator)
	:when (get s :org.melusina.confidence/testcase)
	:collect s))

(defun quit ()
  "Quit the SBCL lisp image and set exit status accordingly."
  (let ((exit-code-success 0)
	(exit-code-failure 1)
	(exit-code-configuration 78))
    (unless *last-testsuite-outcome*
      (format t "~&Error: There was no testsuite performed.~%")
      (uiop:quit exit-code-configuration))
    (with-slots (success total) *last-testsuite-outcome*
      (when (= 0 total)
	(format t "~&Error: There was a testsuite performed but no test outcome recorded.~%")
	(uiop:quit exit-code-configuration))
      (export-testcase-outcome *last-testsuite-outcome*)
      (if (< success total)
	  (uiop:quit exit-code-failure)
	  (uiop:quit exit-code-success)))))

;;;; End of file `testcase.lisp'
