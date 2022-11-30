;;;; testcase.lisp — Testcases for Confidence

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

(defparameter *testcase-path* nil
  "The current path in the testcase hierarchy.
This is a list of symbols designating the argument-less testcases in the call stack.")

(defparameter *testsuite-name* "TESTSUITE"
  "The basename for the testsuite.

Usually TESTSUITE but commonly used values are ACCEPTANCE, INTEGRATION, PREFLIGHT, etc.")

(defparameter *testsuite-id* nil
  "A unique identfier for the current testsuite run batch.")


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


;;;
;;; SUPERVISE-ASSERTION
;;;

(define-condition assertion-failed (serious-condition)
  ((result
    :initarg :result
    :documentation "The ASSERTION-RESULT describing the failure."))
  (:report
   (lambda (condition stream)
     (with-slots (result) condition
       (describe result stream)))))

(defun supervise-assertion-1 (&key name form type argument-names argument-lambdas assertion-lambda)
  "Supervise the execution of ARGUMENTS-LAMBDA and ASSERTION-LAMBDA."
  (labels ((evaluation-strategy-1 (argument-condition argument-values)
	     (multiple-value-bind (success-p description)
		 (handler-case
		     (if argument-condition
                         (make-instance
                          'assertion-condition
                          :path *testcase-path*
                          :name name
                          :argument-names argument-names
                          :argument-values argument-values
                          :form form
                          :type type
                          :condition argument-condition)
                         (funcall assertion-lambda argument-values))
	     	   (t (unexpected-condition)
		     (make-instance
		      'assertion-condition
		      :path *testcase-path*
		      :name name
		      :argument-names argument-names
		      :argument-values argument-values
		      :form form
		      :type type
		      :condition unexpected-condition)))
	       (cond
		 ((typep success-p 'result)
		  success-p)
		 ((not success-p)
		  (make-instance 'assertion-failure
				 :path *testcase-path*
				 :name name
				 :argument-names argument-names
				 :argument-values argument-values
				 :form form
				 :type type
				 :description description))
		 (t
		  (make-instance 'assertion-success
				 :path *testcase-path*
				 :name name
				 :argument-names argument-names
				 :argument-values argument-values
				 :form form
				 :type type)))))
	   (evaluation-strategy (argument-values)
	     (evaluation-strategy-1
	      (first
	       (member-if (lambda (object) (typep object 'condition)) argument-values))
	      argument-values))
	   (supervise-evaluation-1 (argument-lambda)
	     (handler-case (funcall argument-lambda)
	       (t (unexpected-condition)
		 unexpected-condition)))
	   (supervise-evaluation (argument-lambdas)
	     (evaluation-strategy (mapcar #'supervise-evaluation-1 argument-lambdas))))
    (loop :with retry-p = t
	  :with result = nil
	  :while retry-p
	  :do (restart-case
		  (progn
		    (setf result (supervise-evaluation argument-lambdas))
		    (when (or (typep result 'assertion-failure)
			      (typep result 'assertion-condition))
		      (error 'assertion-failed :result result))
		    (setf retry-p nil)
		    (values result))
		(*retry ()
		  :report
		  (lambda (stream)
		    (format stream
			    "~@<Retry ~A.~@:>"
			    name)))
		(continue ()
		  :report
		  (lambda (stream)
		    (format stream
			    "~@<Record a failure for ~A and continue testing.~@:>"
			    name))
		  (setf retry-p nil))
		(ignore ()
		  :report
		  (lambda (stream)
		    (format stream
			    "~@<Record a success for ~A and continue testing.~@:>"
			    name))
		  (setf retry-p nil
			result
			(with-slots (argument-values) result
			  (make-instance
			   'assertion-success
			   :path *testcase-path*
			   :name name
			   :argument-names argument-names
			   :argument-values argument-values
			   :form form
			   :type type)))))
	  :finally (return result))))

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

(defun define-testcase/wrap-confidence-forms (body-forms)
  "Walks through BODY-FORMS and wrap Confidence forms found with a supervisor."
  (labels
      ((is-funcall-p (form)
         (when (and (listp form) (not (null form)) (symbolp (first form)) (listp (rest form)))
           (case (first form)
             ((funcall apply)
              (second form))
             (t (first form)))))
       (is-assert-name-p (symbol)
         (and
	  (>= (length (symbol-name symbol)) 7)
          (and (string= (symbol-name symbol) "ASSERT" :end1 6)
               (position (char (symbol-name symbol) 6) "-=<>"))
	  symbol))
       (is-assert-form-p (form)
         (get (is-assert-name-p (is-funcall-p form))
	      :org.melusina.confidence/assertion))
       (is-testcase-form-p (form)
         (get (is-funcall-p form)
	      :org.melusina.confidence/testcase))
       (wrap-confidence-forms (form)
         (cond
	   ((eq 'without-confidence (is-funcall-p form))
	    `(progn ,@(rest form)))
	   ((is-assert-form-p form)
            `(record-testcase-result
	      (supervise-assertion ,form)))
	   ((is-testcase-form-p form)
	    `(record-testcase-result ,form))
           ((is-funcall-p form)
            (cons (first form) (mapcar #'wrap-confidence-forms (rest form))))
           (t
	    form))))
    (mapcar #'wrap-confidence-forms body-forms)))

(defun testcase-result-pathname (result)
  "The pathname used to write RESULT description."
  (merge-pathnames
   (make-pathname
    :name (string-downcase (symbol-name (slot-value result 'name)))
    :type "log")
   #p"obj/confidence/"))

(defun export-testcase-result (result)
  "Export RESULT description."
  (let ((pathname
	  (testcase-result-pathname result)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output :if-exists :supersede)
      (describe result stream))))

(defun maybe-perform-testsuite-epilogue ()
  "When invoked from a testsuite, prints datails about the results."
  (when (< 1 (length *testcase-path*))
    (return-from maybe-perform-testsuite-epilogue *current-testcase-result*))
  (when (>= 1 (length *testcase-path*))
    (setf *testsuite-last-result* *current-testcase-result*)
    (describe *testsuite-last-result*)
    (format t "~&")))

(defmacro define-testcase (testcase-name testcase-args &body body)
  "Define a test case function TESTCASE-NAME, accepting TESTCASE-ARGS with BODY.

The BODY is examined and assertions spotted in it are wrapped with extra code
installing restarts and aggregating results for assertions and nested testcases..

The return value of a testcase is a RESULT, holding a precise description of test that
ran and their outcomes.

When *TESTCASE-INTERACTIVE-P* is NIL, batch mode is assumed and a summary of
failures is printed on stdout and the program is exited with a status
reflecting the failure or success of tests."
  (set-testcase-properties testcase-name)
  `(prog1
       (defun ,testcase-name ,testcase-args
	 (declare (optimize (safety 3) (debug 3)))
	 (let ((*testsuite-id*
		 (or *testsuite-id* (make-testsuite-id)))
	       (*current-testcase-result*
		 (make-instance
		  'testcase-result
		  :results nil
		  :name (quote ,testcase-name)
		  :path *testcase-path*))
	       (*testcase-path*
		 (cons (quote ,testcase-name) *testcase-path*)))
	   ,@(define-testcase/wrap-confidence-forms body)
	   (maybe-perform-testsuite-epilogue)))
     (export (quote ,testcase-name))
     (set-testcase-properties ',testcase-name)))

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
    (unless *testsuite-last-result*
      (format t "~&Error: There was no testsuite performed.~%")
      (uiop:quit exit-code-configuration))
    (with-slots (success total) *testsuite-last-result*
      (when (= 0 total)
	(format t "~&Error: There was a testsuite performed but no test result recorded.~%")
	(uiop:quit exit-code-configuration))
      (export-testcase-result *testsuite-last-result*)
      (if (< success total)
	  (uiop:quit exit-code-failure)
	  (uiop:quit exit-code-success)))))

;;;; End of file `testcase.lisp'
