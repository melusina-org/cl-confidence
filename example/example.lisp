;;;; example.lisp — Example for Confidence

;;;; Confidence (https://github.com/melusina-org/cl-confidence)
;;;; This file is part of Confidence.
;;;;
;;;; Copyright © 2019–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT


;;;; This example file displays a few features of Confidence. To use it,
;;;; evaluate the toplevel forms one by one and read the comments.



;;;;
;;;; Package Declarations
;;;;

#+quicklisp
(unless (find-package "ORG.MELUSINA.CONFIDENCE")
  (ql:quickload "ORG.MELUSINA.CONFIDENCE"))

;; We create a package to host tests we write. We import required symbols so that
;; we avoid to just :use Confidence.
(defpackage #:org.melusina.confidence/example
  (:use #:cl)
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:assert-nil
   #:assert-string=
   #:assert-t)
  (:export
   ;; A testcase function defined with DEFINE-TESTCASE is always added
   ;; to the export list.
   ))

(in-package #:org.melusina.confidence/example)



;;;;
;;;; A Succesful Testcase
;;;;

;; We define a testcase VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A which exercises
;; some aspects of string designator equality for symbols in Common Lisp.
(define-testcase validate-common-lisp-string-designator-a ()
  (assert-t (string-equal "a" :A))
  (assert-t (string-equal "A" :A))
  (assert-nil (string= "a" :A))
  (assert-t (string= "A" :A)))

;; We run the VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A TEST, note that
;; the testcase is represented by a function in the export list of
;; the package.
(org.melusina.confidence/example:validate-common-lisp-string-designator-a)

;; When working in SLIME/Emacs, the “Eval defun” (C-M x) command prints
;; the following description message in the slime REPL window

#|
Name: VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A
Total: 4
Success: 4/4 (100%)
Failure: 0/4 (0%)
Condition: 0/4 (0%)
Outcome: Success
|#

;; An object of type ORG.MELUSINA.CONFIDENCE:TESTCASE-RESULT is returned and
;; displayed below the modeline in Emacs.
;;
;; Just issuing the call at the REPL shows the description message and the
;; returned value, displayed below with line breaking for easier reading.

#|
#<ORG.MELUSINA.CONFIDENCE:TESTCASE-RESULT
 :NAME ORG.MELUSINA.CONFIDENCE/EXAMPLE:VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A
 :ORG.MELUSINA.CONFIDENCE::TOTAL 4
 :ORG.MELUSINA.CONFIDENCE::SUCCESS 4
 :ORG.MELUSINA.CONFIDENCE::FAILURE 0
 :CONDITION 0 {7000000000}
>
|#

;; The description message above is the actual description provided by DESCRIBE for
;; the TESTCASE-RESULT value returned by Confidence.


;;;;
;;;; A Failing Testcase
;;;;

;; We define a testcase DEMONSTRATE-ASSERTION-FAILURE which displays the behaviour
;; of Confidence when an assertion fails.
(define-testcase demonstrate-assertion-failure ()
  (assert-string= "AABA"
		  (concatenate 'string "A" "A" "A" "A")))

(org.melusina.confidence/example:demonstrate-assertion-failure)

;; The following test summary is displayed:

#|
Name: ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ASSERTION-FAILURE
Total: 1
Success: 0/1 (0%)
Failure: 1/1 (100%)
Condition: 0/1 (0%)
Outcome: Failure
================================================================================
#<ASSERTION-FAILURE {7000000000}> is an assertion result of type ASSERTION-FAILURE.
Type: :FUNCTION
Name: ORG.MELUSINA.CONFIDENCE:ASSERT-STRING=
Path:
  ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ASSERTION-FAILURE
Arguments:
 Argument #1: "AABA"
 Argument #2: "AAAA"
Form: (ORG.MELUSINA.CONFIDENCE:ASSERT-STRING= "AABA"
                                              (CONCATENATE 'STRING "A"
                                                           "A" "A" "A"))
Outcome: Failure
Description: Assert that STRING1 and STRING2 satisfy the STRING= predicate.
This assertion supports the same keyword parameters as STRING=.
Every character of STRING1 is equal to the character of STRING2 at
the same index upto index 2. However this condition does not hold for characters
at position 2, which are #\B and #\A.
  In this call, forms in argument position evaluate as:

  STRING1: "AABA"

  STRING2: (CONCATENATE 'STRING "A" "A" "A" "A") => "AAAA"
|#

;; The output feature an overall summary before it dives into the details of the failed
;; assertions, after the separation line made of several EQUAL-signs.
;;
;; - The Type of the failed assertion is almost always :FUNCTION,
;;   but ASSERT-CONDITION is a macro.
;; - The Name is the symbol name of the failed assertion.
;; - The Path locates the assertion form in the codebase and in the test hierarchy.
;; - The Arguments is the vector of arguments of the assertions.
;; - The Form is the assertion form as it appears in the program.
;; - The Outcome displays a Failure.
;; - The Description is an explanatory text providing context to the assertion failure,
;;   especially the result of evaluating forms in argument position in the expression
;;   supplied by Form.
;;
;; The Description is a very important field and must be concise while providing
;; enough details so that failure analysis can start without touching a debugger
;; or altering the code.
;;
;; This description message is generated by the assertion and Confidence users who
;; define new assertions with DEFINE-ASSERTION have the chance to define how
;; the description message looks like.

;; The corresponding testcase result values is displayed below, adding linebreaks for
;; readability.

#|
#<ORG.MELUSINA.CONFIDENCE:TESTCASE-RESULT
 :NAME ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ASSERTION-FAILURE
 :ORG.MELUSINA.CONFIDENCE::TOTAL 1
 :ORG.MELUSINA.CONFIDENCE::SUCCESS 0
 :ORG.MELUSINA.CONFIDENCE::FAILURE 1
 :CONDITION 0 {7000000000}
>
|#


;;;;
;;;; A Testcase where assertion argument evaluation triggers a condition
;;;;

;; We define a testcase DEMONSTRATE-ARGUMENT-FAILURE which displays the behaviour
;; of Confidence when one of the arguments of an assertion yields a condition.

(define-testcase demonstrate-argument-failure ()
  (assert-string= "AABA"
		  (error "AAAA")))

;; We can run the testcase we just defined.
(org.melusina.confidence/example:demonstrate-argument-failure)

;; The resulting testcase result value has its description reproduced below,
;; it is very similar to the description of a failed assertion but also
;; provides details about the condition signalled by the form in argument
;; position.

#|
Name: ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ARGUMENT-FAILURE
Total: 1
Success: 0/1 (0%)
Failure: 0/1 (0%)
Condition: 1/1 (100%)
Outcome: Failure
================================================================================
#<ASSERTION-CONDITION {7000000000}> is an assertion result of type ASSERTION-CONDITION.
Type: :FUNCTION
Name: ORG.MELUSINA.CONFIDENCE:ASSERT-STRING=
Path:
  ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ARGUMENT-FAILURE
Arguments:
 Argument #1: "AABA"
 Argument #2: #<SIMPLE-ERROR "AAAA" {7000000000}>
Form: (ORG.MELUSINA.CONFIDENCE:ASSERT-STRING= "AABA" (ERROR "AAAA"))
Outcome: Condition
Description:
  In this call, forms in argument position evaluate as:

  STRING1: "AABA"

  STRING2: (ERROR "AAAA") => CONDITION
    The evaluation of this form yielded a condition

#<SIMPLE-ERROR "AAAA" {7000000000}>
  [condition]

Slots with :INSTANCE allocation:
  FORMAT-CONTROL                 = "AAAA"
  FORMAT-ARGUMENTS               = NIL
|#

;; The corresponding testcase result values is displayed below, adding
;; linebreaks for readability.

#|
#<ORG.MELUSINA.CONFIDENCE:TESTCASE-RESULT
 :NAME ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ARGUMENT-FAILURE
 :ORG.MELUSINA.CONFIDENCE::TOTAL 1
 :ORG.MELUSINA.CONFIDENCE::SUCCESS 0
 :ORG.MELUSINA.CONFIDENCE::FAILURE 0
 :CONDITION 1 {7000000000}
>
|#


;;;;
;;;; Creating test hierarchies
;;;;

;; Test hierarchies are just test cases calling other test cases in
;; Confidence. Remember that a test case defined by DEFINE-TESTCASE is a
;; function. Hence we can define a test suite called RUN-ALL-TESTS and
;; decorated with a documentation string as follows:

(define-testcase run-all-tests ()
  "Run all tests for the Confidence introduction example."
  (validate-common-lisp-string-designator-a)
  (demonstrate-assertion-failure)
  (demonstrate-argument-failure))

;; We can now run all tests and see the different testcase results produced by
;; VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A, DEMONSTRATE-ASSERTION-FAILURE
;; and DEMONSTRATE-ARGUMENT-FAILURE aggregated together in a testcase result
;; whose description is reproduced below.  The main difference to the individual
;; description we saw above is the Path attribute in the reports, which locates
;; the failed assertions form in the codebase and in the test hierarchy.

(org.melusina.confidence/example:run-all-tests)

#|
Name: ORG.MELUSINA.CONFIDENCE/EXAMPLE:RUN-ALL-TESTS
Total: 6
Success: 4/6 (67%)
Failure: 1/6 (17%)
Condition: 1/6 (17%)
Outcome: Failure
================================================================================
Name: ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ASSERTION-FAILURE
Total: 1
Success: 0/1 (0%)
Failure: 1/1 (100%)
Condition: 0/1 (0%)
Outcome: Failure
================================================================================
#<ASSERTION-FAILURE {7000000000}> is an assertion result of type ASSERTION-FAILURE.
Type: :FUNCTION
Name: ORG.MELUSINA.CONFIDENCE:ASSERT-STRING=
Path:
  ORG.MELUSINA.CONFIDENCE/EXAMPLE:RUN-ALL-TESTS
    ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ASSERTION-FAILURE
Arguments:
 Argument #1: "AABA"
 Argument #2: "AAAA"
Form: (ORG.MELUSINA.CONFIDENCE:ASSERT-STRING= "AABA"
                                              (CONCATENATE 'STRING "A"
                                                           "A" "A" "A"))
Outcome: Failure
Description: Assert that STRING1 and STRING2 satisfy the STRING= predicate.
This assertion supports the same keyword parameters as STRING=.
Every character of STRING1 is equal to the character of STRING2 at
the same index upto index 2. However this condition does not hold for characters
at position 2, which are #\B and #\A.
  In this call, forms in argument position evaluate as:

  STRING1: "AABA"

  STRING2: (CONCATENATE 'STRING "A" "A" "A" "A") => "AAAA"

================================================================================
Name: ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ARGUMENT-FAILURE
Total: 1
Success: 0/1 (0%)
Failure: 0/1 (0%)
Condition: 1/1 (100%)
Outcome: Failure
================================================================================
#<ASSERTION-CONDITION {7000000000}> is an assertion result of type ASSERTION-CONDITION.
Type: :FUNCTION
Name: ORG.MELUSINA.CONFIDENCE:ASSERT-STRING=
Path:
  ORG.MELUSINA.CONFIDENCE/EXAMPLE:RUN-ALL-TESTS
    ORG.MELUSINA.CONFIDENCE/EXAMPLE:DEMONSTRATE-ARGUMENT-FAILURE
Arguments:
 Argument #1: "AABA"
 Argument #2: #<SIMPLE-ERROR "AAAA" {7000000000}>
Form: (ORG.MELUSINA.CONFIDENCE:ASSERT-STRING= "AABA" (ERROR "AAAA"))
Outcome: Condition
Description:
  In this call, forms in argument position evaluate as:

  STRING1: "AABA"

  STRING2: (ERROR "AAAA") => CONDITION
    The evaluation of this form yielded a condition

#<SIMPLE-ERROR "AAAA" {7000000000}>
  [condition]

Slots with :INSTANCE allocation:
  FORMAT-CONTROL                 = "AAAA"
  FORMAT-ARGUMENTS               = NIL
|#

;; The corresponding testcase result values is displayed below, adding linebreaks for
;; readability.

#|
#<ORG.MELUSINA.CONFIDENCE:TESTCASE-RESULT
 :NAME ORG.MELUSINA.CONFIDENCE/EXAMPLE:RUN-ALL-TESTS
 :ORG.MELUSINA.CONFIDENCE::TOTAL 6
 :ORG.MELUSINA.CONFIDENCE::SUCCESS 4
 :ORG.MELUSINA.CONFIDENCE::FAILURE 1
 :CONDITION 1 {7000000000}
>
|#


;;;;
;;;; Parametrised Tests
;;;;

;; A testcase is a function and can be parametrised, for instance, the testcase
;; VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A could be generalised as

(define-testcase validate-common-lisp-string-designator (keyword uppercase-name lowercase-name)
  "Exercise string predicates on KEYWORD and its UPPERCASE-NAME and LOWERCASE-NAME.
This testcase encodes several basic expectations about the relationships between a
keyword name (the name of the symbol) and its string representations."
  (assert-t (string-equal lowercase-name keyword))
  (assert-t (string-equal uppercase-name keyword))
  (assert-nil (string= lowercase-name keyword))
  (assert-t (string= uppercase-name keyword)))

;; This allows us to redefine VALIDATE-COMMON-LISP-STRING-DESIGNATOR-A as a specific case:
(define-testcase validate-common-lisp-string-designator-a ()
  (validate-common-lisp-string-designator :a "A" "a"))

;; We can then quickly add more tests at an eloquent level of abstraction:
(define-testcase validate-common-lisp-string-designator-batch ()
  (validate-common-lisp-string-designator :a "A" "a")
  (validate-common-lisp-string-designator :b "B" "b")
  (validate-common-lisp-string-designator :c "C" "c"))

;; The latter can also be rewritten using various looping techniques, as displayed by
;; the following examples:
(define-testcase validate-common-lisp-string-designator/dolist ()
  (dolist (spec '((:a "A" "a")
		  (:b "B" "b")
		  (:c "C" "c")))
    (destructuring-bind (keyword uppercase-name lowercase-name) spec
      (validate-common-lisp-string-designator keyword uppercase-name lowercase-name))))

(define-testcase validate-common-lisp-string-designator/loop ()
  (loop :for (keyword uppercase-name lowercase-name)
	:in '((:a "A" "a")
	      (:b "B" "b")
	      (:c "C" "c"))
	:do (validate-common-lisp-string-designator keyword uppercase-name lowercase-name)))


;;;;
;;;; Running tests from the command line
;;;;

;; When running a testcase from the command line, Confidence prints the test summary
;; and terminates the process with the appropriate exit code.  This makes it easy
;; to run testcases from the command line and interpret their results, as in the
;; following example:

#|
sbcl --eval '(ql:quickload "org.melusina.confidence/testsuite" :silent t)"\
     --eval "(org.melusina.confidence/testsuite:run-all-tests)"
|#

;; The script `development/testsuite' give a more realistic example how to make
;; running the testsuite or any of its part easy to developers as well as
;; to the continuous delivery pipeline.

;; The strategy that Confidence uses to decide wether to exit after a testcase completed
;; is actually controlled by the special variable *TESTCASE-INTERACTIVE-P*
;; which is initialised appropriately by looking at the :SWANK feature. This heuristic
;; is good enough to tell command line use from interactive use in many cases but can be
;; overridden explicitly by setting the value of *TESTCASE-INTERACTIVE-P*
;; after loading Confidence, as in:

#|
sbcl --eval '(ql:quickload "org.melusina.confidence/testsuite" :silent t)"\
     --eval "(setf org.melusina.confidence/testsuite:*testcase-interactive-p*)"\
     --eval "(org.melusina.confidence/testsuite:run-all-tests)"
|#


;;;;
;;;; Reference Manual
;;;;

;; There is a reference manual which can be built locally using TexInfo and
;; the “development/makedoc” script or downloaded from GitHub actions:
;;
;;  https://github.com/melusina-org/cl-confidence/actions
;;
;; Look for the artefacts on a recent workflow run, three files are available:
;; PDF, HTML and INFO.



;;;;
;;;; Confidence in Public Repositories
;;;;

;; Here is a list of public repositories using Confidence:
;;
;; - https://github.com/melusina-org/cl-confidence
;; - https://github.com/melusina-org/cl-rashell
;; - https://github.com/melusina-org/cl-atelier

;;;; End of file `example.lisp'
