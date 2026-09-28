;;; The REPL's history variables: CLHS 25.1.1's * ** ***, + ++ +++, / // ///
;;; and -.
;;;
;;; The loop kept none of them, so there was no way to reach the value a form
;;; had just returned without binding it first. The rotation itself is a
;;; function of the form and the values, in DotCL.ReplHistory, so most of it is
;;; checked here by calling the same entry points the loop calls.
;;;
;;; Two things cannot be reached that way and are checked by running a REPL in
;;; another process: that the values written are the values of the evaluation
;;; rather than of something else, which depends on the multiple-value channel
;;; being read at the right moment, and that an evaluation which exits
;;; non-locally leaves the history alone, which is a property of where the call
;;; sits in the loop rather than of the rotation.

(require "asdf")

(defun rhv-iteration (form values)
  "One iteration of a read-eval-print loop, as far as the history is concerned:
note FORM as the one being evaluated, then record VALUES as what it returned."
  (dotnet:static "DotCL.ReplHistory" "BeforeEval" form)
  (dotnet:static "DotCL.ReplHistory" "Record" values))

(defun rhv-values ()
  (list * ** ***))

(defun rhv-forms ()
  (list + ++ +++))

(defun rhv-lists ()
  (list / // ///))

;;; -- The rotation ------------------------------------------------------------

;;; One value: it is both the primary value and the whole of the value list,
;;; and the form that produced it becomes +.
(deftest rhv-one-iteration
  (progn
    (rhv-iteration '(+ 1 2) '(3))
    (values * / + -))
  3 (3) (+ 1 2) (+ 1 2))

;;; - is the form being evaluated now and + is the one before it, so during an
;;; iteration they differ. This is the shape the reported case turns on.
(deftest rhv-minus-is-current-plus-is-previous
  (progn
    (rhv-iteration '(+ 1 2) '(3))
    (dotnet:static "DotCL.ReplHistory" "BeforeEval" '(list * + / -))
    (values + -))
  (+ 1 2) (list * + / -))

;;; Three iterations fill all nine of the three-deep variables, oldest last.
(deftest rhv-three-deep
  (progn
    (rhv-iteration 'one '(1))
    (rhv-iteration 'two '(2))
    (rhv-iteration 'three '(3))
    (values (rhv-values) (rhv-forms) (rhv-lists)))
  (3 2 1)
  (three two one)
  ((3) (2) (1)))

;;; Several values: / is all of them, * is the first.
(deftest rhv-multiple-values
  (progn
    (rhv-iteration 'mv '(1 2 3))
    (values * /))
  1 (1 2 3))

;;; No values at all: * is NIL, which is what CLHS says, and / is the empty
;;; list rather than a list holding NIL. The two are easy to confuse and the
;;; difference shows the moment anyone takes the length of /.
(deftest rhv-zero-values
  (progn
    (rhv-iteration 'something '(7))
    (rhv-iteration 'nothing '())
    (values * / ** //))
  nil nil 7 (7))

;;; The previous value survives one iteration of rotation even when that
;;; iteration produced nothing.
(deftest rhv-rotation-does-not-lose-the-previous-value
  (progn
    (rhv-iteration 'a '(1))
    (rhv-iteration 'b '())
    (rhv-iteration 'c '(3))
    (rhv-values))
  (3 nil 1))

;;; -- A whole session ---------------------------------------------------------

;;; One process, several questions of it. Starting a REPL is the only way to
;;; see the values that an evaluation really published and to see an error
;;; leave the history where it was, so it is worth the second or so it costs,
;;; but it is worth it once.

(defvar *rhv-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rhv-script*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-repl-history-variables-test/")))
    (ensure-directories-exist dir)
    (pathname (concatenate 'string dir "session.lisp"))))

(defun rhv-image ()
  "The image arguments to start the child REPL on: this process's own.

Only --core is forwarded, and only when this process was given one. A build
without the emitter cannot make a REPL out of nothing and refuses to start
without an image; a build with the emitter can, and is given none here, which
is the same thing a developer gets from running the executable by hand.

Read off this process's command line rather than by looking for a core file in
the tree. A core left behind by an earlier build exists whether or not it has
anything to do with the build under test, and a child started on one would be
running different code from the one these tests are about -- silently, and
with every assertion still passing."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rhv-run (lines)
  "Feed LINES to a REPL in another process and return what it printed.
Standard error is kept out of it, so the one line an error prints does not
look like a value."
  (with-open-file (out *rhv-script* :direction :output :if-exists :supersede)
    (dolist (line lines)
      (write-string line out)
      (terpri out)))
  (or (ignore-errors
       (uiop:run-program (append (list *rhv-exe*) (rhv-image) (list "repl"))
                         :input *rhv-script*
                         :output :string
                         :error-output :string
                         :ignore-error-status t))
      ""))

(defvar *rhv-transcript*
  (rhv-run '("(+ 1 2)"
             "(list * + / -)"
             "(values 1 2)"
             "(list * /)"
             "(values)"
             "(list * /)"
             "77"
             ;; Signals, and leaves through ABORT from its own handler rather
             ;; than through the debugger, which reading from a pipe has nobody
             ;; to answer.
             "(handler-bind ((error (lambda (c) (declare (ignore c)) (abort)))) (car 1))"
             "(list * +)")))

(defun rhv-printed-p (text)
  (and (search text *rhv-transcript*) t))

;;; The reported case, and the answer SBCL gives for it.
(deftest rhv-session-reported-case
  (rhv-printed-p "(3 (+ 1 2) (3) (LIST * + / -))")
  t)

;;; The values of the evaluation, not of whatever ran inside the printer
;;; afterwards: / holds both values of (VALUES 1 2) and * the first.
(deftest rhv-session-multiple-values
  (rhv-printed-p "(1 (1 2))")
  t)

;;; (VALUES) leaves * NIL and / empty.
(deftest rhv-session-zero-values
  (rhv-printed-p "(NIL NIL)")
  t)

;;; An evaluation that signals does not advance the history: * and + still
;;; belong to the last form that finished.
(deftest rhv-session-error-does-not-advance
  (rhv-printed-p "(77 77)")
  t)

;;; And the session as a whole ran, so a failure above is a wrong answer rather
;;; than an empty transcript.
(deftest rhv-session-ran
  (rhv-printed-p "dotcl REPL")
  t)
