;;; When *DEBUGGER-HOOK* (or INVOKE-DEBUGGER itself) returns instead of
;;; transferring control, the error still has to unwind. What unwinds must not
;;; run the handlers again: they have seen the condition once already.
;;;
;;; Every test here watches with a HANDLER-BIND that declines (it only records
;;; what it was given), so the error goes on to the debugger. The outer
;;; HANDLER-CASE that stops the unwind only matches once the debugger stage has
;;; been reached (RDH-PAST-DEBUGGER-P), so it cannot take the error first.
;;;
;;; What unwinds after the debugger stage is never signalled: the outer
;;; HANDLER-CASE stops it because HANDLER-CASE also matches an error that
;;; unwinds past it by its condition's type. That holds for the tree-walk
;;; interpreter's HANDLER-CASE as well as the compiled one, so every test here
;;; runs in every mode.

(defun rdh-set-switch (on)
  (setf (dotnet:static "DotCL.ConditionSystem" "UnhandledErrorsEnterDebugger") on))

(defvar *rdh-past-debugger* nil)
(defun rdh-past-debugger-p (c)
  (declare (ignore c))
  *rdh-past-debugger*)

(defun rdh-returning-hook (c h)
  (declare (ignore c h))
  (setf *rdh-past-debugger* t)
  nil)

(defun rdh-watch (switch thunk)
  "Run THUNK with a returning *DEBUGGER-HOOK*. Return the conditions the
HANDLER-BIND around it was given, oldest first."
  (let ((seen '()))
    (setf *rdh-past-debugger* nil)
    (handler-case
        (handler-bind ((error (lambda (c) (push c seen))))
          (let ((*debugger-hook* #'rdh-returning-hook))
            (rdh-set-switch switch)
            (unwind-protect (funcall thunk)
              (rdh-set-switch nil))))
      ((satisfies rdh-past-debugger-p) () nil))
    (reverse seen)))

(defun rdh-times-first (seen)
  "How many times the first condition the handler saw was given to it."
  (count (first seen) seen))

;;; The handlers here are the ones under test, so DEFTEST must not wrap its
;;; own HANDLER-CASE around them: that would take the error before the debugger.
(let ((*deftest-contain-errors* nil))

  ;; An error the runtime signals itself, at the REPL (the switch on): the
  ;; condition reaches the handler once, and the debugger stage runs.
  (deftest rdh-runtime-error-signalled-once
    (let ((seen (rdh-watch t (lambda () (car 1)))))
      (list (type-of (first seen)) (rdh-times-first seen) *rdh-past-debugger*))
    (type-error 1 t))

  ;; The same for ERROR, with and without the switch.
  (deftest rdh-error-signalled-once
    (list (rdh-times-first (rdh-watch nil (lambda () (error "boom"))))
          (rdh-times-first (rdh-watch t (lambda () (error "boom")))))
    (1 1))

  ;; INVOKE-DEBUGGER itself returning. CL's never does, but it can be replaced,
  ;; and ERROR calls whatever is there. The unwind that follows used to build
  ;; its exception with the constructor that signals, so the same SIMPLE-ERROR
  ;; reached the handler twice.
  (deftest rdh-invoke-debugger-replaced-signals-once
    (let ((original (symbol-function 'invoke-debugger)))
      (unwind-protect
           (progn
             (let ((dotcl::*package-locks-disabled* t))
               (setf (symbol-function 'invoke-debugger)
                     (lambda (c) (declare (ignore c)) (setf *rdh-past-debugger* t) nil)))
             (let ((seen (rdh-watch nil (lambda () (error "boom")))))
               (list (length seen) (type-of (first seen)) *rdh-past-debugger*)))
        (let ((dotcl::*package-locks-disabled* t))
          (setf (symbol-function 'invoke-debugger) original))))
    (1 simple-error t)))

;;; -- The debugger's own report is not a second error -------------------------
;;;
;;; When *DEBUGGER-HOOK* returns, INVOKE-DEBUGGER goes on to the debugger, and
;;; with nobody to ask it unwinds with a report of its own ("Debugger:
;;; non-interactive session; ...", or "Debugger: stdin closed, no ABORT
;;; restart; ..." when the terminal's input ended). That report used to be built
;;; with the constructor that signals, so a HANDLER-BIND ERROR clause ran twice
;;; for one (ERROR ...): once for the error, once for the report.
;;;
;;; The subprocess test below covers the same path with a terminal's standard
;;; input at end of file.
(require "asdf")

(let ((*deftest-contain-errors* nil))

  (deftest rdh-error-clause-runs-once
    (list (length (rdh-watch nil (lambda () (error "boom"))))
          (length (rdh-watch t (lambda () (error "boom"))))
          (length (rdh-watch t (lambda () (car 1)))))
    (1 1 1))

  ;; The report still unwinds, and HANDLER-CASE still sees it as an ERROR:
  ;; only the second signal is gone.
  (deftest rdh-report-still-unwinds
    (let ((reached nil))
      (setf *rdh-past-debugger* nil)
      (list (handler-case
                (let ((*debugger-hook* #'rdh-returning-hook))
                  (handler-bind ((error (lambda (c) (declare (ignore c)) nil)))
                    (error "boom"))
                  (setf reached t))
              ((satisfies rdh-past-debugger-p) (e)
                (and (search "Debugger: non-interactive session" (princ-to-string e)) t)))
            reached))
    (t nil)))

;;; -- HANDLER-CASE takes an error that unwinds without being signalled --------
;;;
;;; INVOKE-DEBUGGER on a condition that is not an ERROR: no ERROR clause can
;;; take the condition itself, so what reaches IGNORE-ERRORS is the debugger's
;;; report, which unwinds without a signal. Compiled HANDLER-CASE matched it by
;;; type; the interpreter's took only what was signalled, so the report went on
;;; to the top level there. Both now answer NIL and the report. The EVAL forms
;;; run through the tree-walk interpreter under :INTERPRET, where LOAD would
;;; compile the rest of this file.
;;;
;;; *DEBUGGER-HOOK* is bound to NIL so the debugger itself is what runs: a
;;; script started with --core has a hook that prints and exits the process.

(defun rdh-ignore-errors-invoke-debugger ()
  (let ((r (multiple-value-list
            (let ((*debugger-hook* nil))
              (ignore-errors (invoke-debugger (make-condition 'warning)))))))
    (list (first r) (typep (second r) 'error))))

(deftest rdh-non-error-invoke-debugger-ignore-errors
  (rdh-ignore-errors-invoke-debugger)
  (nil t))

(deftest rdh-non-error-invoke-debugger-ignore-errors-eval
  (eval '(let ((r (multiple-value-list
                   (let ((*debugger-hook* nil))
                     (ignore-errors (invoke-debugger (make-condition 'warning)))))))
          (list (first r) (typep (second r) 'error))))
  (nil t))

;;; The first clause whose type matches takes it, and a clause that does not
;;; match lets it go on to the next HANDLER-CASE out.
(deftest rdh-unsignalled-error-clause-order-eval
  (eval '(let ((*debugger-hook* nil))
          (handler-case
              (handler-case (invoke-debugger (make-condition 'warning))
                (warning () :warning)
                (type-error () :type-error))
            (control-error () :control-error)
            (error (e) (and (search "Debugger: non-interactive session"
                                    (princ-to-string e))
                            :error)))))
  :error)

;;; The clause runs after the unwind: a cleanup in the body comes first, as it
;;; does when the error is signalled to the clause.
(deftest rdh-unsignalled-error-cleanup-first-eval
  (eval '(let ((log '()) (*debugger-hook* nil))
          (handler-case (unwind-protect (invoke-debugger (make-condition 'warning))
                          (push :cleanup log))
            (error () (push :clause log)))
          (nreverse log)))
  (:cleanup :clause))

;;; The interpreter's HANDLER-CASE clause binds its variable with the clause's
;;; own declarations, and :NO-ERROR still gets every value.
(deftest rdh-handler-case-clause-declarations-eval
  (eval '(list (handler-case (error "x") (error (c) (declare (ignore c)) :ignored))
               (handler-case (error "x")
                 (error (c) (declare (special c))
                   (funcall (lambda () (declare (special c)) (typep c 'simple-error)))))
               (multiple-value-list
                (handler-case (values 1 2) (:no-error (a b) (values b a))))))
  (:ignored t (2 1)))

;;; A script with an interactive debugger whose standard input is at end of
;;; file: the debugger reads EOF, finds no ABORT, and unwinds with its report.
;;; Run in another process so its standard input can be an empty file; the
;;; count is printed by an UNWIND-PROTECT on the way out.

(defvar *rdh-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun rdh-image ()
  "This process's --core, if it was started with one."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rdh-run-script (forms)
  "Run FORMS as a script in another process with an empty standard input;
return its output and error output as one string."
  (let* ((dir (uiop:ensure-directory-pathname
               (concatenate 'string
                            (regression-temp-dir)
                            "/dotcl-debugger-hook-returns-test/")))
         (script (merge-pathnames "script.lisp" dir))
         (empty (merge-pathnames "empty.txt" dir)))
    (ensure-directories-exist script)
    (with-open-file (out script :direction :output :if-exists :supersede)
      (let ((*package* (find-package "CL-USER")))
        (dolist (form forms)
          (prin1 form out)
          (terpri out))))
    (with-open-file (out empty :direction :output :if-exists :supersede))
    (multiple-value-bind (out err)
        (ignore-errors
         (uiop:run-program (append (list *rdh-exe*) (rdh-image)
                                   (list (namestring script)))
                           :input empty
                           :output :string
                           :error-output :string
                           :ignore-error-status t))
      (concatenate 'string (or out "") (or err "")))))

(defvar *rdh-eof-transcript*
  (rdh-run-script
   '((setf (dotnet:static "DotCL.Debugger" "InteractiveRepl") t)
     (defvar *n* 0)
     (unwind-protect
          (handler-bind ((error (lambda (c) (declare (ignore c)) (incf *n*))))
            (let ((*debugger-hook* (lambda (c h) (declare (ignore c h)) nil)))
              (error "boom")))
       (format t "~&RDH-CALLS=~d~%" *n*)
       (finish-output)))))

(deftest rdh-stdin-eof-report-not-signalled
  (list (and (search "Debugger: stdin closed" *rdh-eof-transcript*) t)
        (and (search "RDH-CALLS=1" *rdh-eof-transcript*) t))
  (t t))
