;;; An error the runtime signals itself -- CAR of a non-list, an undefined
;;; function, an index out of bounds, division by zero -- enters the debugger
;;; at the REPL when nothing handles it, the way an unhandled ERROR does.
;;;
;;; Only ERROR, BREAK and INVOKE-DEBUGGER used to get there. Everything the
;;; runtime signalled went through the handlers and then just unwound, so the
;;; REPL printed one line and the debugger's :bt and :locals were out of reach
;;; for exactly the mistakes made most often at a prompt.
;;;
;;; The REPL asks for it with ConditionSystem.UnhandledErrorsEnterDebugger
;;; around the evaluation of each form. The in-process tests below set the
;;; same switch and watch through *DEBUGGER-HOOK*, which INVOKE-DEBUGGER runs
;;; before the debugger itself; the last group runs a real REPL.

(require "asdf")

(defun rred-set-switch (on)
  (setf (dotnet:static "DotCL.ConditionSystem" "UnhandledErrorsEnterDebugger") on))

(defmacro rred-with-switch (&body body)
  `(progn
     (rred-set-switch t)
     (unwind-protect (progn ,@body)
       (rred-set-switch nil))))

(defvar *rred-log* nil)

(defun rred-hook (condition hook)
  (declare (ignore hook))
  (push (list :hook (type-of condition) (dotcl:backtrace)) *rred-log*)
  (throw 'rred-out :hooked))

(defun rred-inner (x) (car x))
(defun rred-middle (x) (rred-inner x))
(defun rred-outer (x) (rred-middle x))

;;; DEFTEST's own HANDLER-CASE would take the error before the debugger could,
;;; which is the point of the second test but not of the others.
(let ((*deftest-contain-errors* nil))

  ;; The hook runs, with the condition the runtime signalled.
  (deftest rred-runtime-error-reaches-debugger
    (let ((*rred-log* nil))
      (list (catch 'rred-out
              (rred-with-switch
                (let ((*debugger-hook* #'rred-hook))
                  (rred-outer 1))))
            (second (first *rred-log*))))
    (:hooked type-error))

  ;; And it runs before anything unwinds: the frames that signalled are still
  ;; there, which is what makes :bt worth having. Compiled only: the tree-walk
  ;; interpreter's calls do not all push a named frame.
  (deftest-compiled-only rred-frames-still-live
    (let ((*rred-log* nil))
      (catch 'rred-out
        (rred-with-switch
          (let ((*debugger-hook* #'rred-hook))
            (rred-outer 1))))
      (let ((bt (third (first *rred-log*))))
        (list (and (member "RRED-INNER" bt :test #'string=) t)
              (and (member "RRED-MIDDLE" bt :test #'string=) t)
              (and (member "RRED-OUTER" bt :test #'string=) t))))
    (t t t))

  ;; Each kind the report named, not only CAR.
  (deftest rred-each-kind
    (let ((*rred-log* nil))
      (dolist (thunk (list (lambda () (+ 1 (read-from-string "a")))
                           (lambda () (funcall (intern "RRED-NO-SUCH-FUNCTION") 1))
                           (lambda () (elt (list 1 2) 9))
                           (lambda () (/ 1 (read-from-string "0")))))
        (catch 'rred-out
          (rred-with-switch
            (let ((*debugger-hook* #'rred-hook))
              (funcall thunk)))))
      (mapcar #'second (reverse *rred-log*)))
    (type-error undefined-function type-error division-by-zero))

  ;; A handler that declines runs first; the debugger comes after it.
  (deftest rred-handler-bind-runs-first
    (let ((*rred-log* nil))
      (catch 'rred-out
        (rred-with-switch
          (let ((*debugger-hook* #'rred-hook))
            (handler-bind ((error (lambda (c) (push (list :handler (type-of c)) *rred-log*))))
              (car 1)))))
      (mapcar #'first (reverse *rred-log*)))
    (:handler :hook))

  ;; The switch is off while the hook runs, so an error inside the hook
  ;; unwinds instead of opening a debugger inside the debugger.
  (deftest rred-switch-off-inside-hook
    (let ((*rred-log* nil))
      (catch 'rred-out
        (rred-with-switch
          (let ((*debugger-hook*
                  (lambda (c h)
                    (declare (ignore c h))
                    (push :outer *rred-log*)
                    (throw 'rred-out
                      (list (dotnet:static "DotCL.ConditionSystem"
                                           "UnhandledErrorsEnterDebugger"))))))
            (car 1)))))
    (nil)))

;;; A handler that takes the error still takes it: the debugger is for errors
;;; nobody handles.
(deftest rred-handler-case-still-wins
  (let ((*rred-log* nil))
    (list (rred-with-switch
            (let ((*debugger-hook* #'rred-hook))
              (handler-case (car 1) (type-error () :caught))))
          *rred-log*))
  (:caught nil))

;;; And the switch is off by default, so a script is unchanged.
(deftest rred-switch-off-by-default
  (dotnet:static "DotCL.ConditionSystem" "UnhandledErrorsEnterDebugger")
  nil)

;;; Compiling a call to a .NET type that does not exist yet (one a later
;;; DEFINE-CLASS will make) is a guess the compiler makes, not an error in the
;;; user's code. It used to signal and catch its own error, which a
;;; HANDLER-BIND around the compilation saw, and which at the REPL would now
;;; open the debugger on a form that is fine.
(deftest-compiled-only rred-compile-dotnet-guess-signals-nothing
  (let ((seen '()))
    (handler-bind ((error (lambda (c) (push (type-of c) seen))))
      (compile nil '(lambda (x) (dotnet:invoke (the (dotnet "RredNoSuch.Type") x) "Frob"))))
    seen)
  nil)

;;; -- A real REPL -------------------------------------------------------------

(defvar *rred-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun rred-image ()
  "This process's --core, if it was started with one (see repl-history-variables)."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rred-repl (lines)
  "Feed LINES to a REPL in another process; return its output and error output
as one string."
  (let ((script (merge-pathnames "session.lisp"
                                 (uiop:ensure-directory-pathname
                                  (concatenate 'string
                                               (regression-temp-dir)
                                               "/dotcl-repl-runtime-error-debugger-test/")))))
    (ensure-directories-exist script)
    (with-open-file (out script :direction :output :if-exists :supersede)
      (dolist (line lines)
        (write-string line out)
        (terpri out)))
    (multiple-value-bind (out err)
        (ignore-errors
         (uiop:run-program (append (list *rred-exe*) (rred-image) (list "repl"))
                           :input script
                           :output :string
                           :error-output :string
                           :ignore-error-status t))
      (concatenate 'string (or out "") (or err "")))))

;;; The unfinished form comes first: its reader signals END-OF-FILE, which must
;;; stay a request for more input rather than become a debugger.
(defvar *rred-transcript*
  (rred-repl '("(list 1"
               "2)"
               "(car 1)")))

(deftest rred-repl-unfinished-form-still-continues
  (and (search "(1 2)" *rred-transcript*) t)
  t)

(deftest rred-repl-runtime-error-enters-debugger
  (and (search "Debugger entered on TYPE-ERROR" *rred-transcript*) t)
  t)
