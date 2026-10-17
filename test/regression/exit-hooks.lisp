;;; DOTCL:*EXIT-HOOKS*, as SB-EXT:*EXIT-HOOKS*: functions of no arguments run
;;; when the process exits, through DOTCL:QUIT or at the end of the program.
;;; Libraries (exit-hooks) attached their own handler to the .NET ProcessExit
;;; event before.
;;;
;;; The child is another dotcl, so this works the same on every platform.

(defvar *exh-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *exh-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-exit-hooks-test/")))
    (ensure-directories-exist dir)
    dir))

(defun exh-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (or (loop for i below (1- count)
              when (equal (dotnet:invoke args "GetValue" i) "--core")
                return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))
        (list "--core" (regression-child-core)))))

(defun exh-run (name text)
  "Run TEXT as a script in a child dotcl. Return its exit code and output."
  (let ((path (concatenate 'string *exh-dir* name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    (destructuring-bind (code out err)
        (dotcl:run-process *exh-exe*
                           (append (exh-image) (list "--no-init" "--load" path)))
      (declare (ignore err))
      (list code (remove #\Return out)))))

(deftest exit-hooks.exported
  (multiple-value-bind (sym status) (find-symbol "*EXIT-HOOKS*" "DOTCL")
    (list (and sym t) status (symbol-value sym)))
  (t :external nil))

(deftest exit-hooks.normal-end
  (exh-run "normal.lisp"
           "(push (lambda () (write-line \"hook\")) dotcl:*exit-hooks*)
(write-line \"body\")
")
  (0 "body
hook
"))

(deftest exit-hooks.quit
  (exh-run "quit.lisp"
           "(push (lambda () (write-line \"hook\")) dotcl:*exit-hooks*)
(dotcl:quit 3)
(write-line \"not reached\")
")
  (3 "hook
"))

(deftest exit-hooks.order-and-errors
  ;; In list order; an error in one does not stop the others.
  (exh-run "order.lisp"
           "(setf dotcl:*exit-hooks*
      (list (lambda () (write-line \"first\"))
            (lambda () (error \"boom\"))
            (lambda () (write-line \"third\"))))
")
  (0 "first
third
"))
