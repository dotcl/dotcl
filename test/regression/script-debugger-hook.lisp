;;; In a script, an error the runtime signals itself (CAR of a non-list, an
;;; undefined function, an unbound variable, division by zero) that nothing
;;; handles goes to INVOKE-DEBUGGER, which runs *DEBUGGER-HOOK* first, as it
;;; does for ERROR. With no hook of the script's own, the run still ends with
;;; the one-line "file:line: message". Expected output taken from SBCL 2.6.8
;;; (--non-interactive, SB-EXT:*INVOKE-DEBUGGER-HOOK* set to NIL).

(defvar *sdh-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *sdh-core* (regression-child-core))

(defun %sdh-write (name text)
  (let ((path (concatenate 'string (regression-temp-dir) "/" name)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

(defparameter *sdh-hook-script* "(defvar *v* 3)
(defvar *z* 0)
(defun try (name type thunk)
  (block nil
    (let ((*debugger-hook*
            (lambda (c h)
              (declare (ignore h))
              (format t \"~a:~a \" name (if (typep c type) \"HOOK\" (type-of c)))
              (return nil))))
      (funcall thunk)
      (format t \"~a:NO-ERROR \" name))))
(try \"car\" 'type-error (lambda () (car *v*)))
(try \"undefined\" 'undefined-function (lambda () (funcall (intern \"NO-SUCH-FN-X\") 1)))
(try \"unbound\" 'unbound-variable (lambda () (symbol-value (intern \"NO-SUCH-VAR-X\"))))
(try \"div0\" 'division-by-zero (lambda () (/ 1 *z*)))
(format t \"END~%\")
")

(defparameter *sdh-expected* "car:HOOK undefined:HOOK unbound:HOOK div0:HOOK END")

;;; (exit code, stdout trimmed)
(defun %sdh-run (args)
  (let ((r (dotcl:run-process *sdh-exe* (list* "--core" *sdh-core* args))))
    (list (first r) (string-trim '(#\Space #\Newline #\Return) (second r)))))

(deftest script-debugger-hook.script-file
  (%sdh-run (list (%sdh-write "sdh-hook.lisp" *sdh-hook-script*)))
  (0 #.*sdh-expected*))

(deftest script-debugger-hook.load-flag
  (%sdh-run (list "--load" (%sdh-write "sdh-hook-load.lisp" *sdh-hook-script*)))
  (0 #.*sdh-expected*))

;;; A HANDLER-BIND that declines does not stop the hook, including for a raw
;;; .NET exception (code compiled at safety 0 given the wrong type).
(deftest script-debugger-hook.declining-handler-bind
  (%sdh-run (list (%sdh-write "sdh-hb.lisp" "(defvar *v* 3)
(defun f (x) (declare (optimize (safety 0)) (fixnum x)) (+ x 1))
(defun try (name thunk)
  (block nil
    (let ((*debugger-hook* (lambda (c h) (declare (ignore c h))
                             (format t \"~a:HOOK \" name) (return nil))))
      (handler-bind ((warning #'print))
        (funcall thunk))
      (format t \"~a:NO-ERROR \" name))))
(try \"car\" (lambda () (car *v*)))
(try \"cast\" (lambda () (f \"a\")))
(format t \"END~%\")
")))
  (0 "car:HOOK cast:HOOK END"))

;;; No hook: the first error ends the run with its source location, and does
;;; not open the debugger.
(deftest script-debugger-hook.no-hook-reports-location
  (let* ((path (%sdh-write "sdh-nohook.lisp" "(defvar *v* 3)
(car *v*)
(print :after)
"))
         (r (dotcl:run-process *sdh-exe* (list "--core" *sdh-core* path)))
         (err (third r)))
    (list (first r)
          (and (search "sdh-nohook.lisp:2: CAR: not a list: 3" err) t)
          (and (search "AFTER" (second r)) t)
          (and (search "Debugger" err) t)))
  (1 t nil nil))

;;; An undefined function or an unbound variable that nothing handles gets the
;;; same "file:line: message" report as the other errors the runtime raises,
;;; not a bare "UNDEFINED-FUNCTION: ..." with no place.
(deftest script-debugger-hook.undefined-names-report-location
  (let* ((fpath (%sdh-write "sdh-undef-fn.lisp" "(print 1)
(no-such-fn-here 1)
"))
         (vpath (%sdh-write "sdh-undef-var.lisp" "(print 1)
(print no-such-var-here)
"))
         (fr (dotcl:run-process *sdh-exe* (list "--core" *sdh-core* fpath)))
         (vr (dotcl:run-process *sdh-exe* (list "--core" *sdh-core* vpath))))
    (list (first fr)
          (and (search "sdh-undef-fn.lisp:2: Undefined function: NO-SUCH-FN-HERE" (third fr)) t)
          (first vr)
          (and (search "sdh-undef-var.lisp:2: Unbound variable: NO-SUCH-VAR-HERE" (third vr)) t)))
  (1 t 1 t))

;;; --eval that ends in an exit reports an unhandled error as one line and exits 1,
;;; as a script does, without the debugger banner and restart list.
(deftest script-debugger-hook.eval-reports-one-line
  (let* ((r (dotcl:run-process *sdh-exe*
                               (list "--core" *sdh-core* "--eval" "(error \"boom-eval\")")))
         (err (third r)))
    (list (first r)
          (and (search "SIMPLE-ERROR: boom-eval" err) t)
          (and (search "Debugger entered" err) nil)
          (and (search "Available restarts" err) nil)))
  (1 t nil nil))
