;;; An invocation whose arguments were not understood must say so, not look like
;;; a normal start.
;;;
;;; Two ways that used to fail silently, both ending in the same place: the
;;; argument was dropped, no action was recorded, and the empty action list took
;;; the process into the REPL -- which reads exactly like a successful `dotcl`.
;;;
;;;   dotcl --evla '(princ 1)'   typo: the flag was discarded
;;;   dotcl --eval               value missing: the option was discarded
;;;
;;; The REPL is now entered only when asked for (`repl`), so an empty action list
;;; is itself reported rather than being a way in.
;;;
;;; Arguments after a script name belong to the script and are not options: that
;;; is checked here too, because the obvious implementation of "reject unknown
;;; options" would eat them.

(defvar *cli-ae-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-ae-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun %cli-ae-run (args) (dotcl:run-process *cli-ae-exe* args))

(defun %cli-ae-core-args (&rest args)
  (append (list "--asm" *cli-ae-core*) args))

;;; --- unknown options -------------------------------------------------------

(deftest cli-argument-errors.unknown-long-option
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--evla" "(princ 1)")))
         (err (third r)))
    (list (first r) (and (search "unknown option" err) t)))
  (2 t))

(deftest cli-argument-errors.unknown-short-option
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "-x")))
         (err (third r)))
    (list (first r) (and (search "unknown option" err) t)))
  (2 t))

;;; --- value-taking options with no value ------------------------------------

(deftest cli-argument-errors.eval-without-form
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--eval")))
         (err (third r)))
    (list (first r) (and (search "requires an argument" err) t)))
  (2 t))

(deftest cli-argument-errors.load-without-file
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--load")))
         (err (third r)))
    (list (first r) (and (search "requires an argument" err) t)))
  (2 t))

;;; --- no action at all ------------------------------------------------------

(deftest cli-argument-errors.no-arguments-reports-instead-of-repl
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core*)))
         (err (third r)))
    (list (first r) (and (search "nothing to do" err) t)))
  (2 t))

;;; --- what must keep working ------------------------------------------------

(deftest cli-argument-errors.eval-still-runs
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--eval" "(princ :ok)"))))
    (list (first r) (and (search "OK" (second r)) t)))
  (0 t))

;;; Flags after a script name are the script's arguments, not dotcl's.
(deftest cli-argument-errors.script-arguments-pass-through
  (let* ((path (concatenate 'string
                            (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                    (dotcl:getenv "TEMP")
                                                    "/tmp"))
                            "/dotcl-cli-ae.lisp")))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(format t \"~&ARGS=~s~%\" (length (dotcl:command-line-arguments)))" s))
    (let ((r (%cli-ae-run (list "--core" *cli-ae-core* path "--foo" "-x"))))
      (list (first r) (and (search "ARGS=" (second r)) t))))
  (0 t))
