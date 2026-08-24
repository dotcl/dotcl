;;; A file named on the command line that is not there must be reported, not
;;; thrown.
;;;
;;; Three of the four ways to name one crashed out of Main with a .NET stack
;;; trace: `--core X`, `--asm X` (both from File.OpenRead) and `--load X` in the
;;; ordinary path (a LOAD error with no source location, which the action loop
;;; did not catch). The fourth, a positional script file, printed one line and
;;; exited 1 -- these now do the same. That mismatch is the point of the tests:
;;; the same mistake spelled two ways gave a one-liner or a stack trace
;;; depending on where it appeared.
;;;
;;; The behaviour under test is argument handling in main, so each case runs
;;; this executable again. `compiler/cil-out.sil` is used as the core: RunCore
;;; takes SIL text as well as a FASL, and the suite always has that file.

(defvar *cli-mf-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-mf-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defvar *cli-mf-absent* "/no/such/directory/no-such-file.lisp")

(defun %cli-mf-run (args) (dotcl:run-process *cli-mf-exe* args))

;;; (exit-code, message present, no stack trace) for each spelling.

(deftest cli-missing-file.core
  (let* ((result (%cli-mf-run (list "--core" *cli-mf-absent* "--eval" "(princ :never)")))
         (err (third result)))
    (list (first result)
          (and (search "core file not found" err) t)
          (and (search "Unhandled exception" err) t)))
  (2 t nil))

(deftest cli-missing-file.asm
  (let* ((result (%cli-mf-run (list "--asm" *cli-mf-absent*)))
         (err (third result)))
    (list (first result)
          (and (search "core file not found" err) t)
          (and (search "Unhandled exception" err) t)))
  (2 t nil))

(deftest cli-missing-file.load
  (let* ((result (%cli-mf-run (list "--core" *cli-mf-core*
                                    "--load" *cli-mf-absent*)))
         (err (third result)))
    (list (first result)
          (and (search "file not found" err) t)
          (and (search "Unhandled exception" err) t)))
  (1 t nil))

;;; The one that was already right, kept as the reference the others match.
(deftest cli-missing-file.positional-script
  (let* ((result (%cli-mf-run (list "--core" *cli-mf-core* *cli-mf-absent*)))
         (err (third result)))
    (list (first result)
          (and (search "file not found" err) t)
          (and (search "Unhandled exception" err) t)))
  (1 t nil))

;;; A present file still loads through the same path.
(deftest cli-missing-file.load-present-file-still-works
  (let* ((path (concatenate 'string
                            (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                    (dotcl:getenv "TEMP")
                                                    "/tmp"))
                            "/dotcl-cli-mf.lisp")))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(format t \"~&LOADED=~s~%\" :yes)" s))
    (let* ((result (%cli-mf-run (list "--core" *cli-mf-core*
                                      "--load" path
                                      "--eval" "(dotcl:quit 0)")))
           (out (second result)))
      (list (first result) (and (search "LOADED=:YES" out) t))))
  (0 t))
