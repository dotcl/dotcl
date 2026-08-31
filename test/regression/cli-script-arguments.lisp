;;; What a script was given, and nothing else.
;;;
;;; dotcl:command-line-arguments is shaped for uiop -- ("dotcl" "--" args...) --
;;; so the obvious (first (dotcl:command-line-arguments)) answers "dotcl". Every
;;; script author writes that once. dotcl:script-arguments is the plain list.
;;;
;;; `repl` is the one word after a script name that dotcl keeps for itself: it
;;; used to be removed from the command line and then dropped on the floor, so
;;; `dotcl app.lisp repl` ran the script, ignored the word and exited.

(defvar *csa-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *csa-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defvar *csa-dir*
  (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp")))

(defun %csa-script (name text)
  (let ((path (concatenate 'string *csa-dir* "/" name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

(defvar *csa-print-args*
  (%csa-script "dotcl-csa-print.lisp"
               "(format t \"~&ARGS=~s~%\" (dotcl:script-arguments))"))

(defun %csa-run (&rest args)
  (dotcl:run-process *csa-exe* (append (list "--core" *csa-core*) args)))

(defun %csa-out (result) (second result))

;;; The arguments, in order, with nothing else.
(deftest csa-plain-arguments
  (let ((r (%csa-run *csa-print-args* "one" "two")))
    (list (first r) (and (search "ARGS=(\"one\" \"two\")" (%csa-out r)) t)))
  (0 t))

;;; Dashes after the script name are the script's.
(deftest csa-dashes-pass-through
  (let ((r (%csa-run *csa-print-args* "--verbose" "-x")))
    (and (search "ARGS=(\"--verbose\" \"-x\")" (%csa-out r)) t))
  t)

;;; No script, no script arguments.
(deftest csa-empty-outside-a-script
  (let ((r (%csa-run "--eval" "(format t \"~&ARGS=~s~%\" (dotcl:script-arguments))")))
    (and (search "ARGS=NIL" (%csa-out r)) t))
  t)

;;; After a file has been named, `repl` is one of its arguments like any other
;;; word -- a script has to be able to receive every argument it is given.
(deftest csa-repl-after-a-script-is-an-argument
  (let ((r (%csa-run *csa-print-args* "repl")))
    (list (first r)
          (and (search "ARGS=(\"repl\")" (%csa-out r)) t)
          (search "CL-USER>" (%csa-out r))))
  (0 t nil))

;;; Before a file it is still the subcommand: the script runs, then the REPL.
;;; stdin is closed here, so the REPL starts and immediately reaches end of input.
(deftest csa-repl-before-a-script-is-the-subcommand
  (let ((r (%csa-run "repl" *csa-print-args*)))
    (list (and (search "ARGS=NIL" (%csa-out r)) t)
          (and (search "CL-USER>" (%csa-out r)) t)))
  (t t))

;;; And --load is the spelling for "run this, then leave me in the REPL".
(deftest csa-repl-after-load-stays
  (let ((r (%csa-run "--load" *csa-print-args* "repl")))
    (list (and (search "ARGS=NIL" (%csa-out r)) t)
          (and (search "CL-USER>" (%csa-out r)) t)))
  (t t))
