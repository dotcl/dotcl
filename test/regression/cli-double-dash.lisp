;;; `--` ends dotcl's own options: what follows is the program's arguments,
;;; seen through dotcl:script-arguments and uiop:command-line-arguments, and
;;; never run as a script. common-lisp-jupyter writes its kernel argv this way:
;;; <lisp> --eval <form> -- <connection-file>.

(defvar *cli-dd-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-dd-core*
  (regression-child-core))

(defvar *cli-dd-dir*
  (regression-temp-dir))

(defvar *cli-dd-print-args*
  (let ((path (concatenate 'string *cli-dd-dir* "/dotcl-cli-dd-print.lisp")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(format t \"~&ARGS=~s~%\" (dotcl:script-arguments))" out))
    path))

(defun %cli-dd-run (&rest args)
  (dotcl:run-process *cli-dd-exe* (append (list "--core" *cli-dd-core*) args)))

(defun %cli-dd-has (result text)
  (and (search text (second result)) t))

(defparameter *cli-dd-print* "(format t \"~&ARGS=~s~%\" (dotcl:script-arguments))")

(deftest cli-dd-after-eval
  (let ((r (%cli-dd-run "--eval" *cli-dd-print* "--" "a" "b")))
    (list (first r) (%cli-dd-has r "ARGS=(\"a\" \"b\")")))
  (0 t))

;;; Options after `--` are arguments, not dotcl's: no help, no unknown-option exit.
(deftest cli-dd-options-after-are-arguments
  (let ((r (%cli-dd-run "--eval" *cli-dd-print* "--" "--help" "--eval" "-x")))
    (list (first r) (%cli-dd-has r "ARGS=(\"--help\" \"--eval\" \"-x\")")))
  (0 t))

;;; A file named after `--` is data, not a script to run.
(deftest cli-dd-file-after-is-not-run
  (let ((r (%cli-dd-run "--eval" *cli-dd-print* "--" *cli-dd-print-args*)))
    (list (first r)
          (%cli-dd-has r (format nil "ARGS=(~s)" *cli-dd-print-args*))
          (= (search "ARGS=" (second r)) (search "ARGS=" (second r) :from-end t))))
  (0 t t))

(deftest cli-dd-empty-after
  (let ((r (%cli-dd-run "--eval" *cli-dd-print* "--")))
    (list (first r) (%cli-dd-has r "ARGS=NIL")))
  (0 t))

;;; After a script name `--` is the script's own argument, as before.
(deftest cli-dd-after-script-is-the-scripts
  (let ((r (%cli-dd-run *cli-dd-print-args* "--" "a")))
    (list (first r) (%cli-dd-has r "ARGS=(\"--\" \"a\")")))
  (0 t))

(deftest cli-dd-uiop-command-line-arguments
  (let ((r (%cli-dd-run "--eval" "(require \"asdf\")"
                        "--eval" "(format t \"~&UIOP=~s~%\" (uiop:command-line-arguments))"
                        "--" "/tmp/conn.json")))
    (list (first r) (%cli-dd-has r "UIOP=(\"/tmp/conn.json\")")))
  (0 t))
