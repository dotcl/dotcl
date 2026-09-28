;;; A subcommand has to be found after the global options, not only at argv[0].
;;;
;;; `clean` was recognised as args[0] alone, so `dotcl --core x.core clean` fell
;;; through to the ordinary path, where `clean` is not a flag -- it was taken as
;;; a script name and reported as "LOAD: file not found: .../clean". --help,
;;; --version and pack were already position-independent; this was the odd one
;;; out. The scan steps over a global's value as well as the global itself, and
;;; stops at anything unrecognised so an unknown flag behaves exactly as before.
;;;
;;; Every case here passes --dry-run: the point is which code path runs, and the
;;; suite must not delete the cache of the machine it runs on.

(defvar *cli-sp-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-sp-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun %cli-sp-run (args) (dotcl:run-process *cli-sp-exe* args))

(defun %cli-sp-cleaned-p (result)
  "True when the child actually ran the clean subcommand (dry run)."
  (and (zerop (first result))
       (search "dotcl clean:" (concatenate 'string (second result) (third result)))
       t))

;;; The shape that was broken: a global option in front of the subcommand.
(deftest cli-subcommand-position.core-before-clean
  (%cli-sp-cleaned-p (%cli-sp-run (list "--core" *cli-sp-core* "clean" "--dry-run")))
  t)

;;; A valueless global in front.
(deftest cli-subcommand-position.flag-before-clean
  (%cli-sp-cleaned-p (%cli-sp-run (list "--no-init" "clean" "--dry-run")))
  t)

;;; Two globals, one of them taking a value.
(deftest cli-subcommand-position.two-globals-before-clean
  (%cli-sp-cleaned-p (%cli-sp-run (list "--no-init" "--core" *cli-sp-core*
                                        "clean" "--dry-run")))
  t)

;;; Still works where it always did.
(deftest cli-subcommand-position.plain-clean
  (%cli-sp-cleaned-p (%cli-sp-run (list "clean" "--dry-run")))
  t)

;;; clean's own option checking still applies after the scan.
(deftest cli-subcommand-position.unknown-clean-option-still-rejected
  (let* ((result (%cli-sp-run (list "--no-init" "clean" "--bogus")))
         (err (third result)))
    (list (first result) (and (search "unknown option" err) t)))
  (2 t))

;;; An unknown flag stops the scan, so the invocation keeps its old meaning
;;; rather than silently gaining a subcommand.
(deftest cli-subcommand-position.unknown-flag-stops-the-scan
  (let* ((result (%cli-sp-run (list "--bogus-flag" "clean" "--dry-run")))
         (out (concatenate 'string (second result) (third result))))
    (and (search "dotcl clean:" out) t))
  nil)

;;; --asm ran its own early path: boot the core, then LOAD every token that is
;;; not --eval / --load / --asd-search-path, and return. A subcommand never
;;; reached the code that parses subcommands, so `--asm x.sil repl` reported
;;; "LOAD: file not found: .../repl" (same for build and pack) while the same
;;; line with --core worked. The REPL is started with stdin at EOF: it prints
;;; its banner and ends normally.
(defun %cli-sp-repl-banner (args)
  "First line the child writes to stdout, and its exit code."
  (let* ((p (dotcl:launch-process *cli-sp-exe* args :input nil :error nil))
         (line (read-line (dotcl:process-output p) nil "")))
    (loop while (read-line (dotcl:process-output p) nil nil))
    (list (and (search "dotcl REPL." line) t) (dotcl:process-wait p))))

(deftest cli-subcommand-position.asm-then-repl
  (%cli-sp-repl-banner (list "--asm" *cli-sp-core* "repl"))
  (t 0))

(deftest cli-subcommand-position.repl-then-asm
  (%cli-sp-repl-banner (list "repl" "--asm" *cli-sp-core*))
  (t 0))

;;; build and pack reach their own argument checks instead of LOAD. Their
;;; missing-argument messages are the evidence that the subcommand ran.
(deftest cli-subcommand-position.asm-then-build
  (let ((result (%cli-sp-run (list "--asm" *cli-sp-core* "build"))))
    (list (first result) (and (search "build: missing <asd> path" (third result)) t)))
  (2 t))

(deftest cli-subcommand-position.asm-then-pack
  (let ((result (%cli-sp-run (list "--asm" *cli-sp-core* "pack"))))
    (list (and (search "pack: missing required option" (third result)) t)
          (and (search "LOAD" (third result)) t)))
  (t nil))

;;; Without a subcommand the --asm path is used exactly as before.
(deftest cli-subcommand-position.asm-eval-unchanged
  (let ((result (%cli-sp-run (list "--asm" *cli-sp-core*
                                   "--eval" "(princ :asm-eval-ok)"))))
    (list (first result) (and (search "ASM-EVAL-OK" (second result)) t)))
  (0 t))
