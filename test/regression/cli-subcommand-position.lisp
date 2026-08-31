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
