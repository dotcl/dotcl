;;; `dotcl pack --help` prints pack's own help instead of the whole-program
;;; help. Before this fix, --help was handled once, before the subcommand was
;;; even looked at, so `dotcl pack --help` printed the same wall of text as
;;; plain `dotcl --help` and the pack options stayed buried in one paragraph
;;; of it.

(require "asdf")

(defvar *ph-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *ph-core*
  (regression-child-core))

(defun ph-run (&rest args)
  "Run this dotcl build with ARGS. Returns (exit-code stdout stderr)."
  (dotcl:run-process *ph-exe* (append (list "--asm" *ph-core*) args)))

(defun ph-says (result text)
  (and (search text (concatenate 'string (second result) (third result))) t))

;;; `pack --help` names the pack subcommand's own options.
(deftest-emitting-only pack-help.shows-pack-usage
  (let ((r (ph-run "pack" "--help")))
    (list (first r)
          (ph-says r "dotcl pack --system <name>")
          (ph-says r "--dotcl-version <ver>")
          (ph-says r "Which dotcl version in --from to build on")))
  (0 t t t))

;;; It does not fall through to the whole-program help.
(deftest-emitting-only pack-help.not-the-general-help
  (let ((r (ph-run "pack" "--help")))
    (ph-says r "dotcl repl                   Start a REPL"))
  nil)

;;; --help before the subcommand still finds it: `pack` is a bare token, not
;;; a global flag or its value, so it is what FirstNonGlobalArg reports.
(deftest-emitting-only pack-help.help-before-subcommand
  (let ((r (ph-run "--help" "pack")))
    (ph-says r "dotcl pack --system <name>"))
  t)

;;; Plain `dotcl --help` (no subcommand) is unaffected.
(deftest-emitting-only pack-help.plain-help-unaffected
  (let ((r (ph-run "--help")))
    (list (ph-says r "dotcl repl                   Start a REPL")
          (ph-says r "dotcl pack --system <name>")))
  (t nil))

;;; Option descriptions line up in one column, continuation lines included.
;;; A few options had their description one or two columns to the right of
;;; the others. Returns the lines whose description starts anywhere else.
(defun ph-description-column (line)
  "Column where LINE's description text starts, or NIL if it has none."
  (cond ((and (> (length line) 3) (string= "  -" line :end2 3))
         (let ((gap (search "  " line :start2 2)))
           (and gap (position #\Space line :start gap :test-not #'char=))))
        ((and (> (length line) 20) (every (lambda (c) (char= c #\Space))
                                          (subseq line 0 20)))
         (position #\Space line :test-not #'char=))))

(deftest-emitting-only pack-help.descriptions-aligned
  (let ((r (ph-run "pack" "--help")))
    (with-input-from-string (s (second r))
      (loop for line = (read-line s nil)
            while line
            for col = (ph-description-column line)
            when (and col (/= col 31))
              collect line)))
  nil)
