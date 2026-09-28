;;; What the REPL does with a transfer of control that happens while it is
;;; reading input through the line editor.
;;;
;;; Two things went wrong there. An interrupt (Ctrl+C) that arrived while the
;;; editor waited for a key entered the debugger, although what it means at an
;;; empty prompt is "drop what I typed". And leaving that debugger with ABORT
;;; unwound through the call to the editor, whose error handler took the
;;; restart for a broken terminal and turned the editor off for the rest of the
;;; session.
;;;
;;; Neither needs a console to reach. The REPL calls whatever function is
;;; installed as its line reader, so a child REPL is given one that requests an
;;; interrupt on its first call, invokes ABORT on its second, and reads plain
;;; lines from standard input after that. The interrupt is delivered by calling
;;; the safepoint directly rather than by spinning until one is reached, so the
;;; child cannot hang where no safepoint is compiled in. The reader returns
;;; what READ-LINE returns, two values, which is also a case the REPL has to
;;; take: interpreted, the two come back as one object, and the REPL used to
;;; read that object's printed form as the line.

(require "asdf")

(defvar *rii-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rii-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-repl-input-interrupt-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rii-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defvar *rii-reader-source*
  "(defvar *rii-calls* 0)
(dotcl::%set-repl-readline-hook
 (lambda (prompt)
   (incf *rii-calls*)
   (write-string prompt)
   (finish-output)
   (case *rii-calls*
     (1 (dotnet:static \"DotCL.ConditionSystem\" \"RequestInterrupt\")
        (dotnet:static \"DotCL.ConditionSystem\" \"PollInterrupt\")
        \"(quote :interrupt-not-delivered)\")
     (2 (invoke-restart (find-restart 'abort)))
     (t (read-line *standard-input* nil nil)))))
")

;;; A second reader stands in for the editor's Ctrl+C on a continuation line:
;;; the first call leaves a form open, so the REPL holds that line and prompts
;;; for more; the second signals the interrupt condition, which is how the
;;; editor tells the REPL to drop the lines it holds.
(defvar *rii-continuation-source*
  "(defvar *rii-calls* 0)
(dotcl::%set-repl-readline-hook
 (lambda (prompt)
   (incf *rii-calls*)
   (write-string prompt)
   (finish-output)
   (case *rii-calls*
     (1 \"(list :dropped\")
     (2 (signal (make-condition
                 (find-symbol \"INTERACTIVE-INTERRUPT\" \"DOTCL-INTERNAL\")))
        \":not-dropped)\")
     (t (read-line *standard-input* nil nil)))))
")

(defun rii-run (lines &optional (source *rii-reader-source*))
  "Run a REPL in another process on the line reader SOURCE, feeding it LINES.
Returns standard output and standard error as two values."
  (let ((setup (concatenate 'string *rii-dir* "reader.lisp"))
        (input (concatenate 'string *rii-dir* "input.lisp")))
    (with-open-file (out setup :direction :output :if-exists :supersede)
      (write-string source out))
    (with-open-file (out input :direction :output :if-exists :supersede)
      (dolist (line lines)
        (write-string line out)
        (terpri out)))
    (multiple-value-bind (out err)
        (ignore-errors
         (uiop:run-program (append (list *rii-exe*) (rii-image)
                                   (list "--no-init" "--load" setup "repl"))
                           :input (pathname input)
                           :output :string
                           :error-output :string
                           :ignore-error-status t))
      (values (or out "") (or err "")))))

(defvar *rii-out*)
(defvar *rii-err*)

(multiple-value-setq (*rii-out* *rii-err*)
  (rii-run '("(+ 1 2)" "(list :calls *rii-calls*)")))

;;; The session ran to the end, reading through the installed reader: the
;;; interrupt and the ABORT each cost one call, and the two lines two more.
(deftest rii-session-ran
  (and (search "(:CALLS 4)" *rii-out*) t)
  t)

(deftest rii-form-after-interrupt-evaluates
  (and (search "CL-USER> 3" *rii-out*) t)
  t)

;;; The interrupt at the prompt is shown the way the editor's own Ctrl+C shows
;;; it, and does not open the debugger.
(deftest rii-interrupt-at-prompt-is-caret-c
  (and (search "^C" *rii-out*) t)
  t)

(deftest rii-interrupt-at-prompt-no-debugger
  (or (search "Debugger entered" *rii-out*)
      (search "Debugger entered" *rii-err*))
  nil)

;;; ABORT from inside the reader goes back to the prompt and leaves the reader
;;; installed: it is not reported as the editor failing.
(deftest rii-abort-keeps-the-reader
  (search "readline failed" *rii-err*)
  nil)

;;; Ctrl+C on a continuation line drops the whole form, the lines the REPL
;;; already holds included, and the next form is read on its own.
(multiple-value-setq (*rii-out* *rii-err*)
  (rii-run '("(list :kept)") *rii-continuation-source*))

(deftest rii-continuation-interrupt-drops-held-lines
  (list (and (search "(:KEPT)" *rii-out*) t)
        (and (search "DROPPED" *rii-out*) t)
        (and (search "NOT-DROPPED" *rii-out*) t))
  (t nil nil))
