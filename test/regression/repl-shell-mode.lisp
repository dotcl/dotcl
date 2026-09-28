;;; The one-line shell mode of the bundled REPL.
;;;
;;; A semicolon typed at the start of an empty line does not go into the line:
;;; it switches the prompt to sh> for that one line, which is then run by the
;;; shell, and the next prompt is the Lisp one again. Backspace on the empty
;;; line switches back without running anything. The semicolon costs nothing
;;; to take, because in Lisp it would only start a comment.
;;;
;;; The keystrokes need a console. Everything they decide is a function of the
;;; key and the state of the line, and is checked here; so is what the shell is
;;; given to run, on both platforms whichever this one is, and the running of
;;; it, with a command that means the same to sh and to cmd.

(require "dotcl-repl")

(defun rsm-mode (ch &key (empty t) pasting (commands t))
  (let ((mode (dotcl-repl::line-mode-for-key ch empty pasting commands)))
    (and mode (dotcl-repl::line-mode-name mode))))

(defvar *rsm-shell* (find :shell dotcl-repl::*line-modes*
                          :key #'dotcl-repl::line-mode-name))

;;; -- Entering the mode --------------------------------------------------------

(deftest rsm-semicolon-on-empty-line
  (rsm-mode #\;)
  :shell)

;;; Anywhere but the start of an empty line, a semicolon is a semicolon.
(deftest rsm-semicolon-after-text
  (rsm-mode #\; :empty nil)
  nil)

;;; A paste is text arriving from elsewhere: a pasted ;;; comment stays Lisp.
(deftest rsm-semicolon-in-paste
  (rsm-mode #\; :pasting t)
  nil)

;;; A continuation line is inside a form, where a semicolon starts a comment.
(deftest rsm-semicolon-on-continuation
  (rsm-mode #\; :commands nil)
  nil)

;;; ? and ] are symbol constituents in CL, so they are not taken.
(deftest rsm-other-characters
  (list (rsm-mode #\() (rsm-mode #\?) (rsm-mode #\]) (rsm-mode #\`) (rsm-mode #\a))
  (nil nil nil nil nil))

;;; The prompt is sh> and a blank, four columns whether painted or not.
(deftest rsm-prompt
  (let ((prompt (dotcl-repl::mode-prompt-string *rsm-shell*)))
    (list (dotcl-repl::strip-control-sequences prompt)
          (dotcl-repl::prompt-display-width prompt)))
  ("sh> " 4))

;;; -- What the shell is given --------------------------------------------------

(defun rsm-invocation (line &rest keys)
  (multiple-value-list (apply #'dotcl-repl::shell-invocation line keys)))

(deftest rsm-unix-shell-from-environment
  (rsm-invocation "ls -l" :windows nil :shell "/bin/zsh")
  ("/bin/zsh" ("-c" "ls -l")))

(deftest rsm-unix-shell-default
  (list (rsm-invocation "ls" :windows nil :shell nil)
        (rsm-invocation "ls" :windows nil :shell ""))
  (("/bin/sh" ("-c" "ls")) ("/bin/sh" ("-c" "ls"))))

;;; cmd takes one command line. /s with the whole line in quotes keeps the
;;; quotes inside it, and /d leaves out AutoRun.
(deftest rsm-windows-shell
  (list (rsm-invocation "dir \"a b\"" :windows t :comspec "C:\\Windows\\system32\\cmd.exe")
        (rsm-invocation "dir" :windows t :comspec nil))
  (("C:\\Windows\\system32\\cmd.exe" "/d /s /c \"dir \"a b\"\"")
   ("cmd.exe" "/d /s /c \"dir\"")))

;;; A line that starts with a semicolon, or has nothing in it, is not run: a
;;; pasted ;;; comment typed without bracketed paste lands here.
(deftest rsm-ignored-lines
  (mapcar #'dotcl-repl::shell-line-ignored-p '(";ls" ";;; comment" "" "   " "ls" " ls"))
  (t t t t nil nil))

;;; -- What the read loop is answered -------------------------------------------

;;; A shell line is run where it is read and the loop is answered with a blank
;;; line, which it skips; a Lisp line goes to the loop as it is.
(deftest rsm-answer
  (list (dotcl-repl::answer-line ";not run" *rsm-shell* nil)
        (dotcl-repl::answer-line "(+ 1 2)" nil nil)
        (dotcl-repl::answer-line "  (list" nil t))
  ("" "(+ 1 2)" "  (list"))

;;; -- Running a line -----------------------------------------------------------

(defun rsm-run (line)
  "Run LINE in shell mode. Returns the exit status and what was said on
*ERROR-OUTPUT*."
  (let* ((err (make-string-output-stream))
         (status (let ((*error-output* err))
                   (dotcl-repl::run-shell-line line))))
    (values status (get-output-stream-string err))))

;;; The exit status comes back, and one that is not zero is reported.
(deftest rsm-exit-status
  (multiple-value-bind (status said) (rsm-run "exit 3")
    (list status (and (search "exit status 3" said) t)))
  (3 t))

(deftest rsm-exit-zero-quiet
  (multiple-value-list (rsm-run "exit 0"))
  (0 ""))

(deftest rsm-ignored-not-run
  (multiple-value-list (rsm-run ";exit 3"))
  (nil ""))

;;; The shell runs in the REPL's directory, the one ,cd moves.
(deftest rsm-runs-in-current-directory
  (let* ((dir (concatenate 'string (regression-temp-dir) "/dotcl-repl-shell-mode/"))
         (old (namestring *default-pathname-defaults*))
         (old-process (dotnet:static "System.IO.Directory" "GetCurrentDirectory")))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (dotcl:chdir dir)
           (rsm-run "echo shell-ok> rsm-out.txt")
           (with-open-file (in (concatenate 'string dir "rsm-out.txt")
                               :if-does-not-exist nil)
             (and in (search "shell-ok" (read-line in nil "")) t)))
      (dotcl:chdir old-process)
      (setf *default-pathname-defaults* (pathname old))))
  t)

;;; A shell that cannot be started is reported and does not end anything.
(deftest rsm-missing-shell
  (let* ((err (make-string-output-stream))
         (status (let ((*error-output* err))
                   (dotcl-repl::run-shell "/no/such/shell-for-dotcl" '("-c" "ls")))))
    (list status (and (search "cannot run" (get-output-stream-string err)) t)))
  (nil t))

;;; -- ,help ---------------------------------------------------------------------

;;; ,help says how to enter the mode and how to leave it.
(deftest rsm-help-explains
  (let ((out (with-output-to-string (*standard-output*)
               (dotcl-repl:dispatch ",help"))))
    (list (and (search "sh>" out) t)
          (and (search "Backspace" out) t)
          (and (search " ;" out) t)))
  (t t t))
