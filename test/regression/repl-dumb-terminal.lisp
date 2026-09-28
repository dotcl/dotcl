;;; A terminal that takes no escape sequences (TERM=dumb: Emacs's shell and
;;; comint buffers) gets plain line input, not the line editor: the editor draws
;;; with escape sequences, which such a terminal shows as text. The decision is
;;; the runtime's, one pure function, which the editor's menus ask as well.
;;;
;;; What reaches a real terminal is checked by hand on a pty; here the decision
;;; is checked as a table, and its effect on a child REPL reading a pipe, where
;;; --readline forces the editor on and TERM=dumb has to win over it.

(require "asdf")
(require "dotcl-repl")

(defun rdt-decide (pref term input output)
  (and (dotcl::%repl-line-editing-decision pref term input output) t))

;;; auto: on only when input and output are both a terminal.
(deftest rdt-auto
  (list (rdt-decide :auto "xterm-256color" t t)
        (rdt-decide :auto "xterm" nil t)
        (rdt-decide :auto "xterm" t nil)
        (rdt-decide :auto nil t t))
  (t nil nil t))

;;; --readline and --no-readline do not look at the streams.
(deftest rdt-forced
  (list (rdt-decide t "xterm" nil nil)
        (rdt-decide nil "xterm" t t))
  (t nil))

;;; TERM=dumb wins over everything, --readline included, as it does for colour.
(deftest rdt-dumb
  (list (rdt-decide :auto "dumb" t t)
        (rdt-decide t "dumb" t t)
        (rdt-decide nil "dumb" t t))
  (nil nil nil))

;;; The menus ask the same question.
(deftest rdt-menu-follows
  (list (dotcl-repl::menu-usable-p :term "dumb" :output-redirected nil
                                   :input-redirected nil)
        (dotcl-repl::menu-usable-p :term "xterm" :output-redirected nil
                                   :input-redirected nil))
  (nil t))

;;; -- A child REPL -------------------------------------------------------------

(defvar *rdt-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rdt-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-repl-dumb-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rdt-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rdt-getenv (name)
  (dotnet:static "System.Environment" "GetEnvironmentVariable" name))

(defun rdt-setenv (name value)
  (dotnet:static "System.Environment" "SetEnvironmentVariable" name value))

(defun rdt-run (flags lines term)
  "Feed LINES to `dotcl FLAGS repl` on a pipe with TERM set (NIL unsets) for the
child to inherit, and put TERM back afterwards. Returns standard output and
standard error."
  (let ((input (concatenate 'string *rdt-dir* "input.lisp"))
        (old-term (rdt-getenv "TERM")))
    (with-open-file (out input :direction :output :if-exists :supersede)
      (dolist (line lines)
        (write-string line out)
        (terpri out)))
    (unwind-protect
         (progn
           (rdt-setenv "TERM" term)
           (multiple-value-bind (out err)
               (ignore-errors
                (uiop:run-program (append (list *rdt-exe*) (rdt-image)
                                          (list "--no-init") flags (list "repl"))
                                  :input (pathname input)
                                  :output :string
                                  :error-output :string
                                  :ignore-error-status t))
             (values (or out "") (or err ""))))
      (rdt-setenv "TERM" old-term))))

(defun rdt-has-escape (s) (and (position #\Escape s) t))

;;; With --readline on a pipe the editor is on and cannot read keys from a pipe,
;;; so it says so and falls back. That is what makes "the editor was on" visible
;;; without a terminal.
(deftest rdt-readline-forced-xterm
  (multiple-value-bind (out err) (rdt-run '("--readline") '("(+ 1 2)") "xterm")
    (declare (ignore out))
    (and (search "readline failed" err) t))
  t)

;;; Under TERM=dumb the same command line never turns the editor on: no note, no
;;; escape sequence even with --color=always, and plain line input answers.
(deftest rdt-readline-forced-dumb
  (multiple-value-bind (out err)
      (rdt-run '("--readline" "--color=always") '("(+ 1 2)") "dumb")
    (list (and (search "readline failed" err) t)
          (rdt-has-escape out) (rdt-has-escape err)
          (and (search "CL-USER> 3" out) t)))
  (nil nil nil t))

;;; An editor turned on before the REPL starts (by an init file, here by --load)
;;; is turned off again under TERM=dumb: the init file serves every terminal.
(defvar *rdt-enable-file*
  (let ((file (concatenate 'string *rdt-dir* "enable.lisp")))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "(require \"dotcl-repl\")" out)
      (write-line "(dotcl-repl:enable)" out))
    file))

(deftest rdt-enabled-by-load-dumb
  (multiple-value-bind (out err)
      (rdt-run (list "--load" *rdt-enable-file*) '("(+ 1 2)") "dumb")
    (list (and (search "readline failed" err) t)
          (rdt-has-escape out)
          (and (search "CL-USER> 3" out) t)))
  (nil nil t))

;;; The same with another TERM keeps the editor the file turned on.
(deftest rdt-enabled-by-load-xterm
  (multiple-value-bind (out err)
      (rdt-run (list "--load" *rdt-enable-file*) '("(+ 1 2)") "xterm")
    (declare (ignore out))
    (and (search "readline failed" err) t))
  t)
