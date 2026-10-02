;;; Colour in the REPL: the prompt, the value a form returned, warnings and
;;; errors, each painted with its own escape sequence, and none of it when the
;;; output is not a terminal or when NO_COLOR or TERM=dumb says not to.
;;;
;;; Two halves. Whether to paint is a pure function of the mode, the two
;;; environment variables and whether the stream is a terminal, so the whole
;;; table is checked here without one. What gets written is checked on a child
;;; REPL reading from a pipe: --color=always is the one mode that paints a pipe,
;;; which is what lets the escape sequences be read back without a terminal.

(require "asdf")
(require "dotcl-repl")

;;; Escape spelled out, so an expected value is a readable literal.
(defun rcl-show (s)
  (with-output-to-string (out)
    (loop for ch across s
          do (if (char= ch #\Escape)
                 (write-string "<ESC>" out)
                 (write-char ch out)))))

;;; -- The decision -------------------------------------------------------------

(defun rcl-decide (mode no-color term terminal)
  (and (dotcl::%repl-color-decision mode no-color term terminal) t))

;;; auto paints a terminal and nothing else.
(deftest rcl-auto-terminal
  (list (rcl-decide "auto" nil "xterm" t)
        (rcl-decide "auto" nil "xterm" nil)
        (rcl-decide "auto" nil nil t))
  (t nil t))

;;; always and never do not look at the stream.
(deftest rcl-always-never
  (list (rcl-decide "always" nil "xterm" nil)
        (rcl-decide "always" nil "xterm" t)
        (rcl-decide "never" nil "xterm" t)
        (rcl-decide "never" nil "xterm" nil))
  (t t nil nil))

;;; NO_COLOR wins over every mode, always included.
(deftest rcl-no-color-wins
  (list (rcl-decide "auto" "1" "xterm" t)
        (rcl-decide "always" "1" "xterm" t)
        (rcl-decide "always" "anything" "xterm" nil))
  (nil nil nil))

;;; An empty NO_COLOR is not set, as no-color.org defines it.
(deftest rcl-no-color-empty
  (list (rcl-decide "auto" "" "xterm" t)
        (rcl-decide "always" "" "xterm" nil))
  (t t))

;;; TERM=dumb wins over every mode too. Other terminal names do not matter.
(deftest rcl-term-dumb-wins
  (list (rcl-decide "auto" nil "dumb" t)
        (rcl-decide "always" nil "dumb" t)
        (rcl-decide "always" nil "vt100" nil)
        (rcl-decide "auto" nil "xterm-256color" t))
  (nil nil t t))

(deftest rcl-bad-mode-signals
  (and (nth-value 1 (ignore-errors (rcl-decide "sometimes" nil nil t))) t)
  t)

;;; -- The escape sequences -----------------------------------------------------

;;; Each role is its sequence, the text, and a reset.
(deftest rcl-paint-roles
  (mapcar (lambda (role) (rcl-show (dotcl::%repl-paint role "x" t)))
          '(:prompt :debugger :shell :result :warning :error))
  ("<ESC>[1;32mx<ESC>[0m"
   "<ESC>[1;31mx<ESC>[0m"
   "<ESC>[1;35mx<ESC>[0m"
   "<ESC>[36mx<ESC>[0m"
   "<ESC>[33mx<ESC>[0m"
   "<ESC>[31mx<ESC>[0m"))

;;; Painting nothing leaves nothing, and a painter told not to paint returns
;;; the text as it is.
(deftest rcl-paint-off-and-empty
  (list (dotcl::%repl-paint :result "x" nil)
        (dotcl::%repl-paint :result "" t))
  ("x" ""))

;;; Outside a REPL nothing is painted: this process is not one.
(deftest rcl-paint-off-outside-repl
  (list (dotcl::%repl-paint :result "x")
        (dotcl::%repl-paint :error "x" :error)
        (dotcl::%repl-paint :error "x" *error-output*))
  ("x" "x" "x"))

;;; A stream that is not the process's own is never painted, whatever the
;;; process's streams would be.
(deftest rcl-paint-string-stream
  (dotcl::%repl-paint :error "x" (make-string-output-stream))
  "x")

;;; -- The line editor and a painted prompt -------------------------------------

;;; The escape sequences take no columns, so the redraw lands after the visible
;;; prompt and not after its bytes.
(deftest rcl-prompt-width
  (list (dotcl-repl::prompt-display-width
         (concatenate 'string (dotcl::%repl-paint :prompt "CL-USER>" t) " "))
        (dotcl-repl::prompt-display-width "CL-USER> ")
        (dotcl-repl::prompt-display-width
         (format nil "~C[1;35msh>~C[0m " #\Escape #\Escape)))
  (9 9 4))

;;; A painted prompt is still the primary prompt, not a continuation.
(deftest rcl-painted-prompt-not-continuation
  (dotcl-repl::continuation-prompt-p (dotcl::%repl-paint :prompt "CL-USER>" t))
  nil)

;;; -- A child REPL -------------------------------------------------------------

(defvar *rcl-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rcl-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-repl-color-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rcl-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rcl-getenv (name)
  (dotnet:static "System.Environment" "GetEnvironmentVariable" name))

(defun rcl-setenv (name value)
  (dotnet:static "System.Environment" "SetEnvironmentVariable" name value))

(defun rcl-run (flags lines &key (no-color nil) (term "xterm") (colors nil))
  "Feed LINES to `dotcl FLAGS repl` on a pipe, with NO_COLOR, TERM and
DOTCL_COLORS set as given (NIL unsets). Returns standard output, standard error
and the exit status. The variables are set in this process for the child to
inherit and put back afterwards, so the result does not depend on the terminal
the suite was started from."
  (let ((input (concatenate 'string *rcl-dir* "input.lisp"))
        (old-no-color (rcl-getenv "NO_COLOR"))
        (old-term (rcl-getenv "TERM"))
        (old-colors (rcl-getenv "DOTCL_COLORS")))
    (with-open-file (out input :direction :output :if-exists :supersede)
      (dolist (line lines)
        (write-string line out)
        (terpri out)))
    (unwind-protect
         (progn
           (rcl-setenv "NO_COLOR" no-color)
           (rcl-setenv "TERM" term)
           (rcl-setenv "DOTCL_COLORS" colors)
           (multiple-value-bind (out err code)
               (ignore-errors
                (uiop:run-program (append (list *rcl-exe*) (rcl-image)
                                          (list "--no-init") flags (list "repl"))
                                  :input (pathname input)
                                  :output :string
                                  :error-output :string
                                  :ignore-error-status t))
             (values (or out "") (or err "") code)))
      (rcl-setenv "NO_COLOR" old-no-color)
      (rcl-setenv "TERM" old-term)
      (rcl-setenv "DOTCL_COLORS" old-colors))))

(defvar *rcl-lines* '("(+ 1 2)" "(warn \"careful\")" "(princ \"plain\")"))

(defun rcl-has-escape (s) (and (position #\Escape s) t))

;;; --color=always paints a pipe: the prompt's package name, the value, the
;;; warning. What PRINC wrote is left as it was.
(defvar *rcl-always-out*)
(defvar *rcl-always-err*)
(multiple-value-setq (*rcl-always-out* *rcl-always-err*)
  (rcl-run '("--color=always") *rcl-lines*))

(deftest rcl-always-prompt-and-value
  (and (search "<ESC>[1;32mCL-USER><ESC>[0m <ESC>[36m3<ESC>[0m"
               (rcl-show *rcl-always-out*))
       t)
  t)

(deftest rcl-always-warning
  (and (search "<ESC>[33mWARNING: careful<ESC>[0m" (rcl-show *rcl-always-err*)) t)
  t)

(deftest rcl-always-printed-output-plain
  (and (search "<ESC>[0m plain<ESC>[36m\"plain\"<ESC>[0m"
               (rcl-show *rcl-always-out*))
       t)
  t)

;;; An error: the report is red, on standard error.
(deftest rcl-always-error
  (multiple-value-bind (out err) (rcl-run '("--color=always") '("(car 1)"))
    (declare (ignore out))
    (and (search "<ESC>[31m; Debugger entered on TYPE-ERROR:<ESC>[0m" (rcl-show err)) t))
  t)

;;; auto does not paint a pipe.
(deftest rcl-auto-pipe-plain
  (multiple-value-bind (out err) (rcl-run '() *rcl-lines*)
    (list (rcl-has-escape out) (rcl-has-escape err)
          (and (search "CL-USER> 3" out) t)))
  (nil nil t))

(deftest rcl-never-plain
  (multiple-value-bind (out err) (rcl-run '("--color=never") *rcl-lines*)
    (list (rcl-has-escape out) (rcl-has-escape err)))
  (nil nil))

;;; NO_COLOR and TERM=dumb win over --color=always.
(deftest rcl-no-color-over-always
  (multiple-value-bind (out err) (rcl-run '("--color=always") *rcl-lines* :no-color "1")
    (list (rcl-has-escape out) (rcl-has-escape err)))
  (nil nil))

(deftest rcl-term-dumb-over-always
  (multiple-value-bind (out err) (rcl-run '("--color=always") *rcl-lines* :term "dumb")
    (list (rcl-has-escape out) (rcl-has-escape err)))
  (nil nil))

;;; A value that is not one of the three is refused before anything starts.
(deftest rcl-bad-value-refused
  (multiple-value-bind (out err code) (rcl-run '("--color=sometimes") '("(+ 1 2)"))
    (list (search "CL-USER>" out) (and (search "--color" err) t) code))
  (nil t 2))

;;; -- DOTCL_COLORS ---------------------------------------------------------------

;;; The variable changes the roles it names and only those. An empty value is
;;; no colour for that role; an unknown role or a value with anything but digits
;;; and semicolons is skipped, and the REPL still starts.
(deftest rcl-dotcl-colors-env
  (multiple-value-bind (out err code)
      (rcl-run '("--color=always") *rcl-lines*
               :colors "prompt=4;34:result=:nosuch=1:warning=red:error=35")
    (declare (ignore code))
    (list (and (search "<ESC>[4;34mCL-USER><ESC>[0m 3" (rcl-show out)) t)
          (and (search "<ESC>[33mWARNING: careful<ESC>[0m" (rcl-show err)) t)))
  (t t))

;;; DOTCL_COLORS picks colours, never whether to paint: NO_COLOR still wins.
(deftest rcl-dotcl-colors-no-color-wins
  (multiple-value-bind (out err)
      (rcl-run '("--color=always") *rcl-lines* :no-color "1" :colors "prompt=4;34")
    (list (rcl-has-escape out) (rcl-has-escape err)))
  (nil nil))

;;; The same form from Lisp, for an init file, on top of what is in effect.
(deftest rcl-set-colors
  (unwind-protect
       (progn
         (dotcl-repl:set-colors "comment=2;3:string=")
         (dotcl-repl:set-colors "keyword=1;35:bogus=1:comment=x")
         (list (rcl-show (dotcl::%repl-paint :comment "c" t))
               (dotcl::%repl-paint :string "s" t)
               (rcl-show (dotcl::%repl-paint :keyword "k" t))
               (rcl-show (dotcl::%repl-paint :prompt "p" t))))
    (dotcl-repl:set-colors "comment=2:string=32:keyword=35"))
  ("<ESC>[2;3mc<ESC>[0m" "s" "<ESC>[1;35mk<ESC>[0m" "<ESC>[1;32mp<ESC>[0m"))
