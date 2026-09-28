;;; A REPL whose standard input is not a terminal has nobody to ask.
;;;
;;; The debugger used to prompt anyway, because the REPL counted as interactive
;;; whatever its input was. On a pipe the prompt then read the end of the input
;;; and took that as a choice of ABORT: the session went on, or ended, as if
;;; the error had been dealt with, and the process exited 0. Now the REPL is
;;; interactive only when its standard input is a terminal, and otherwise the
;;; debugger reports the error and the session ends with a non-zero status, as
;;; a script does.
;;;
;;; A REPL whose input IS a terminal still prompts; that needs a console and is
;;; not checked here.

(require "asdf")

(defvar *rpd-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rpd-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-repl-piped-debugger-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rpd-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rpd-run (lines)
  "Feed LINES to `dotcl repl` on a pipe. Returns standard output, standard
error and the exit status."
  (let ((input (concatenate 'string *rpd-dir* "input.lisp")))
    (with-open-file (out input :direction :output :if-exists :supersede)
      (dolist (line lines)
        (write-string line out)
        (terpri out)))
    (multiple-value-bind (out err code)
        (ignore-errors
         (uiop:run-program (append (list *rpd-exe*) (rpd-image)
                                   (list "--no-init" "repl"))
                           :input (pathname input)
                           :output :string
                           :error-output :string
                           :ignore-error-status t))
      (values (or out "") (or err "") code))))

(defvar *rpd-out*)
(defvar *rpd-err*)
(defvar *rpd-code*)

(multiple-value-setq (*rpd-out* *rpd-err* *rpd-code*)
  (rpd-run '("(+ 1 2)"
             "(with-simple-restart (abort \"give up\") (error \"boom\"))"
             "(print :next)")))

;;; The forms before the error ran.
(deftest rpd-form-before-error-ran
  (and (search "CL-USER> 3" *rpd-out*) t)
  t)

;;; The error is reported.
(deftest rpd-error-reported
  (and (search "boom" *rpd-err*) t)
  t)

;;; No debugger prompt was shown and no restart was taken for us, so the form
;;; after the error did not run...
(deftest rpd-no-debugger-prompt
  (or (search "0] " *rpd-out*) (search "0] " *rpd-err*))
  nil)

(deftest rpd-session-stopped
  (search ":NEXT" *rpd-out*)
  nil)

;;; ...and the exit status says the session failed.
(deftest rpd-exit-status-non-zero
  (and (integerp *rpd-code*) (/= *rpd-code* 0))
  t)

;;; An error the runtime signals itself (CAR of a non-list) enters the debugger
;;; at the REPL too when nothing handles it (see repl-runtime-error-debugger),
;;; so on a pipe it is treated the same way: reported, the rest of the input
;;; not run, a non-zero exit.
(defvar *rpd-runtime-out*)
(defvar *rpd-runtime-err*)
(defvar *rpd-runtime-code*)

(multiple-value-setq (*rpd-runtime-out* *rpd-runtime-err* *rpd-runtime-code*)
  (rpd-run '("(car 1)" "(+ 40 2)")))

(deftest rpd-runtime-error-reported
  (and (search "TYPE-ERROR" *rpd-runtime-err*) t)
  t)

(deftest rpd-runtime-error-stops
  (list (search "CL-USER> 42" *rpd-runtime-out*)
        (or (search "0] " *rpd-runtime-out*) (search "0] " *rpd-runtime-err*))
        (and (integerp *rpd-runtime-code*) (/= *rpd-runtime-code* 0)))
  (nil nil t))
