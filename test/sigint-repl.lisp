;;; Integration test: Ctrl-C (SIGINT) interrupts the dotcl REPL instead of killing it.
;;;
;;; On Unix the REPL reads raw fd 0 and avoids .NET's Unix console driver, so
;;; Console.CancelKeyPress never fires; SIGINT is delivered through a
;;; PosixSignalRegistration instead. This test starts a REPL on a real pty with
;;; contrib/dotcl-posix, starts an infinite call-heavy loop, sends SIGINT to the
;;; process, and checks that the REPL survives: the debugger's ABORT restart
;;; returns to top level and the next form is evaluated.
;;;
;;; Not part of `make test-regression` (it starts a second runtime); run it with
;;;
;;;     make test-sigint-repl
;;;
;;; or by hand, with the runtime built:
;;;
;;;     dotcl test/sigint-repl.lisp [path/to/runtime.dll]
;;;
;;; Unix only. The Windows Ctrl+C is a different mechanism and is checked there.
;;; Exit status 0 on pass, 1 on failure.

(require "dotcl-posix")

(defun runtime-dll ()
  (let ((arg (first (dotcl:script-arguments))))
    (namestring
     (or (and arg (probe-file arg))
         (probe-file (merge-pathnames "../runtime/bin/Debug/net10.0/runtime.dll" *load-truename*))
         (probe-file (merge-pathnames "../runtime/bin/Release/net10.0/runtime.dll" *load-truename*))
         (error "no runtime.dll found: build the runtime first, or give its path")))))

(defun run-sigint-test (dll)
  ;; --no-readline: a plain line reader, so the test can feed input over the pty
  ;; without driving the raw line editor.
  (dotcl-posix:with-pty-process
      (p (list "dotnet" dll "repl" "--no-readline")
         :environment (cons "DOTNET_GCConserveMemory=7" (dotcl:posix-environ)))
    (dotcl-posix:pty-drain p :timeout 8)                ; the banner
    (dotcl-posix:pty-write p (format nil "(loop for i from 0 do (identity i))~%"))
    (dotcl-posix:pty-drain p :timeout 2)                ; let the loop spin
    (let ((mark (length (dotcl-posix:pty-process-text p))))
      (dotcl-posix:kill (dotcl-posix:pty-process-pid p) :int)   ; the actual Ctrl-C
      (dotcl-posix:pty-drain p :timeout 3)
      (dotcl-posix:pty-write p (format nil "(+ 40 2)~%"))       ; is the REPL still alive?
      (dotcl-posix:pty-drain p :timeout 3)
      (dotcl-posix:pty-write p (string (code-char 4)))          ; Ctrl-D to exit
      (dotcl-posix:pty-drain p :timeout 0.5)
      (let* ((after (subseq (dotcl-posix:pty-process-text p) mark))
             (interrupted (or (search "INTERACTIVE-INTERRUPT" after)
                              (search "interrupt" after :test #'char-equal)))
             (alive (search "42" after)))
        (write-string after)
        (format t "~%RESULT interrupted=~A repl_alive_after=~A~%" (and interrupted t) (and alive t))
        (and interrupted alive t)))))

(dotcl:quit (if (run-sigint-test (runtime-dll)) 0 1))
