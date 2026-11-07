;;; Wall time of fresh processes: dotcl startup-time.lisp COUNT COMMAND [ARGS...]
;;; Runs COMMAND once unmeasured (file cache), then COUNT times measured, each in
;;; a new process with stdin closed and output captured. If STARTUP_EXPECT is set,
;;; the captured stdout must contain it. Prints one line:
;;;   STARTUP <median ms> <min ms> <max ms> <count>

(defun fail (control &rest args)
  (format *error-output* "~?~%" control args)
  (dotcl:quit 1))

(defun run-once (command expect)
  "Run COMMAND in a new process (stdin closed, stderr discarded) and return its wall time in ms."
  (let* ((info (dotnet:new "System.Diagnostics.ProcessStartInfo" (first command)))
         (watch (dotnet:new "System.Diagnostics.Stopwatch")))
    (dolist (arg (rest command))
      (dotnet:invoke (dotnet:invoke info "ArgumentList") "Add" arg))
    (setf (dotnet:invoke info "UseShellExecute") nil
          (dotnet:invoke info "RedirectStandardInput") t
          (dotnet:invoke info "RedirectStandardOutput") t
          (dotnet:invoke info "RedirectStandardError") t)
    (dotnet:invoke watch "Start")
    (let ((process (handler-case (dotnet:static "System.Diagnostics.Process" "Start" info)
                     (error () (fail "command failed (status 127)")))))
      (dotnet:invoke (dotnet:invoke process "StandardInput") "Close")
      (let* ((errors (dotnet:invoke (dotnet:invoke process "StandardError") "ReadToEndAsync"))
             (output (dotnet:invoke (dotnet:invoke process "StandardOutput") "ReadToEnd")))
        (dotnet:invoke process "WaitForExit")
        (dotnet:invoke errors "Wait")
        (dotnet:invoke watch "Stop")
        (let ((status (dotnet:invoke process "ExitCode"))
              (ms (dotnet:invoke (dotnet:invoke watch "Elapsed") "TotalMilliseconds")))
          (unless (zerop status) (fail "command failed (status ~D)" status))
          (when (and expect (not (search expect output)))
            (fail "output lacks '~A'" expect))
          ms)))))

(let ((arguments (dotcl:script-arguments)))
  (let ((count (and arguments (parse-integer (first arguments) :junk-allowed t)))
        (command (rest arguments))
        (expect (let ((value (dotcl:getenv "STARTUP_EXPECT"))) (and value (plusp (length value)) value))))
    (unless (and count (plusp count) command)
      (fail "usage: startup-time.lisp COUNT COMMAND..."))
    (run-once command expect)
    (let* ((times (sort (loop repeat count collect (run-once command expect)) #'<))
           (n (length times))
           (median (if (oddp n)
                       (nth (floor (1- n) 2) times)
                       (/ (+ (nth (1- (floor n 2)) times) (nth (floor n 2) times)) 2))))
      (format t "STARTUP ~,1F ~,1F ~,1F ~D~%" median (first times) (car (last times)) n))))
