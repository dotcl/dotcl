;;; On Windows a string command reaches the shell as CMD /C <string> (UIOP:RUN-PROGRAM builds it
;;; that way). cmd parses its own command line, so the string must reach it unquoted: the
;;; MSVCRT-style quoting of an argument list turned each inner " into \", and cmd printed the
;;; backslashes. Windows only; elsewhere the test just passes.

(defun %lpc-run (command)
  (let ((r (dotcl:run-process "cmd" (list "/c" command))))
    (remove #\Return (string-trim '(#\Space #\Return #\Newline) (second r)))))

(defun %lpc-results ()
  #+windows (list (%lpc-run "echo \"a b\" c")
                  (%lpc-run "echo x&echo y"))
  #-windows :not-windows)

(deftest launch-process-cmd-string.inner-quotes
  (%lpc-results)
  #+windows ("\"a b\" c" #.(format nil "x~%y"))
  #-windows :not-windows)
