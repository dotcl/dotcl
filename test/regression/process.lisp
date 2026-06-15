;;; Regression tests for dotcl:launch-process — streaming process handle.
;;; Uses `dotnet --version` as a portable child process (present on Windows local
;;; and Linux CI). Exercises the launch-program/process-info building blocks:
;;; live output stream, pid, blocking wait, exit code, liveness.

;;; output stream readable + blocking wait returns exit code 0
(deftest d284-launch-process-output-and-exit
  (let* ((p (dotcl:launch-process "dotnet" '("--version")))
         (line (read-line (dotcl:process-output p) nil ""))
         (code (dotcl:process-wait p)))
    (list (eql code 0) (> (length line) 0)))
  (t t))

;;; process-pid is a positive integer
(deftest d284-launch-process-pid-positive-integer
  (let ((p (dotcl:launch-process "dotnet" '("--version"))))
    (prog1 (and (integerp (dotcl:process-pid p))
                (> (dotcl:process-pid p) 0)
                t)
      (dotcl:process-wait p)))
  t)

;;; after wait, process is no longer alive and exit-code is available
(deftest d284-process-alive-p-and-exit-code
  (let ((p (dotcl:launch-process "dotnet" '("--version"))))
    (dotcl:process-wait p)
    (list (dotcl:process-alive-p p) (dotcl:process-exit-code p)))
  (nil 0))

;;; process-output / process-error / process-input are streams
(deftest d284-process-streams-are-streams
  (let ((p (dotcl:launch-process "dotnet" '("--version"))))
    (prog1 (list (streamp (dotcl:process-output p))
                 (streamp (dotcl:process-error p))
                 (streamp (dotcl:process-input p)))
      (dotcl:process-wait p)))
  (t t t))

;;; writing to process-input reaches the child, and reading the child's
;;; output never truncates. `sort` reads all of stdin then emits it sorted and
;;; exits immediately; a tight read-line loop concurrent with that fast exit used
;;; to drop the final line (transient empty-pipe read mistaken for EOF) before
;;; ProcessStreamReader drained the pipe to true EOF. Runs a few times because
;;; the truncation was timing-sensitive.
(deftest d284-process-input-and-no-output-truncation
  (flet ((sort3 ()
           (let* ((p (dotcl:launch-process "sort" '()))
                  (in (dotcl:process-input p))
                  (out (dotcl:process-output p)))
             (write-line "banana" in)
             (write-line "apple" in)
             (write-line "cherry" in)
             (close in)
             (prog1 (loop for line = (read-line out nil nil) while line
                          collect (string-right-trim '(#\return) line))
               (dotcl:process-wait p)))))
    (loop repeat 5 always (equal (sort3) '("apple" "banana" "cherry"))))
  t)
