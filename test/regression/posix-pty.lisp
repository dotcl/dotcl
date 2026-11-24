;;; contrib/dotcl-posix: a child on a pseudo-terminal, its output and input, the exit status, signals, the window
;;; size, errors, and no descriptor left open. Unix only (the file is not loaded on Windows), and no second runtime
;;; is started: the children are /bin/sh and cat.

(require "dotcl-posix")

(defun %pty-text (argv &rest options)
  (apply #'dotcl-posix:spawn-in-pty argv options))

(deftest posix-pty.output-and-exit-code
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "echo hello-pty; exit 7"))
    (let ((text (dotcl-posix:pty-expect p "hello-pty" :timeout 10)))
      (multiple-value-bind (exited code sig) (dotcl-posix:pty-wait p)
        (list (and (search "hello-pty" text) t) exited code sig))))
  (t t 7 nil))

(deftest posix-pty.input-echo
  (dotcl-posix:with-pty-process (p '("/bin/cat"))
    (dotcl-posix:pty-write p (format nil "ping-42~%"))
    (let ((text (dotcl-posix:pty-expect p "ping-42" :timeout 10)))
      (and text t)))
  t)

(deftest posix-pty.signal-ends-child
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "sleep 30"))
    (dotcl-posix:pty-drain p :timeout 0.2)
    (let ((pid (dotcl-posix:pty-process-pid p)))
      (when (typep pid '(integer 2 2147483647)) (dotcl-posix:kill pid :term)))
    (multiple-value-list (dotcl-posix:pty-wait p)))
  (t nil 15))

;;; A child's exit (SIGCHLD) arrives while a read waits for it: the wait goes on, it is not an error (poll fails with
;;; EINTR on Linux). Twenty children, each ending a moment after the read started to wait.
(deftest posix-pty.read-waits-through-a-childs-exit
  (let ((results '()))
    (dotimes (i 20)
      (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "sleep 0.1; exit 0"))
        (push (handler-case
                  (let ((r nil))
                    (dotimes (k 40) (setf r (dotcl-posix:pty-read p :timeout 0.5)) (when (eq r :eof) (return)))
                    r)
                (error (e) (type-of e)))
              results)))
    (remove-duplicates results))
  (:eof))

(deftest posix-pty.eof-after-exit
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "exit 0"))
    (let ((r nil))
      (dotimes (i 50)
        (setf r (dotcl-posix:pty-read p :timeout 0.2))
        (when (eq r :eof) (return)))
      r))
  :eof)

(deftest posix-pty.window-size
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "sleep 3") :window-size '(30 100))
    (let ((first (multiple-value-list (dotcl-posix:pty-window-size p))))
      (setf (dotcl-posix:pty-window-size p) '(40 120))
      (list first (multiple-value-list (dotcl-posix:pty-window-size p)))))
  ((30 100) (40 120)))

(deftest posix-pty.window-size-above-32767
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "sleep 3") :window-size '(32768 40000))
    (let ((first (multiple-value-list (dotcl-posix:pty-window-size p))))
      (setf (dotcl-posix:pty-window-size p) '(65535 65535))
      (list first (multiple-value-list (dotcl-posix:pty-window-size p)))))
  ((32768 40000) (65535 65535)))

(deftest posix-pty.child-sees-window-size
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "stty size") :window-size '(24 80))
    (and (dotcl-posix:pty-expect p "24 80" :timeout 10) t))
  t)

;;; A child that prints and exits at once: every run reads its output. macOS drops what the master has not read
;;; when the last slave descriptor closes, so the slave stays open for as long as the process lives (a loaded
;;; machine lost the output of about one run in 60 before). The loop only makes a loss likely, it cannot force one.
(deftest posix-pty.output-of-a-short-lived-child-is-read
  (let ((lost 0))
    (dotimes (i 60)
      (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "stty size") :window-size '(24 80))
        (unless (dotcl-posix:pty-expect p "24 80" :timeout 10) (incf lost))))
    lost)
  0)

;;; With the child gone and everything it wrote read, a read says so.
(deftest posix-pty.read-after-the-child-exited-is-eof
  (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "echo bye"))
    (dotcl-posix:pty-expect p "bye" :timeout 10)
    (dotcl-posix:pty-wait p)
    (dotcl-posix:pty-read p :timeout 0.2))
  :eof)

(deftest posix-pty.errors
  (list (handler-case (dotcl-posix::fd-window-size -1)
          (dotcl-posix:posix-error (e) (dotcl-posix:posix-error-function e)))
        (handler-case (dotcl-posix:signal-number :nosuchsignal) (error () :error)))
  ("ioctl" :error))

(defun %open-fd-count ()
  "How many descriptors 0..4095 are open, found with fstat, which neither duplicates nor closes anything (a
dup + close probe closes a descriptor the process may not close: macOS guards some of its own, and the
process is killed when one is closed)."
  (let ((n 0)
        (buf (dotnet:static "System.Runtime.InteropServices.Marshal" "AllocHGlobal" 512)))
    (unwind-protect
         (dotimes (fd 4096 n)
           (when (zerop (dotnet:%ffi-call "libc" "fstat" '(:int :ptr) :int fd buf)) (incf n)))
      (dotnet:static "System.Runtime.InteropServices.Marshal" "FreeHGlobal" buf))))

(deftest posix-pty.no-descriptor-left-open
  (progn
    (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "exit 0")) (dotcl-posix:pty-wait p))
    (let ((before (%open-fd-count)))
      (dotimes (i 20)
        (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "echo x") :window-size '(24 80))
          (dotcl-posix:pty-expect p "x" :timeout 10))
        (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "exit 1"))
          (dotcl-posix:pty-wait p))
        (ignore-errors (dotcl-posix:spawn-in-pty '("/nonexistent/program-xyz")))
        (ignore-errors (dotcl-posix:spawn-in-pty '("/bin/sh" "-c" "true") :directory "/nonexistent-dir")))
      (= before (%open-fd-count))))
  t)

(deftest posix-pty.bad-window-size-errors-and-leaks-nothing
  (let ((before (%open-fd-count)))
    (let ((results (loop for size in '((24) (nil nil) (a b) (70000 80) (-1 80) 5)
                         collect (handler-case (progn (dotcl-posix:spawn-in-pty '("/bin/sh" "-c" "true")
                                                                                 :window-size size)
                                                      :no-error)
                                   (error () :error)))))
      (dotimes (i 20)
        (ignore-errors (dotcl-posix:spawn-in-pty '("/bin/sh" "-c" "true") :window-size '(a b))))
      (list results (= before (%open-fd-count)))))
  ((:error :error :error :error :error :error) t))

;;; The descriptors spawn-in-pty itself holds (the master, and the slave it keeps open) do not reach the child. The
;;; child is told their numbers and looks for them in /dev/fd. A sweep over fixed numbers would also see a descriptor
;;; another thread of this process has open and not yet marked close-on-exec (a .NET pipe made on macOS is one,
;;; for the moment between pipe and fcntl): that is not this function's.
(deftest posix-pty.child-inherits-no-extra-descriptors
  (flet ((open-in-child (&rest options)
           (let ((p (apply #'dotcl-posix:spawn-in-pty
                           '("/bin/sh" "-c" "read m s; for i in $m $s; do if [ -e /dev/fd/$i ]; then echo open-$i; fi; done; echo done")
                           options)))
             (unwind-protect
                  (progn
                    (dotcl-posix:pty-write p (format nil "~D ~D~%"
                                                     (dotcl-posix::pty-process-master p)
                                                     (or (dotcl-posix::pty-process-slave p) 0)))
                    (dotcl-posix:pty-expect p "done" :timeout 10)
                    (let ((text (dotcl-posix:pty-process-text p)))
                      (list (and (search "done" text) t) (and (search "open-" text) t))))
               (ignore-errors (dotcl-posix:pty-wait p))
               (dotcl-posix:pty-close p)))))
    (list (open-in-child) (open-in-child :window-size '(24 80))))
  ((t nil) (t nil)))

;; The refusal of an unsafe pid is tested through CHECK-PID alone: a test that CALLED kill with 0 or -1
;; (the caller's own process group, every process of the user) would take the test run down with it
;; whenever an older dotcl-posix without the check happens to be the one loaded.
(deftest posix-pty.pid-check-refuses-pid-zero-and-negative
  (list (and (fboundp 'dotcl-posix::check-pid) t)
        (loop for pid in (list 0 -1 -5 nil 1.5 (expt 2 40) :all 2147483648)
              collect (handler-case (progn (dotcl-posix::check-pid "kill" pid) :accepted)
                        (error () :refused)))
        (handler-case (dotcl-posix::check-pid "kill" 12345) (error () :refused)))
  (t (:refused :refused :refused :refused :refused :refused :refused :refused) 12345))
