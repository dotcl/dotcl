;;; dotcl-posix.lisp: pseudo-terminals and signals on Unix.
;;;
;;;   (require "dotcl-posix")
;;;   (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "tty; sleep 1"))
;;;     (dotcl-posix:pty-expect p "/dev/tty" :timeout 5))
;;;
;;; The shape follows Python's pty / os / signal modules. Everything is a call
;;; into libc through DOTNET:%FFI-CALL; nothing here forks (a managed process is
;;; multi-threaded, so fork is not safe). A child is started with posix_spawn in
;;; a new session of its own (POSIX_SPAWN_SETSID) and the slave side of the pty is
;;; opened as its standard input, which makes it the child's controlling terminal.
;;;
;;; Linux and macOS. Platform differences (flag values, the errno accessor, the
;;; signal numbers that differ) are in the table at the top of this file.
;;; Loading it on Windows signals an error.

(defpackage :dotcl-posix
  (:use :cl)
  (:export #:posix-error #:posix-error-function #:posix-error-errno
           #:open-pty #:spawn-in-pty #:pty-process #:pty-process-pid #:pty-process-master
           #:pty-process-text #:pty-drain #:pty-read #:pty-write #:pty-expect #:pty-close #:with-pty-process
           #:pty-wait #:wait-pid #:kill #:kill-process-group #:signal-number #:pty-window-size
           #:+sighup+ #:+sigint+ #:+sigquit+ #:+sigkill+ #:+sigterm+ #:+sigusr1+ #:+sigusr2+))

(in-package :dotcl-posix)

(when (dotnet:static "System.OperatingSystem" "IsWindows")
  (error "dotcl-posix is not available on Windows: it calls the Unix libc for pseudo-terminals and signals (ConPTY is not covered); use it on Linux or macOS"))

;;; ---- the platform table ----------------------------------------------------

(defvar *mac-p* (dotnet:static "System.OperatingSystem" "IsMacOS"))

(defmacro platform-case ((&key mac linux))
  `(if *mac-p* ,mac ,linux))

(defparameter +o-rdwr+ 2)
(defparameter +o-noctty+ (platform-case (:mac #x20000 :linux #x100)))
(defparameter +spawn-setsid+ (platform-case (:mac #x400 :linux #x80)))
(defparameter +o-cloexec+ (platform-case (:mac #x1000000 :linux #x80000)))
(defparameter +pollin+ 1)
(defparameter +wnohang+ 1)
(defparameter +tiocgwinsz+ (platform-case (:mac #x40087468 :linux #x5413)))
(defparameter +tiocswinsz+ (platform-case (:mac #x80087467 :linux #x5414)))
(defparameter +sighup+ 1)
(defparameter +sigint+ 2)
(defparameter +sigquit+ 3)
(defparameter +sigkill+ 9)
(defparameter +sigusr1+ (platform-case (:mac 30 :linux 10)))
(defparameter +sigusr2+ (platform-case (:mac 31 :linux 12)))
(defparameter +sigterm+ 15)

(defun signal-number (signal)
  "The number for SIGNAL: an integer, or a keyword such as :INT, :TERM, :KILL, :HUP, :QUIT, :USR1."
  (if (integerp signal)
      signal
      (let ((name (string-upcase (string signal))))
        (when (and (> (length name) 3) (string= "SIG" name :end2 3))
          (setf name (subseq name 3)))
        (cond ((string= name "HUP") +sighup+)
              ((string= name "INT") +sigint+)
              ((string= name "QUIT") +sigquit+)
              ((string= name "KILL") +sigkill+)
              ((string= name "USR1") +sigusr1+)
              ((string= name "USR2") +sigusr2+)
              ((string= name "TERM") +sigterm+)
              (t (error "dotcl-posix: unknown signal ~S" signal))))))

;;; ---- libc ------------------------------------------------------------------

(defun libc (name types ret &rest args)
  (apply #'dotnet:%ffi-call "libc" name types ret args))

(defun marshal (name &rest args)
  (apply #'dotnet:static "System.Runtime.InteropServices.Marshal" name args))

(define-condition posix-error (error)
  ((function :initarg :function :reader posix-error-function)
   (errno :initarg :errno :reader posix-error-errno))
  (:report (lambda (c s)
             (format s "~A failed, errno ~D" (posix-error-function c) (posix-error-errno c)))))

(defun errno ()
  (marshal "ReadInt32" (libc (platform-case (:mac "__error" :linux "__errno_location")) '() :ptr) 0))

(defconstant +eintr+ 4)

(defmacro retrying-eintr (&body body)
  "BODY (a libc call that returns a negative number on failure), run again while it failed because a signal
interrupted it (EINTR). A child's exit is such a signal (SIGCHLD): on Linux it interrupts a poll or a waitpid
that was waiting for exactly that."
  (let ((r (gensym)))
    `(loop (let ((,r (progn ,@body)))
             (unless (and (< ,r 0) (= (errno) +eintr+))
               (return ,r))))))

(defun check (name result)
  "RESULT, or a POSIX-ERROR for the libc function NAME when RESULT is negative."
  (if (< result 0)
      (error 'posix-error :function name :errno (errno))
      result))

(defun check-zero (name result)
  "For the functions that return an error number themselves (the posix_spawn family)."
  (unless (zerop result)
    (error 'posix-error :function name :errno result))
  result)

(defmacro with-native ((var size) &body body)
  `(let ((,var (marshal "AllocHGlobal" ,size)))
     (unwind-protect (progn ,@body) (marshal "FreeHGlobal" ,var))))

;;; ---- signals and waiting ---------------------------------------------------

(defun check-pid (who pid)
  "PID when it names one process: a positive integer that fits a pid_t. 0, negative numbers (process groups, and -1
for every process), NIL and anything else are refused here, before any system call: a SIGKILL sent to the wrong
number takes down the caller's own process group or the user's session."
  (unless (typep pid '(integer 1 2147483647))
    (error "dotcl-posix: ~A needs a positive process id, not ~S" who pid))
  pid)

(defun kill (pid signal)
  "Send SIGNAL (a number or a keyword such as :INT) to the process PID, a positive process id. The group forms are
KILL-PROCESS-GROUP."
  (check "kill" (libc "kill" '(:int :int) :int (check-pid "kill" pid) (signal-number signal)))
  t)

(defun kill-process-group (pgid signal)
  "Send SIGNAL to every process of the process group PGID (a positive group id, as kill(-PGID))."
  (check "kill" (libc "kill" '(:int :int) :int (- (check-pid "kill-process-group" pgid)) (signal-number signal)))
  t)

(defun wait-pid (pid &key nohang)
  "waitpid. Returns (values pid status): the pid and the raw wait status, or NIL NIL when NOHANG is true and the
child is still running. Use PTY-WAIT for the exit code."
  (with-native (status 8)
    (marshal "WriteInt32" status 0 0)
    (let ((r (check "waitpid" (retrying-eintr (libc "waitpid" '(:int :ptr :int) :int (check-pid "wait-pid" pid) status (if nohang +wnohang+ 0))))))
      (if (zerop r)
          (values nil nil)
          (values r (marshal "ReadInt32" status 0))))))

(defun decode-status (status)
  "(values exit-code signal) from a raw wait status."
  (let ((sig (logand status #x7f)))
    (if (zerop sig)
        (values (logand (ash status -8) #xff) nil)
        (values nil sig))))

;;; ---- pseudo-terminals ------------------------------------------------------

(defun open-pty ()
  "Open a pseudo-terminal. Returns (values master-fd slave-path)."
  (let ((fd (check "posix_openpt" (libc "posix_openpt" '(:int) :int (logior +o-rdwr+ +o-noctty+ +o-cloexec+)))))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (libc "close" '(:int) :int fd))))
      (check "grantpt" (libc "grantpt" '(:int) :int fd))
      (check "unlockpt" (libc "unlockpt" '(:int) :int fd))
      (let ((name (libc "ptsname" '(:int) :ptr fd)))
        (when (zerop name) (error 'posix-error :function "ptsname" :errno (errno)))
        (values fd (marshal "PtrToStringAnsi" name))))))

(defstruct (pty-process (:constructor %make-pty-process))
  pid master slave (text "") waited)

(defun native-string-array (strings)
  "A NULL-terminated char* array for STRINGS; the second value frees it."
  (let* ((n (length strings))
         (array (marshal "AllocHGlobal" (* 8 (1+ n))))
         (cells (loop for s in strings for i from 0
                      collect (let ((p (marshal "StringToHGlobalAnsi" s)))
                                (marshal "WriteInt64" array (* 8 i) p)
                                p))))
    (marshal "WriteInt64" array (* 8 n) 0)
    (values array
            (lambda ()
              (dolist (p cells) (marshal "FreeHGlobal" p))
              (marshal "FreeHGlobal" array)))))

(defun ioctl-ptr (fd request pointer)
  "ioctl(FD, REQUEST, POINTER). ioctl is variadic, which a fixed-argument call cannot make on Apple arm64 (the
variadic arguments go on the stack there), so macOS calls __ioctl, the system call stub it wraps."
  (libc (platform-case (:mac "__ioctl" :linux "ioctl")) '(:int :ulong :ptr) :int fd request pointer))

(defun check-window-size (size)
  "SIZE as a list (rows columns) of two integers in 0..65535, or an error."
  (unless (and (consp size) (consp (cdr size)) (null (cddr size))
               (every (lambda (n) (typep n '(integer 0 65535))) size))
    (error "dotcl-posix: a window size is a list (rows columns) of two integers from 0 to 65535, not ~S" size))
  size)

(defun fd-window-size (fd)
  "(values rows columns) of the terminal FD."
  (with-native (ws 8)
    (dotimes (i 4) (marshal "WriteInt16" ws (* 2 i) 0))
    (check "ioctl" (ioctl-ptr fd +tiocgwinsz+ ws))
    (values (logand #xffff (marshal "ReadInt16" ws 0)) (logand #xffff (marshal "ReadInt16" ws 2)))))

(defun set-fd-window-size (fd rows columns)
  (with-native (ws 8)
    (marshal "WriteInt16" ws 0 rows)
    (marshal "WriteInt16" ws 2 columns)
    (marshal "WriteInt16" ws 4 0)
    (marshal "WriteInt16" ws 6 0)
    (check "ioctl" (ioctl-ptr fd +tiocswinsz+ ws))
    t))

(defun pty-window-size (process)
  "(values rows columns) of the child's terminal. (SETF PTY-WINDOW-SIZE) takes a list (rows columns) and resizes it;
the child gets SIGWINCH."
  (fd-window-size (pty-process-master process)))

(defun (setf pty-window-size) (size process)
  (destructuring-bind (rows columns) (check-window-size size)
    (set-fd-window-size (pty-process-master process) rows columns)
    size))

(defun spawn-in-pty (argv &key (environment (dotcl:posix-environ)) directory window-size)
  "Start the program ARGV (a list of strings; the first is looked up on PATH) in a new session whose controlling
terminal is a new pseudo-terminal, and return a PTY-PROCESS. ENVIRONMENT is a list of \"NAME=value\" strings
(default: this process's); DIRECTORY is the child's working directory; WINDOW-SIZE is a list (rows columns) the
terminal has from the start."
  (when window-size (check-window-size window-size))
  (multiple-value-bind (master slave) (open-pty)
    (let ((ok nil) (held-slave nil))
      (unwind-protect
           (progn
             ;; The slave is held open (close-on-exec, so the child does not inherit it) for as long as the
             ;; PTY-PROCESS lives. macOS refuses the window-size ioctl on the master until the slave is open,
             ;; forgets the size when the last slave descriptor closes, and drops the output the child has
             ;; written but the master has not read yet when that happens: a child that exits before its
             ;; output is read would otherwise leave nothing to read.
             (setf held-slave (check "open" (libc "open" '(:string :int) :int slave
                                                  (logior +o-rdwr+ +o-noctty+ +o-cloexec+))))
             (when window-size
               (set-fd-window-size held-slave (first window-size) (second window-size)))
             (with-native (attr 512)
               (with-native (actions 512)
                 (with-native (pid-cell 8)
                   (multiple-value-bind (argv-array free-argv) (native-string-array argv)
                     (multiple-value-bind (env-array free-env) (native-string-array environment)
                       (unwind-protect
                            (progn
                              (check-zero "posix_spawnattr_init" (libc "posix_spawnattr_init" '(:ptr) :int attr))
                              (check-zero "posix_spawnattr_setflags"
                                          (libc "posix_spawnattr_setflags" '(:ptr :short) :int attr +spawn-setsid+))
                              (check-zero "posix_spawn_file_actions_init"
                                          (libc "posix_spawn_file_actions_init" '(:ptr) :int actions))
                              (check-zero "posix_spawn_file_actions_addopen"
                                          (libc "posix_spawn_file_actions_addopen" '(:ptr :int :string :int :uint32)
                                                :int actions 0 slave +o-rdwr+ 0))
                              (check-zero "posix_spawn_file_actions_adddup2"
                                          (libc "posix_spawn_file_actions_adddup2" '(:ptr :int :int) :int actions 0 1))
                              (check-zero "posix_spawn_file_actions_adddup2"
                                          (libc "posix_spawn_file_actions_adddup2" '(:ptr :int :int) :int actions 0 2))
                              (when directory
                                (check-zero "posix_spawn_file_actions_addchdir_np"
                                            (libc "posix_spawn_file_actions_addchdir_np" '(:ptr :string) :int
                                                  actions (namestring directory))))
                              (check-zero "posix_spawnp"
                                          (libc "posix_spawnp" '(:ptr :string :ptr :ptr :ptr :ptr) :int
                                                pid-cell (first argv) actions attr argv-array env-array))
                              (setf ok t)
                              (prog1 (%make-pty-process :pid (marshal "ReadInt32" pid-cell 0) :master master :slave held-slave)
                                (setf held-slave nil)))
                         (libc "posix_spawn_file_actions_destroy" '(:ptr) :int actions)
                         (libc "posix_spawnattr_destroy" '(:ptr) :int attr)
                         (funcall free-argv)
                         (funcall free-env))))))))
        (when held-slave (libc "close" '(:int) :int held-slave))
        (unless ok (libc "close" '(:int) :int master))))))

(defun poll-readable (fd timeout)
  "True when FD has data (or hang-up) within TIMEOUT seconds."
  (with-native (pollfd 8)
    (marshal "WriteInt32" pollfd 0 fd)
    (marshal "WriteInt16" pollfd 4 +pollin+)
    (marshal "WriteInt16" pollfd 6 0)
    ;; An interrupted poll goes on for what is left of the time.
    (let* ((deadline (+ (get-internal-real-time) (round (* timeout internal-time-units-per-second))))
           (r (check "poll" (retrying-eintr
                              (libc "poll" '(:ptr :uint32 :int) :int pollfd 1
                                    (max 0 (round (* 1000 (/ (- deadline (get-internal-real-time))
                                                              internal-time-units-per-second)))))))))
      (plusp r))))

(defun pty-read (process &key (timeout 0) (max 4096))
  "Read what the child has written. Waits up to TIMEOUT seconds for something to arrive. Returns a string, \"\"
when nothing came in time, or :EOF when the child has closed its side (or has exited and everything it wrote has
been read). Bytes are decoded as UTF-8."
  (let ((fd (pty-process-master process)))
    (flet ((read-some ()
             (with-native (buffer max)
               (let ((n (retrying-eintr (libc "read" '(:int :ptr :long) :long fd buffer max))))
                 (cond ((plusp n)
                        (let ((octets (make-array n :element-type '(unsigned-byte 8))))
                          (dotimes (i n) (setf (aref octets i) (marshal "ReadByte" buffer i)))
                          (dotcl:octets-to-string octets)))
                       ;; Linux reports a closed slave as EIO, macOS as end of file.
                       ((zerop n) :eof)
                       ((= (errno) 5) :eof)
                       (t (error 'posix-error :function "read" :errno (errno))))))))
      (cond ((poll-readable fd timeout) (read-some))
            ;; The slave is held open here, so the terminal never reports the child's exit itself: a child
            ;; that has exited, with nothing left to read, is the end.
            ((and (pty-process-slave process) (pty-wait process :nohang t))
             (if (poll-readable fd 0) (read-some) :eof))
            (t "")))))

(defun pty-write (process string)
  "Write STRING (UTF-8) to the child's terminal, as if typed."
  (let* ((octets (dotcl:string-to-octets string))
         (n (length octets)))
    (with-native (buffer (max n 1))
      (dotimes (i n) (marshal "WriteByte" buffer i (aref octets i)))
      (let ((done 0))
        (loop while (< done n)
              do (let ((w (check "write" (retrying-eintr (libc "write" '(:int :ptr :long) :long (pty-process-master process)
                                                                  (+ buffer done) (- n done))))))
                   (incf done w)))))
    n))

(defun pty-expect (process text &key (timeout 5))
  "Read the child's output into PTY-PROCESS-TEXT until TEXT is in it or TIMEOUT seconds have passed. Returns the
text read so far when TEXT is in it, NIL when it never appeared (or the child closed the terminal first)."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (when (search text (pty-process-text process))
        (return (pty-process-text process)))
      (let ((remaining (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= remaining 0) (return nil))
        (let ((chunk (pty-read process :timeout (min remaining 0.2))))
          (cond ((eq chunk :eof) (return nil))
                (t (setf (pty-process-text process)
                         (concatenate 'string (pty-process-text process) chunk)))))))))

(defun pty-drain (process &key (timeout 0.5))
  "Read whatever the child writes during TIMEOUT seconds into PTY-PROCESS-TEXT; returns that text."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (let ((remaining (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= remaining 0) (return (pty-process-text process)))
        (let ((chunk (pty-read process :timeout (min remaining 0.2))))
          (when (eq chunk :eof) (return (pty-process-text process)))
          (setf (pty-process-text process) (concatenate 'string (pty-process-text process) chunk)))))))

(defun pty-wait (process &key nohang)
  "Wait for the child. Returns (values exited-p exit-code signal): NIL when NOHANG is true and it is still
running, else T with the exit code, or with NIL and the signal number that ended it."
  (if (pty-process-waited process)
      (values t (car (pty-process-waited process)) (cdr (pty-process-waited process)))
      (multiple-value-bind (pid status) (wait-pid (pty-process-pid process) :nohang nohang)
        (if (null pid)
            nil
            (multiple-value-bind (code sig) (decode-status status)
              (setf (pty-process-waited process) (cons code sig))
              (values t code sig))))))

(defun pty-close (process)
  "Close the pseudo-terminal: the master side, and the slave side this process held open."
  (when (pty-process-master process)
    (libc "close" '(:int) :int (pty-process-master process))
    (setf (pty-process-master process) nil))
  (when (pty-process-slave process)
    (libc "close" '(:int) :int (pty-process-slave process))
    (setf (pty-process-slave process) nil))
  nil)

(defmacro with-pty-process ((var argv &rest options) &body body)
  "Run BODY with VAR bound to a PTY-PROCESS started by SPAWN-IN-PTY. On exit the child is killed (SIGKILL) if it is
still running, reaped, and the terminal closed."
  `(let ((,var (spawn-in-pty ,argv ,@options)))
     (unwind-protect (progn ,@body)
       (when (and (not (pty-process-waited ,var)) (typep (pty-process-pid ,var) '(integer 1 2147483647)))
         (ignore-errors (kill (pty-process-pid ,var) +sigkill+))
         (ignore-errors (pty-wait ,var)))
       (pty-close ,var))))

(provide "dotcl-posix")
