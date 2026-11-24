;;; contrib/dotcl-posix: a child on a pseudo-terminal, signals to it, its exit status.
;;; Unix only; run.lisp does not load this file on Windows or where there is no
;;; code generation (the libc calls are emitted).

(require "dotcl-posix")

(defun dpx-sh (script)
  (list "/bin/sh" "-c" script))

(deftest dotcl-posix.child-controlling-terminal-and-exit-code
  (progn
      (dotcl-posix:with-pty-process (p (dpx-sh "tty; echo hi; exit 3"))
        (let ((seen (dotcl-posix:pty-expect p "hi" :timeout 10)))
          (multiple-value-bind (exited code sig) (dotcl-posix:pty-wait p)
            (and seen (search "/dev/" seen) exited (eql code 3) (null sig))))))
  t)

(deftest dotcl-posix.sigint-reaches-the-child
  (progn
      (dotcl-posix:with-pty-process (p (dpx-sh "trap 'echo got-int' INT; echo ready; while :; do sleep 1; done"))
        (and (dotcl-posix:pty-expect p "ready" :timeout 10)
             (progn (dotcl-posix:kill (dotcl-posix:pty-process-pid p) :int) t)
             (dotcl-posix:pty-expect p "got-int" :timeout 10)
             t)))
  t)

(deftest dotcl-posix.killed-child-reports-the-signal
  (progn
      (dotcl-posix:with-pty-process (p (dpx-sh "echo up; while :; do sleep 1; done"))
        (dotcl-posix:pty-expect p "up" :timeout 10)
        (dotcl-posix:kill (dotcl-posix:pty-process-pid p) :term)
        (multiple-value-bind (exited code sig) (dotcl-posix:pty-wait p)
          (and exited (null code) (= sig dotcl-posix:+sigterm+)))))
  t)

(deftest dotcl-posix.write-reaches-the-child
  (progn
      (dotcl-posix:with-pty-process (p (list "cat"))
        (dotcl-posix:pty-write p (format nil "hello-pty~%"))
        (and (dotcl-posix:pty-expect p "hello-pty" :timeout 10) t)))
  t)

(deftest dotcl-posix.nohang-wait-on-a-running-child
  (progn
      (dotcl-posix:with-pty-process (p (dpx-sh "echo up; while :; do sleep 1; done"))
        (dotcl-posix:pty-expect p "up" :timeout 10)
        (null (dotcl-posix:pty-wait p :nohang t))))
  t)

(deftest dotcl-posix.missing-program-is-a-posix-error
  (progn
      (handler-case (progn (dotcl-posix:with-pty-process (p (list "/nonexistent/program-zz")) p) nil)
        (dotcl-posix:posix-error (e) (and (integerp (dotcl-posix:posix-error-errno e)) t))))
  t)
