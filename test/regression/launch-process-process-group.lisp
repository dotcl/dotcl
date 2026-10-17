;;; On Unix a child of LAUNCH-PROCESS leads a process group of its own unless
;;; it shares the caller's standard input (:input t), as with SBCL's
;;; RUN-PROGRAM. Killing the child's group then leaves the caller running.

(require "asdf")

#-windows
(defun lpg-pgid (pid)
  (let* ((p (dotcl:launch-process "ps" (list "-o" "pgid=" "-p" (princ-to-string pid))
                                  :input t))
         (line (read-line (dotcl:process-output p) nil "")))
    (dotcl:process-wait p)
    (parse-integer line :junk-allowed t)))

#-windows
(defun lpg-child-report (input)
  (let* ((p (dotcl:launch-process "sh" '("-c" "ps -o pgid= -p $$; echo out; exit 7")
                                  :input input))
         (lines (loop for l = (read-line (dotcl:process-output p) nil) while l collect l))
         (code (dotcl:process-wait p)))
    (list (dotcl:process-pid p)
          (parse-integer (first lines) :junk-allowed t)
          (rest lines)
          code)))

#-windows
(deftest launch-process-own-process-group
  (let ((self (lpg-pgid (dotcl:getpid))))
    (loop for input in (list :stream nil)
          collect (destructuring-bind (pid pgid out code) (lpg-child-report input)
                    (list (= pgid pid) (/= pgid self) out code))))
  ((t t ("out") 7) (t t ("out") 7)))

#-windows
(deftest launch-process-input-t-shares-process-group
  (let ((self (lpg-pgid (dotcl:getpid))))
    (destructuring-bind (pid pgid out code) (lpg-child-report t)
      (list (/= pgid pid) (= pgid self) out code)))
  (t t ("out") 7))

#-windows
(deftest run-program-own-process-group
  (let ((self (lpg-pgid (dotcl:getpid))))
    (list (/= self (parse-integer (uiop:run-program "ps -o pgid= -p $$" :output :string)
                                  :junk-allowed t))
          (= self (parse-integer (uiop:run-program "ps -o pgid= -p $$" :output :string
                                                   :input :interactive)
                                 :junk-allowed t))))
  (t t))

#-windows
(deftest launch-process-kill-child-group-spares-caller
  (let* ((p (dotcl:launch-process "sleep" '("30") :output nil))
         (pid (dotcl:process-pid p))
         (pgid (lpg-pgid pid)))
    (if (and (eql pgid pid) (/= pgid (lpg-pgid (dotcl:getpid))))
        (let ((k (dotcl:launch-process "sh" (list "-c" "kill -s TERM -- \"-$1\"" "sh"
                                                  (princ-to-string pgid))
                                       :input t :output nil)))
          (list (dotcl:process-wait k) (dotcl:process-wait p)))
        (progn (dotcl:process-kill p) (dotcl:process-wait p) :same-group)))
  (0 143))
