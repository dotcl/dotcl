;;; LAUNCH-PROCESS :ON-EXIT FUNCTION calls FUNCTION with the process, in a new
;;; Lisp thread, once the child has exited and the copies between it and Lisp
;;; streams are done. An error in FUNCTION ends only that thread, and
;;; PROCESS-WAIT still returns the exit code. The child is another dotcl, so
;;; this works the same on every platform.

(defvar *lpoe-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *lpoe-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-launch-process-on-exit-test/")))
    (ensure-directories-exist dir)
    dir))

(defun lpoe-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (or (loop for i below (1- count)
              when (equal (dotnet:invoke args "GetValue" i) "--core")
                return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))
        (list "--core" (regression-child-core)))))

(defun lpoe-script (name text)
  (let ((path (concatenate 'string *lpoe-dir* name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

(defvar *lpoe-exit0* (lpoe-script "exit0.lisp" "(write-line \"hello\" *standard-output*)
(finish-output *standard-output*)
"))
(defvar *lpoe-exit3* (lpoe-script "exit3.lisp" "(dotcl:quit 3)
"))
(defvar *lpoe-sleep* (lpoe-script "sleep.lisp" "(sleep 120)
"))

(defun lpoe-launch (script &rest keys)
  (apply #'dotcl:launch-process *lpoe-exe*
         (append (lpoe-image) (list "--no-init" "--load" script))
         keys))

(defun lpoe-await (cell &optional (seconds 30))
  "The value CELL holds once a callback has set it, or :TIMEOUT."
  (loop repeat (* seconds 50)
        until (car cell) do (sleep 0.02))
  (or (car cell) :timeout))

(deftest launch-process-on-exit.normal-exit
  (let* ((cell (list nil))
         (p (lpoe-launch *lpoe-exit0*
                         :on-exit (lambda (proc)
                                    (setf (car cell)
                                          (list (dotcl:process-alive-p proc)
                                                (dotcl:process-exit-code proc)))))))
    (list (lpoe-await cell)
          (dotcl:process-wait p)))
  ((nil 0) 0))

(deftest launch-process-on-exit.non-zero-exit
  (let* ((cell (list nil))
         (p (lpoe-launch *lpoe-exit3*
                         :on-exit (lambda (proc)
                                    (setf (car cell) (list (dotcl:process-exit-code proc)))))))
    (list (lpoe-await cell)
          (dotcl:process-wait p)))
  ((3) 3))

(deftest launch-process-on-exit.callback-gets-the-process
  (let* ((cell (list nil))
         (p (lpoe-launch *lpoe-exit0* :on-exit (lambda (proc) (setf (car cell) (list proc))))))
    (eq (first (lpoe-await cell)) p))
  t)

(deftest launch-process-on-exit.kill
  (let* ((cell (list nil))
         (p (lpoe-launch *lpoe-sleep*
                         :on-exit (lambda (proc)
                                    (setf (car cell) (list (dotcl:process-alive-p proc)))))))
    (sleep 0.5)
    (dotcl:process-kill p)
    (list (lpoe-await cell)
          (dotcl:process-alive-p p)))
  ((nil) nil))

(deftest launch-process-on-exit.several-processes-each-their-own-callback
  (let* ((cells (loop repeat 3 collect (list nil)))
         (ps (loop for cell in cells
                   for script in (list *lpoe-exit0* *lpoe-exit3* *lpoe-exit0*)
                   collect (let ((cell cell))
                             (lpoe-launch script
                                          :on-exit (lambda (proc)
                                                     (setf (car cell)
                                                           (list (dotcl:process-exit-code proc)))))))))
    (prog1 (mapcar #'lpoe-await cells)
      (mapc #'dotcl:process-wait ps)))
  ((0) (3) (0)))

(deftest launch-process-on-exit.output-copies-are-done
  (let* ((cell (list nil))
         (out (make-string-output-stream))
         (p (lpoe-launch *lpoe-exit0* :output out
                         :on-exit (lambda (proc)
                                    (declare (ignore proc))
                                    (setf (car cell) (list (get-output-stream-string out)))))))
    (dotcl:process-wait p)
    (let ((text (first (lpoe-await cell))))
      (and (stringp text) (search "hello" text) t)))
  t)

;; An error in the callback ends its own thread only. A later PROCESS-WAIT
;; still returns, and another process's callback still runs.
(deftest launch-process-on-exit.error-in-callback-is-contained
  (let* ((cell (list nil))
         (*error-output* (make-broadcast-stream))
         (p1 (lpoe-launch *lpoe-exit3* :on-exit (lambda (proc) (declare (ignore proc)) (error "boom"))))
         (code (dotcl:process-wait p1))
         (p2 (lpoe-launch *lpoe-exit0* :on-exit (lambda (proc)
                                                  (declare (ignore proc))
                                                  (setf (car cell) (list :ran))))))
    (list code (lpoe-await cell) (dotcl:process-wait p2)))
  (3 (:ran) 0))

(deftest launch-process-on-exit.bad-argument
  (list (handler-case (lpoe-launch *lpoe-exit0* :on-exit 5) (type-error () :type-error))
        (handler-case (progn (dotcl:process-wait (lpoe-launch *lpoe-exit0* :on-exit nil)) :ok)
          (error () :error)))
  (:type-error :ok))
