;;; LISTEN and READ-CHAR-NO-HANG on standard input answer NIL at once while an
;;; open pipe has nothing in it, instead of waiting for the writer; the child
;;; is another dotcl whose standard input is a pipe the test holds open.
;;; The child exits by itself after a watchdog delay, so a regression of this
;;; fails the test rather than hanging the suite.

(defvar *snh-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *snh-dir*
  (let ((dir (concatenate 'string (regression-temp-dir)
                          "/dotcl-stdin-no-hang-test/")))
    (ensure-directories-exist dir)
    dir))

(defun snh-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (or (loop for i below (1- count)
              when (equal (dotnet:invoke args "GetValue" i) "--core")
                return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))
        (list "--core" (regression-child-core)))))

(defvar *snh-script*
  (let ((path (concatenate 'string *snh-dir* "child.lisp")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(require \"dotcl-thread\")
(dotcl-thread:make-thread
 (lambda () (sleep 25) (dotnet:static \"System.Environment\" \"Exit\" 3)))
(defun out (x) (format t \"~S~%\" x) (finish-output))
(defun wait-for (fn)
  (loop for v = (funcall fn) when v return v do (sleep 0.02)))
(out (list :listen (listen)))
(out (list :rcnh (read-char-no-hang nil nil :eof)))
(out (list :listen-after (wait-for (lambda () (and (listen) :t)))))
(out (list :c1 (read-char-no-hang nil nil :eof)))
(out (list :c2 (read-char-no-hang nil nil :eof)))
(out (list :open-empty (listen) (read-char-no-hang nil nil :eof)))
(out (list :end (wait-for (lambda () (read-char-no-hang nil nil :eof)))))
(out (list :listen-eof (listen)))
(out (list :eof-error (handler-case (read-char-no-hang) (end-of-file () :eof-error))))
" out))
    path))

(deftest stdin-no-hang.open-pipe
  (let* ((p (dotcl:launch-process *snh-exe*
                                  (append (snh-image) (list "--no-init" "--load" *snh-script*))))
         (in (dotcl:process-input p))
         (out (dotcl:process-output p)))
    (flet ((line () (let ((l (read-line out nil nil))) (and l (read-from-string l)))))
      (let ((r (list (line) (line))))
        (write-string "ab" in)
        (finish-output in)
        (setf r (append r (list (line) (line) (line) (line))))
        (close in)
        (setf r (append r (list (line) (line) (line))))
        (dotcl:process-wait p)
        r)))
  ((:listen nil) (:rcnh nil) (:listen-after :t) (:c1 #\a) (:c2 #\b) (:open-empty nil nil)
   (:end :eof) (:listen-eof nil) (:eof-error :eof-error)))
