;;; OPEN :IF-EXISTS :SUPERSEDE decides whether an existing file may be replaced by
;;; whether this process can write it, as the system answers (so root may replace a
;;; file another user owns), not by the read-only attribute derived from the mode.

#-windows
(defun %suw-chmod (mode path)
  (dotcl:process-wait
   (dotcl:launch-process "chmod" (list mode path)
                         :output (make-broadcast-stream) :input nil :error nil)))

#-windows
(defun %suw-root-p ()
  (let ((out (make-string-output-stream)))
    (dotcl:process-wait
     (dotcl:launch-process "id" (list "-u") :output out :input nil :error nil))
    (string= (string-trim '(#\Newline #\Space) (get-output-stream-string out)) "0")))

#-windows
(deftest supersede-unix-write-permission.writable
  (let* ((dir (regression-temp-dir))
         (f (concatenate 'string dir "/suw-a.txt")))
    (with-open-file (s f :direction :output :if-exists :supersede) (write-string "old" s))
    (%suw-chmod "644" f)
    (with-open-file (s f :direction :output :if-exists :supersede) (write-string "new" s))
    (with-open-file (s f) (values (read-line s))))
  "new")

#-windows
(deftest supersede-unix-write-permission.mode-0444
  ;; A process that cannot write the file gets a FILE-ERROR; root can, as SBCL's does.
  (let* ((dir (regression-temp-dir))
         (f (concatenate 'string dir "/suw-b.txt")))
    (with-open-file (s f :direction :output :if-exists :supersede) (write-string "old" s))
    (%suw-chmod "444" f)
    (let ((r (handler-case
                 (progn (with-open-file (s f :direction :output :if-exists :supersede)
                          (write-string "new" s))
                        :replaced)
               (file-error () :file-error))))
      (%suw-chmod "644" f)
      (if (%suw-root-p)
          (eq r :replaced)
          (eq r :file-error))))
  t)
