;;; LAUNCH-PROCESS takes any Lisp stream for :INPUT, :OUTPUT and :ERROR, as
;;; sb-ext:run-program does: a string stream, *STANDARD-OUTPUT*, a synonym
;;; stream, a Gray stream. The runtime copies between the pipe and the stream
;;; in a thread, and PROCESS-WAIT returns once the copies are done. Before,
;;; such a stream was taken for an unknown spec, and libraries (external-program,
;;; trivial-shell) carried their own pipe-and-thread copies.
;;;
;;; The child is another dotcl, so this works the same on every platform.

(defvar *lpls-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *lpls-dir*
  (let ((dir (concatenate 'string (regression-temp-dir)
                          "/dotcl-launch-process-lisp-streams-test/")))
    (ensure-directories-exist dir)
    dir))

(defun lpls-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (or (loop for i below (1- count)
              when (equal (dotnet:invoke args "GetValue" i) "--core")
                return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))
        (list "--core" (regression-child-core)))))

(defun lpls-script (name text)
  (let ((path (concatenate 'string *lpls-dir* name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

;; Copies its standard input to its standard output, byte for byte.
(defvar *lpls-copy*
  (lpls-script "copy.lisp"
               "(let ((in (dotnet:static \"System.Console\" \"OpenStandardInput\"))
      (out (dotnet:static \"System.Console\" \"OpenStandardOutput\")))
  (dotnet:invoke in \"CopyTo\" out)
  (dotnet:invoke out \"Flush\"))
"))

;; Writes "out" to its standard output and "err" to its standard error.
(defvar *lpls-both*
  (lpls-script "both.lisp"
               "(write-line \"out\" *standard-output*)
(finish-output *standard-output*)
(write-line \"err\" *error-output*)
(finish-output *error-output*)
"))

(defun lpls-run (script &rest keys)
  (let ((p (apply #'dotcl:launch-process *lpls-exe*
                  (append (lpls-image) (list "--no-init" "--load" script))
                  keys)))
    (dotcl:process-wait p)))

(deftest launch-process-lisp-streams.output-to-string-stream
  (let ((code nil))
    (list (with-output-to-string (s)
            (setf code (lpls-run *lpls-both* :input nil :output s :error nil)))
          code))
  ("out
" 0))

(deftest launch-process-lisp-streams.error-to-string-stream
  (with-output-to-string (s)
    (lpls-run *lpls-both* :input nil :output nil :error s))
  "err
")

(deftest launch-process-lisp-streams.input-from-string-stream
  ;; The last line has no newline and must arrive without one.
  (with-output-to-string (out)
    (with-input-from-string (in (format nil "x~%y~%z"))
      (lpls-run *lpls-copy* :input in :output out :error nil)))
  "x
y
z")

(deftest launch-process-lisp-streams.error-to-output
  ;; :error :output sends the error output to the same Lisp stream.
  (let ((text (with-output-to-string (s)
                (lpls-run *lpls-both* :input nil :output s :error :output))))
    (sort (list (subseq text 0 3) (subseq text 4 7)) #'string<))
  ("err" "out"))

(defvar *lpls-target*)

(deftest launch-process-lisp-streams.synonym-stream
  ;; The synonym stream is resolved in the caller's thread, where the binding is.
  (with-output-to-string (*lpls-target*)
    (lpls-run *lpls-both* :input nil
                          :output (make-synonym-stream '*lpls-target*)
                          :error nil))
  "out
")

(deftest launch-process-lisp-streams.standard-output
  (with-output-to-string (*standard-output*)
    (lpls-run *lpls-both* :input nil :output *standard-output* :error nil))
  "out
")

(deftest launch-process-lisp-streams.octets-through-file-streams
  ;; Binary Lisp streams are copied as octets.
  (let ((in-path (concatenate 'string *lpls-dir* "in.bin"))
        (out-path (concatenate 'string *lpls-dir* "out.bin"))
        (bytes '(0 1 10 13 127 128 200 255)))
    (with-open-file (o in-path :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
      (dolist (b bytes) (write-byte b o)))
    (with-open-file (in in-path :element-type '(unsigned-byte 8))
      (with-open-file (out out-path :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
        (lpls-run *lpls-copy* :input in :output out :error nil)))
    (with-open-file (in out-path :element-type '(unsigned-byte 8))
      (loop for b = (read-byte in nil nil) while b collect b)))
  (0 1 10 13 127 128 200 255))

(deftest launch-process-lisp-streams.closed-target-signals-on-wait
  ;; A copy that cannot write to its target is reported by PROCESS-WAIT.
  (let ((s (make-string-output-stream)))
    (close s)
    (handler-case (progn (lpls-run *lpls-both* :input nil :output s :error nil) :no-error)
      (error () :error)))
  :error)
