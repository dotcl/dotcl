;;; Strings that are not plain CHARACTER simple strings (base strings, which
;;; UIOP:STRCAT returns for ASCII text, fill-pointer and displaced strings)
;;; reach .NET as their characters: program, arguments, directory, file
;;; redirection, environment variables, names.

(defun pbs-base (s) (coerce s 'simple-base-string))
(defun pbs-fill (s)
  (make-array (length s) :element-type 'character :fill-pointer (length s)
                         :initial-contents s))
(defun pbs-displaced (s)
  (make-array (length s) :element-type 'character
                         :displaced-to (concatenate 'string "zz" s)
                         :displaced-index-offset 2))

(defvar *pbs-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-process-base-string/")))
    (ensure-directories-exist dir)
    dir))

(deftest process-base-string.run-process
  (loop for f in (list #'pbs-base #'pbs-fill #'pbs-displaced)
        collect (first (dotcl:run-process (funcall f "dotnet")
                                          (list (funcall f "--version")))))
  (0 0 0))

(deftest process-base-string.launch-process
  (loop for f in (list #'pbs-base #'pbs-fill #'pbs-displaced)
        collect (let* ((p (dotcl:launch-process (funcall f "dotnet")
                                                (list (funcall f "--version"))
                                                :directory (funcall f *pbs-dir*)))
                       (line (read-line (dotcl:process-output p) nil "")))
                  (list (dotcl:process-wait p) (plusp (length line)))))
  ((0 t) (0 t) (0 t)))

(deftest process-base-string.output-file
  (let ((out (concatenate 'string *pbs-dir* "out.txt")))
    (dotcl:process-wait
     (dotcl:launch-process "dotnet" '("--version")
                           :output (pbs-base out) :if-output-exists :supersede))
    (with-open-file (s out) (plusp (length (read-line s nil "")))))
  t)

(deftest process-base-string.environment-variables
  (progn
    (dotcl:setenv (pbs-base "DOTCL_PBS_TEST") (pbs-fill "value"))
    (list (dotcl:getenv (pbs-displaced "DOTCL_PBS_TEST"))
          (progn (dotcl:unsetenv (pbs-base "DOTCL_PBS_TEST"))
                 (dotcl:getenv "DOTCL_PBS_TEST"))))
  ("value" nil))

(deftest process-base-string.names
  (list (%thread-name (%make-thread (lambda () 1) :name (pbs-base "thr")))
        (load-logical-pathname-translations (pbs-base "SYS"))
        (not (null (member 'car (apropos-list "CAR" (pbs-base "CL")))))
        (dotnet:invoke (dotnet:new "System.Text.StringBuilder" (pbs-base "sb"))
                       "ToString"))
  ("thr" nil t "sb"))
