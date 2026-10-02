;;; LAUNCH-PROCESS with :ELEMENT-TYPE (UNSIGNED-BYTE 8) gives byte streams for
;;; its :STREAM targets. The element type was ignored, so the standard input
;;; was a character stream and WRITE-BYTE on it signalled "not a binary output
;;; stream". UIOP passes the element type through, and GrammaTech's cl-utils
;;; feeds a compressor through a flexi-stream on such a pipe.
;;;
;;; The child is another dotcl that copies its standard input to its standard
;;; output byte for byte, so this works the same on every platform.

(defvar *lpbs-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *lpbs-dir*
  (let ((dir (concatenate 'string (regression-temp-dir)
                          "/dotcl-launch-process-binary-stream-test/")))
    (ensure-directories-exist dir)
    dir))

(defun lpbs-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defvar *lpbs-copy*
  (let ((path (concatenate 'string *lpbs-dir* "copy.lisp")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(let ((in (dotnet:static \"System.Console\" \"OpenStandardInput\"))
      (out (dotnet:static \"System.Console\" \"OpenStandardOutput\")))
  (dotnet:invoke in \"CopyTo\" out)
  (dotnet:invoke out \"Flush\"))
" out))
    path))

(defvar *lpbs-bytes* '(0 1 10 13 127 128 200 255 0 65))

(defun lpbs-roundtrip (write)
  (let* ((p (dotcl:launch-process *lpbs-exe*
                                  (append (lpbs-image)
                                          (list "--no-init" "--load" *lpbs-copy*))
                                  :input :stream :output :stream :error nil
                                  :element-type '(unsigned-byte 8)))
         (in (dotcl:process-input p))
         (out (dotcl:process-output p)))
    (funcall write in)
    (close in)
    (prog1 (loop for b = (read-byte out nil nil) while b collect b)
      (dotcl:process-wait p))))

(deftest launch-process-binary-stream.write-byte
  (lpbs-roundtrip (lambda (in) (dolist (b *lpbs-bytes*) (write-byte b in))))
  (0 1 10 13 127 128 200 255 0 65))

(deftest launch-process-binary-stream.write-sequence
  (lpbs-roundtrip
   (lambda (in)
     (write-sequence (make-array (length *lpbs-bytes*)
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents *lpbs-bytes*)
                     in)))
  (0 1 10 13 127 128 200 255 0 65))

(deftest launch-process-binary-stream.element-types
  (let ((p (dotcl:launch-process *lpbs-exe*
                                 (append (lpbs-image)
                                         (list "--no-init" "--load" *lpbs-copy*))
                                 :input :stream :output :stream :error nil
                                 :element-type '(unsigned-byte 8))))
    (prog1 (list (stream-element-type (dotcl:process-input p))
                 (stream-element-type (dotcl:process-output p)))
      (close (dotcl:process-input p))
      (dotcl:process-wait p)))
  ((unsigned-byte 8) (unsigned-byte 8)))
