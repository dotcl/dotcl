;;; Standard input that starts with a byte order mark and ends its lines with
;;; CR LF, which is what a Windows shell can hand a program on a pipe.
;;;
;;; Both used to reach Lisp as characters. The mark became the first
;;; character of the first token, so a REPL fed (+ 1 2) reported an unbound
;;; variable whose name printed as nothing at all; the CR stayed on the end of
;;; every line READ-LINE returned. Standard input now drops one leading U+FEFF
;;; and reads CR LF as a newline, as Python and Node do.
;;;
;;; Checked by starting another process with those bytes on its standard input.

(defvar *sbc-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *sbc-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-stdin-bom-crlf-test/")))
    (ensure-directories-exist dir)
    dir))

(defun sbc-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun sbc-input-file ()
  "A file holding EF BB BF, then (+ 1 2) CR LF, then (* 2 3) CR LF."
  (let ((path (concatenate 'string *sbc-dir* "input.txt")))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (dolist (b (append '(#xEF #xBB #xBF)
                         (map 'list #'char-code "(+ 1 2)") '(13 10)
                         (map 'list #'char-code "(* 2 3)") '(13 10)))
        (write-byte b out)))
    path))

(defun sbc-run (args)
  "Run dotcl with ARGS and the bytes of the input file on standard input.
Returns standard output and standard error.

The child is started through System.Diagnostics.Process and the file copied
to its standard input as bytes. RUN-PROGRAM's :INPUT decodes the file and
encodes it again on the way, and the mark does not survive that, so a test
written with it passes whether or not the mark is handled."
  (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo" *sbc-exe*)))
    (dolist (a (append (sbc-image) (list "--no-init") args))
      (dotnet:invoke (dotnet:invoke psi "ArgumentList") "Add" a))
    (setf (dotnet:invoke psi "UseShellExecute") nil
          (dotnet:invoke psi "RedirectStandardInput") t
          (dotnet:invoke psi "RedirectStandardOutput") t
          (dotnet:invoke psi "RedirectStandardError") t)
    (let* ((p (dotnet:static "System.Diagnostics.Process" "Start" psi))
           (in (dotnet:invoke p "StandardInput"))
           (bytes (dotnet:static "System.IO.File" "ReadAllBytes"
                                 (sbc-input-file))))
      (dotnet:invoke (dotnet:invoke in "BaseStream") "Write" bytes 0
                     (dotnet:invoke bytes "Length"))
      (dotnet:invoke in "Close")
      (let* ((err-task (dotnet:invoke (dotnet:invoke p "StandardError")
                                      "ReadToEndAsync"))
             (out (dotnet:invoke (dotnet:invoke p "StandardOutput") "ReadToEnd")))
        (dotnet:invoke p "WaitForExit")
        (values out (dotnet:invoke err-task "Result"))))))

(defvar *sbc-script*
  (let ((path (concatenate 'string *sbc-dir* "lines.lisp")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(print (map 'list #'char-code (read-line)))
(print (map 'list #'char-code (read-line)))
(print (read-line *standard-input* nil :eof))
(terpri)
" out))
    path))

(defvar *sbc-repl-out*)
(defvar *sbc-repl-err*)
(multiple-value-setq (*sbc-repl-out* *sbc-repl-err*) (sbc-run (list "repl")))

(defvar *sbc-lines-out* (sbc-run (list "--load" *sbc-script*)))

;;; The REPL reads the first form as (+ 1 2), not as a symbol with an
;;; invisible name in front of it, and goes on to the second.
(deftest sbc-repl-first-form
  (and (search "CL-USER> 3" *sbc-repl-out*) t)
  t)

(deftest sbc-repl-second-form
  (and (search "CL-USER> 6" *sbc-repl-out*) t)
  t)

(deftest sbc-repl-no-unbound-variable
  (search "UNBOUND-VARIABLE" *sbc-repl-err*)
  nil)

;;; READ-LINE on standard input: no mark in front of the first line, no CR at
;;; the end of either, and the input ends where the bytes do.
(deftest sbc-read-line-first
  (and (search "(40 43 32 49 32 50 41)" *sbc-lines-out*) t)
  t)

(deftest sbc-read-line-second
  (and (search "(40 42 32 50 32 51 41)" *sbc-lines-out*) t)
  t)

(deftest sbc-read-line-then-eof
  (and (search ":EOF" *sbc-lines-out*) t)
  t)
