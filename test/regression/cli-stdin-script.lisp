;;; Standard input is the program when no program is named anywhere else.
;;;
;;; `dotcl -` reads the script from standard input, and so does a plain `dotcl`
;;; whose standard input is not a terminal. Before this there was no way to pipe
;;; a program in: `echo ... | dotcl` said "nothing to do" and `dotcl -` was an
;;; unknown option. With a program named (a file, --load, --eval) standard input
;;; stays the program's data, and `dotcl repl` stays a REPL.
;;;
;;; Every child here gets a pipe for standard input, written and then closed,
;;; so none of these depend on what the suite's own standard input is. The
;;; terminal case (plain `dotcl` from a console prints usage and exits 2) cannot
;;; be produced through a pipe and is not covered here.

(defvar *cli-si-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-si-core*
  (regression-child-core))

(defun %cli-si-run (args input)
  "Run the child with INPUT on a pipe as its standard input.
Returns (exit-code stdout stderr)."
  (let* ((p (dotcl:launch-process *cli-si-exe*
                                  (list* "--core" *cli-si-core* args)))
         (in (dotcl:process-input p)))
    (write-string input in)
    (close in)
    (flet ((slurp (s)
             (with-output-to-string (o)
               (loop for line = (read-line s nil nil)
                     while line
                     do (write-line (string-right-trim '(#\return) line) o)))))
      (let* ((out (slurp (dotcl:process-output p)))
             (err (slurp (dotcl:process-error p))))
        (list (dotcl:process-wait p) out err)))))

(defun %cli-si-trim (s)
  (string-trim '(#\space #\newline #\return) s))

;;; --- stdin is the program --------------------------------------------------

(deftest cli-stdin-script.dash
  (let ((r (%cli-si-run (list "-") "(print (+ 1 2))")))
    (list (first r) (%cli-si-trim (second r)) (third r)))
  (0 "3" ""))

(deftest cli-stdin-script.no-arguments
  (let ((r (%cli-si-run '() "(print (+ 1 2))")))
    (list (first r) (%cli-si-trim (second r)) (third r)))
  (0 "3" ""))

;;; Only global options: still no program named.
(deftest cli-stdin-script.global-option-only
  (let ((r (%cli-si-run (list "--no-init") "(princ :ok)")))
    (list (first r) (%cli-si-trim (second r))))
  (0 "OK"))

;;; The arguments after `-` are the script's, dashes included.
(deftest cli-stdin-script.arguments
  (let ((r (%cli-si-run (list "-" "a" "--b" "c")
                        "(prin1 (dotcl:script-arguments))")))
    (list (first r) (%cli-si-trim (second r))))
  (0 "(\"a\" \"--b\" \"c\")"))

;;; Nothing on stdin: nothing to run, and that is success (sbcl --script agrees).
(deftest cli-stdin-script.empty
  (%cli-si-run '() "")
  (0 "" ""))

;;; Same treatment as a script file: an error is reported and exits non-zero,
;;; without an interactive debugger, and what came before it has run.
(deftest cli-stdin-script.error-exits-non-zero
  (let ((r (%cli-si-run (list "-") "(princ :before) (error \"boom\") (princ :after)")))
    (list (first r)
          (%cli-si-trim (second r))
          (and (search "boom" (third r)) t)))
  (1 "BEFORE" t))

;;; Read as a stream: there is no file, so *load-pathname* is NIL.
(deftest cli-stdin-script.load-pathname
  (%cli-si-trim (second (%cli-si-run (list "-") "(prin1 *load-pathname*)")))
  "NIL")

;;; A #! line is ignored, as for a file.
(deftest cli-stdin-script.shebang
  (%cli-si-trim (second (%cli-si-run '() (format nil "#!/usr/bin/env dotcl~%(princ :ok)~%"))))
  "OK")

;;; The script can read what follows it on the same stream.
(deftest cli-stdin-script.reads-rest-of-stdin
  (%cli-si-trim (second (%cli-si-run (list "-") (format nil "(prin1 (read))~%42~%"))))
  "42")

;;; What a Windows shell can hand over on a pipe: a byte order mark first and
;;; CR LF line ends. Standard input drops the mark and reads CR LF as a newline
;;; (see stdin-bom-crlf), and the script is read through that same stream. The
;;; bytes go to the child's pipe as bytes: writing a string would re-encode it
;;; on the way, and the test could then pass whether or not the mark is handled.
(defun %cli-si-run-bytes (args bytes)
  "Run the child with the octets BYTES on its standard input.
Returns (exit-code stdout stderr)."
  (let* ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo" *cli-si-exe*))
         (path (concatenate 'string
                            (regression-temp-dir)
                            "/dotcl-cli-si-bytes.bin"))
         (buf (progn
                (with-open-file (out path :direction :output :if-exists :supersede
                                          :element-type '(unsigned-byte 8))
                  (dolist (b bytes) (write-byte b out)))
                (prog1 (dotnet:static "System.IO.File" "ReadAllBytes" path)
                  (ignore-errors (delete-file path))))))
    (dolist (a (list* "--core" *cli-si-core* args))
      (dotnet:invoke (dotnet:invoke psi "ArgumentList") "Add" a))
    (setf (dotnet:invoke psi "UseShellExecute") nil
          (dotnet:invoke psi "RedirectStandardInput") t
          (dotnet:invoke psi "RedirectStandardOutput") t
          (dotnet:invoke psi "RedirectStandardError") t)
    (let* ((p (dotnet:static "System.Diagnostics.Process" "Start" psi))
           (in (dotnet:invoke p "StandardInput")))
      (dotnet:invoke (dotnet:invoke in "BaseStream") "Write" buf 0 (length bytes))
      (dotnet:invoke in "Close")
      (let* ((err-task (dotnet:invoke (dotnet:invoke p "StandardError")
                                      "ReadToEndAsync"))
             (out (dotnet:invoke (dotnet:invoke p "StandardOutput") "ReadToEnd")))
        (dotnet:invoke p "WaitForExit")
        (list (dotnet:invoke p "ExitCode") out (dotnet:invoke err-task "Result"))))))

(deftest cli-stdin-script.bom-crlf
  (let ((r (%cli-si-run-bytes
            (list "-")
            (append '(#xEF #xBB #xBF)
                    (map 'list #'char-code "(princ (+ 1 2))") '(13 10)
                    (map 'list #'char-code "(princ (* 2 3))") '(13 10)))))
    (list (first r) (%cli-si-trim (second r)) (third r)))
  (0 "36" ""))

;;; --- stdin is data ----------------------------------------------------------

(deftest cli-stdin-script.eval-reads-stdin-as-data
  (let ((r (%cli-si-run (list "--eval" "(prin1 (read-line))") (format nil "(+ 1 2)~%"))))
    (list (first r) (%cli-si-trim (second r))))
  (0 "\"(+ 1 2)\""))

(deftest cli-stdin-script.file-reads-stdin-as-data
  (let ((path (concatenate 'string
                           (regression-temp-dir)
                           "/dotcl-cli-si.lisp")))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(prin1 (read-line))" s))
    (unwind-protect
         (let ((r (%cli-si-run (list path) (format nil "hello~%"))))
           (list (first r) (%cli-si-trim (second r))))
      (ignore-errors (delete-file path))))
  (0 "\"hello\""))

;;; `repl` is a REPL whatever its stdin is.
(deftest cli-stdin-script.repl-stays-a-repl
  (let ((r (%cli-si-run (list "repl") (format nil "(+ 1 2)~%"))))
    (list (first r) (and (search "dotcl REPL." (second r)) t)))
  (0 t))
