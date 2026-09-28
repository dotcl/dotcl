;;; RUN-PROGRAM with :INPUT naming a file hands the child the file's bytes
;;; unchanged, as SBCL does.
;;;
;;; The file used to be read as text and written again as text on the way to
;;; the child, so a leading byte order mark disappeared and bytes that are not
;;; valid UTF-8 were replaced.
;;;
;;; The child is another dotcl that reads its standard input as raw bytes and
;;; prints them as decimal numbers, which works the same on every platform.

(require "asdf")

(defvar *rpib-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rpib-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-run-program-input-bytes-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rpib-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defvar *rpib-script*
  (let ((path (concatenate 'string *rpib-dir* "dump.lisp")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(let ((s (dotnet:static \"System.Console\" \"OpenStandardInput\")))
  (format t \"BYTES(~{~D~^ ~})~%\"
          (loop for b = (dotnet:invoke s \"ReadByte\")
                until (= b -1) collect b)))
" out))
    path))

(defun rpib-dump (bytes)
  "Write BYTES to a file, run the child with that file as :INPUT, and return
the list of bytes the child read from its standard input."
  (let ((input (concatenate 'string *rpib-dir* "input.bin")))
    (with-open-file (out input :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
      (dolist (b bytes) (write-byte b out)))
    (let* ((out (uiop:run-program (append (list *rpib-exe*) (rpib-image)
                                          (list "--no-init" "--load" *rpib-script*))
                                  :input (pathname input)
                                  :output :string
                                  :ignore-error-status t))
           (start (search "BYTES(" out))
           (end (and start (position #\) out :start start))))
      (and start end
           (read-from-string
            (concatenate 'string "(" (subseq out (+ start 6) end) ")"))))))

;;; A UTF-8 byte order mark at the start of the file reaches the child.
(defvar *rpib-bom* '(#xEF #xBB #xBF 40 43 32 49 32 50 41 13 10))

(deftest run-program-input-file-keeps-bom
  (equal (rpib-dump *rpib-bom*) *rpib-bom*)
  t)

;;; Bytes that do not decode as UTF-8 (a UTF-16 mark, a lone continuation
;;; byte, NUL, a truncated sequence, high bytes) reach the child unchanged.
(defvar *rpib-binary* '(#xFF #xFE #x80 #x00 #xC3 #x28 #x0D #x0A #x1A #xFF #x00 #x7F #xFE))

(deftest run-program-input-file-keeps-non-utf8-bytes
  (equal (rpib-dump *rpib-binary*) *rpib-binary*)
  t)
