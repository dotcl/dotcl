;;; RUN-PROGRAM / LAUNCH-PROCESS honour :EXTERNAL-FORMAT for the child's
;;; standard input, output and error output.
;;;
;;; It used to be ignored: the pipes were always decoded with the .NET default
;;; (UTF-8 once dotcl sets the console code page), so a child writing cp932,
;;; like the console tools of Japanese Windows, came back as U+FFFD and junk.
;;;
;;; The child is another dotcl that writes fixed bytes, or reads its standard
;;; input as raw bytes and prints them as decimal numbers, which works the same
;;; on every platform. Non-ASCII text is built from code points so this file
;;; stays ASCII.

(require "asdf")

(defvar *rpef-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rpef-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-run-program-external-format-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rpef-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rpef-script (name text)
  (let ((path (concatenate 'string *rpef-dir* name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

;; U+65E5 U+672C U+8A9E ("Japanese" in Japanese) and its cp932 bytes.
(defvar *rpef-nihongo*
  (coerce (mapcar #'code-char '(#x65E5 #x672C #x8A9E)) 'string))
(defvar *rpef-cp932* '(#x93 #xFA #x96 #x7B #x8C #xEA))

;;; A child that writes the cp932 bytes to stdout and to stderr.
(defvar *rpef-emit*
  (rpef-script "emit.lisp"
               (format nil "(dolist (name '(\"OpenStandardOutput\" \"OpenStandardError\"))
  (let ((s (dotnet:static \"System.Console\" name)))
    (dolist (b '~S) (dotnet:invoke s \"WriteByte\" b))
    (dotnet:invoke s \"Flush\")))
" *rpef-cp932*)))

;;; A child that prints the bytes of its standard input.
(defvar *rpef-dump*
  (rpef-script "dump.lisp" "(let ((s (dotnet:static \"System.Console\" \"OpenStandardInput\")))
  (format t \"BYTES(~{~D~^ ~})~%\"
          (loop for b = (dotnet:invoke s \"ReadByte\")
                until (= b -1) collect b)))
"))

(defun rpef-command (script)
  (append (list *rpef-exe*) (rpef-image) (list "--no-init" "--load" script)))

(defun rpef-parse-bytes (out)
  (let* ((start (search "BYTES(" out))
         (end (and start (position #\) out :start start))))
    (and start end
         (values (read-from-string
                  (concatenate 'string "(" (subseq out (+ start 6) end) ")"))))))

;;; :output :string and :error-output :string decode cp932.
(deftest run-program-external-format-cp932-output
  (multiple-value-bind (out err)
      (uiop:run-program (rpef-command *rpef-emit*)
                        :output :string :error-output :string
                        :external-format :cp932 :ignore-error-status t)
    (list (and (search *rpef-nihongo* out) t)
          (and (search *rpef-nihongo* err) t)))
  (t t))

;;; :external-format :utf-8 is applied too (the same bytes do not decode).
(deftest run-program-external-format-utf8-is-applied
  (let ((out (uiop:run-program (rpef-command *rpef-emit*)
                               :output :string :error-output nil
                               :external-format :utf-8 :ignore-error-status t)))
    (list (and (search *rpef-nihongo* out) t)
          (and (find (code-char #xFFFD) out) t)))
  (nil t))

;;; :output to a file keeps the bytes the child wrote.
(deftest run-program-external-format-output-file-bytes
  (let ((path (concatenate 'string *rpef-dir* "out.bin")))
    (uiop:run-program (rpef-command *rpef-emit*)
                      :output (pathname path) :error-output nil
                      :external-format :cp932 :ignore-error-status t)
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (let ((bytes (loop for b = (read-byte s nil nil) while b collect b)))
        (equal (subseq bytes 0 (min 6 (length bytes))) *rpef-cp932*))))
  t)

;;; :input given as a string reaches the child encoded as cp932.
(deftest run-program-external-format-cp932-input
  (with-input-from-string (in *rpef-nihongo*)
    (rpef-parse-bytes
     (uiop:run-program (rpef-command *rpef-dump*)
                       :input in :output :string
                       :external-format :cp932 :ignore-error-status t)))
  (#x93 #xFA #x96 #x7B #x8C #xEA))

;;; LAUNCH-PROCESS directly: the :input :stream writer uses the format too.
(deftest launch-process-external-format-input-stream
  (let* ((p (dotcl:launch-process *rpef-exe*
                                  (append (rpef-image)
                                          (list "--no-init" "--load" *rpef-dump*))
                                  :input :stream :output :stream :error nil
                                  :external-format :cp932))
         (in (dotcl:process-input p)))
    (write-string *rpef-nihongo* in)
    (close in)
    (let ((out (with-output-to-string (o)
                 (loop for line = (read-line (dotcl:process-output p) nil nil)
                       while line do (write-line line o)))))
      (dotcl:process-wait p)
      (rpef-parse-bytes out)))
  (#x93 #xFA #x96 #x7B #x8C #xEA))

;;; Windows: cmd's TYPE copies a cp932 file to the pipe as it is, which is what
;;; cmd built-ins and console tools do on Japanese Windows.
(deftest run-program-external-format-cmd-type
  (if (uiop:os-windows-p)
      (let ((path (concatenate 'string *rpef-dir* "cp932.txt")))
        (with-open-file (s path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
          (dolist (b *rpef-cp932*) (write-byte b s)))
        (let ((out (uiop:run-program (list "cmd" "/c" "type"
                                           (uiop:native-namestring path))
                                     :output :string :external-format :cp932)))
          (string= out *rpef-nihongo*)))
      t)
  t)
