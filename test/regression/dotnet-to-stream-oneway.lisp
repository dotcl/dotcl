;;; Wrapping a one-way .NET stream.
;;;
;;; dotnet:to-stream was written for sockets, where the .NET Stream reads and
;;; writes, and it built a StreamReader and a StreamWriter over it regardless.
;;; A stream that only goes one way -- File.OpenRead, File.OpenWrite, an HTTP
;;; request or response body -- threw out of the constructor before any Lisp code
;;; ran: "Stream was not writable" / "Stream was not readable". The direction is
;;; now read off the stream, so each of those becomes the matching CL stream.
;;;
;;; The wrapper leaves the underlying .NET stream open (a socket must outlive the
;;; wrapper), so these tests dispose it themselves.

(defvar *dtso-dir*
  (substitute #\/ #\ (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp")))

(defun dtso-path (name) (concatenate 'string *dtso-dir* "/" name))

(with-open-file (s (dtso-path "dotcl-to-stream-oneway.txt")
                   :direction :output :if-exists :supersede)
  (write-line "line one" s))

(defmacro dtso-with-stream ((var net-form) &body body)
  "Bind VAR to the CL stream over NET-FORM, closing both on the way out."
  (let ((net (gensym)))
    `(let* ((,net ,net-form) (,var (dotnet:to-stream ,net)))
       (unwind-protect (progn ,@body)
         (ignore-errors (close ,var))
         (dotnet:invoke ,net "Dispose")))))

(defun dtso-open-read ()
  (dotnet:static "System.IO.File" "OpenRead" (dtso-path "dotcl-to-stream-oneway.txt")))

(defun dtso-open-write ()
  ;; OpenWrite is write-only; Create returns a stream that also reads.
  (let ((path (dtso-path "dotcl-to-stream-oneway-out.txt")))
    (dotnet:static "System.IO.File" "Delete" path)
    (dotnet:static "System.IO.File" "OpenWrite" path)))

;;; Read-only: a CL input stream, and only an input stream.
(deftest dtso-read-only-reads
  (dtso-with-stream (in (dtso-open-read)) (values (read-line in nil :eof)))
  "line one")

(deftest dtso-read-only-is-input-only
  (dtso-with-stream (in (dtso-open-read))
    (list (input-stream-p in) (output-stream-p in)))
  (t nil))

;;; Write-only: a CL output stream, and what was written arrives.
(deftest dtso-write-only-writes
  (progn
    (dtso-with-stream (out (dtso-open-write)) (write-string "written" out))
    (with-open-file (s (dtso-path "dotcl-to-stream-oneway-out.txt"))
      (values (read-line s nil :eof))))
  "written")

(deftest dtso-write-only-is-output-only
  (dtso-with-stream (out (dtso-open-write))
    (list (input-stream-p out) (output-stream-p out)))
  (nil t))

;;; A stream that goes both ways still gets both halves.
(deftest dtso-two-way-unchanged
  (dtso-with-stream (both (dotnet:new "System.IO.MemoryStream"))
    (list (input-stream-p both) (output-stream-p both)))
  (t t))

;;; :binary already worked on a one-way stream; keep it that way.
(deftest dtso-binary-read-only
  (let* ((net (dtso-open-read)) (in (dotnet:to-stream net :binary t)))
    (unwind-protect (read-byte in nil :eof)
      (ignore-errors (close in))
      (dotnet:invoke net "Dispose")))
  108)
