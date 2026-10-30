;;; File opening, abortive close, RENAME-FILE, DIRECTORY types and binary LISTEN.
;;;
;;; OPEN :IF-EXISTS :RENAME must keep the old file; CLOSE :ABORT must remove a
;;; file it created or restore one it superseded; RENAME-FILE failure is a
;;; FILE-ERROR; DIRECTORY's "*" has a NIL type and so does not match "d.txt";
;;; and LISTEN on a binary file must not consume a byte.

(defun sff-path (name)
  (regression-temp-file name))

(defun sff-text (path)
  (when (probe-file path)
    (values (with-open-file (s path :direction :input) (read-line s)))))

(deftest file-open-rename-keeps-old
  (let* ((f (sff-path "sff-rename.txt"))
         (bak (concatenate 'string f ".bak")))
    (ignore-errors (delete-file f))
    (ignore-errors (delete-file bak))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line "OLD" s))
    (with-open-file (s f :direction :output :if-exists :rename
                         :if-does-not-exist :create)
      (write-line "NEW" s))
    (list (sff-text f) (sff-text bak)))
  ("NEW" "OLD"))

(deftest file-open-rename-io-keeps-old
  (let* ((f (sff-path "sff-rename-io.txt"))
         (bak (concatenate 'string f ".bak")))
    (ignore-errors (delete-file f))
    (ignore-errors (delete-file bak))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line "OLD" s))
    (with-open-file (s f :direction :io :if-exists :rename
                         :if-does-not-exist :create)
      (write-line "NEW" s))
    (list (sff-text f) (sff-text bak)))
  ("NEW" "OLD"))

(deftest file-close-abort-restores-renamed
  (let* ((f (sff-path "sff-abort-renamed.txt"))
         (bak (concatenate 'string f ".bak")))
    (ignore-errors (delete-file f))
    (ignore-errors (delete-file bak))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line "OLD" s))
    (let ((s (open f :direction :output :if-exists :rename
                     :if-does-not-exist :create)))
      (write-line "PARTIAL" s)
      (close s :abort t))
    (list (sff-text f) (and (probe-file bak) t)))
  ("OLD" nil))

(deftest file-close-abort-deletes-created
  (let ((f (sff-path "sff-abort-created.txt")))
    (ignore-errors (delete-file f))
    (let ((s (open f :direction :output :if-exists :supersede
                     :if-does-not-exist :create)))
      (write-line "NEW" s)
      (close s :abort t))
    (and (probe-file f) t))
  nil)

(deftest file-close-abort-restores-superseded
  (let ((f (sff-path "sff-abort-superseded.txt")))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line "OLD" s))
    (let ((s (open f :direction :output :if-exists :supersede
                     :if-does-not-exist :create)))
      (write-line "PARTIAL" s)
      (close s :abort t))
    (list (and (probe-file f) t) (sff-text f)))
  (t "OLD"))

(deftest with-open-file-abnormal-exit-deletes-created
  (let ((f (sff-path "sff-woff-created.txt")))
    (ignore-errors (delete-file f))
    (catch 'out
      (with-open-file (s f :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
        (write-line "PARTIAL" s)
        (throw 'out :thrown)))
    (and (probe-file f) t))
  nil)

(deftest with-open-file-abnormal-exit-restores-superseded
  (let ((f (sff-path "sff-woff-superseded.txt")))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line "OLD" s))
    (handler-case
        (with-open-file (s f :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (write-line "PARTIAL" s)
          (error "abort this file"))
      (error () nil))
    (sff-text f))
  "OLD")

(deftest rename-file-missing-signals-file-error
  (handler-case
      (rename-file (sff-path "sff-no-such-file.txt")
                   (sff-path "sff-no-such-target.txt"))
    (file-error () :file-error)
    (error (e) (list :other (type-of e))))
  :file-error)

(deftest rename-file-directory
  (let* ((from (concatenate 'string (regression-temp-dir) "/sff-dir-from/"))
         (to (concatenate 'string (regression-temp-dir) "/sff-dir-to/")))
    (ignore-errors (delete-file from))
    (ignore-errors (delete-file to))
    (ensure-directories-exist from)
    (rename-file from to)
    (list (and (probe-file from) t) (and (probe-file to) t)))
  (nil t))

(deftest directory-wild-card-has-no-type
  (let ((dir (concatenate 'string (regression-temp-dir) "/sff-dirwild/")))
    (ensure-directories-exist dir)
    (dolist (name '("sffx-a.txt" "sffx-b.bin" "sffx-c"))
      (with-open-file (s (concatenate 'string dir name)
                         :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
        (write-line "x" s)))
    (list (mapcar #'file-namestring
                  (directory (concatenate 'string dir "sffx-*")))
          (mapcar #'file-namestring
                  (directory (concatenate 'string dir "sffx-*.*")))))
  (("sffx-c") ("sffx-a.txt" "sffx-b.bin" "sffx-c")))

(deftest listen-binary-file-does-not-consume
  (let ((f (sff-path "sff-listen.bin")))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (write-byte 1 s)
      (write-byte 2 s)
      (write-byte 3 s))
    (with-open-file (s f :direction :input :element-type '(unsigned-byte 8))
      (list (file-position s) (listen s) (file-position s)
            (read-byte s nil :eof))))
  (0 t 0 1))

(deftest listen-binary-file-at-eof
  (let ((f (sff-path "sff-listen-empty.bin")))
    (with-open-file (s f :direction :output :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      nil)
    (with-open-file (s f :direction :input :element-type '(unsigned-byte 8))
      (list (listen s) (read-byte s nil :eof) (listen s))))
  (nil :eof nil))
