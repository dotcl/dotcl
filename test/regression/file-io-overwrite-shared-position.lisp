;;; An :IO file stream opened with :IF-EXISTS :OVERWRITE has one file position for reading
;;; and writing (CLHS 21.2). The reader reads ahead, and a write used to go to the end of
;;; what it had read, not to the position of the last character read: after reading "a"
;;; of "abcdef", writing "XY" made "abcdefXY".

(defun %fiosp-file (name content)
  (let ((path (merge-pathnames name (regression-temp-dir))))
    (with-open-file (s path :direction :output :if-exists :supersede) (write-string content s))
    path))

(defun %fiosp-contents (path)
  (with-open-file (s path) (values (read-line s nil ""))))

(deftest file-io-overwrite-shared-position.write-after-read
  (let ((path (%fiosp-file "fiosp1.txt" "abcdef")))
    (with-open-file (s path :direction :io :if-exists :overwrite)
      (list (read-char s) (progn (write-string "XY" s) (file-position s))
            (progn (finish-output s) (%fiosp-contents path))
            (read-char s))))
  (#\a 3 "aXYdef" #\d))

(deftest file-io-overwrite-shared-position.write-then-write
  (let ((path (%fiosp-file "fiosp2.txt" "abcdef")))
    (with-open-file (s path :direction :io :if-exists :overwrite)
      (write-string "12" s) (write-string "34" s))
    (%fiosp-contents path))
  "1234ef")

(deftest file-io-overwrite-shared-position.read-write-read
  (let ((path (%fiosp-file "fiosp3.txt" "abcdef")))
    (with-open-file (s path :direction :io :if-exists :overwrite)
      (let ((a (read-char s)) (b (read-char s)))
        (write-char #\Z s)
        (list a b (read-char s) (progn (finish-output s) (%fiosp-contents path))))))
  (#\a #\b #\d "abZdef"))

(deftest file-io-overwrite-shared-position.append-and-supersede-unchanged
  (let ((path (%fiosp-file "fiosp4.txt" "abc")))
    (with-open-file (s path :direction :io :if-exists :append) (write-string "XY" s))
    (list (%fiosp-contents path)
          (progn (with-open-file (s path :direction :io :if-exists :supersede) (write-string "new" s))
                 (%fiosp-contents path))))
  ("abcXY" "new"))
