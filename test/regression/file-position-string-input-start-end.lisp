;;; FILE-POSITION on a string input stream accepts :START and :END as position designators.

(deftest file-position-string-input-start-end.designators
  (let ((s (make-string-input-stream "hello")))
    (list (file-position s :end) (file-position s) (read-char s nil :eof)
          (file-position s :start) (file-position s) (read-char s)))
  (5 5 :eof 0 0 #\h))

(deftest file-position-string-input-start-end.with-offsets
  (let ((s (make-string-input-stream "hello" 1 4)))
    (list (file-position s) (file-position s :end) (read-char s nil :eof)
          (file-position s :start) (read-char s)))
  (0 3 :eof 0 #\e))

(deftest file-position-string-input-start-end.integer-still-works
  (let ((s (make-string-input-stream "hello")))
    (list (file-position s 3) (read-char s) (file-position s)))
  (3 #\l 4))
