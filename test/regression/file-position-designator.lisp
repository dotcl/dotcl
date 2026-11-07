;;; FILE-POSITION's second argument is :START, :END or a non-negative integer
;;; (CLHS); a negative or non-integer position is a type-error, not a quiet NIL.

(defun %fpd (thunk)
  (handler-case (funcall thunk) (type-error () :type-error)))

(deftest file-position-designator.string-stream
  (list (%fpd (lambda () (file-position (make-string-input-stream "abc") -1)))
        (%fpd (lambda () (file-position (make-string-input-stream "abc") 1.5)))
        (%fpd (lambda () (file-position (make-string-input-stream "abc") "1")))
        (%fpd (lambda () (file-position (make-string-input-stream "abc") :middle))))
  (:type-error :type-error :type-error :type-error))

(deftest file-position-designator.file-stream
  (let ((path (regression-temp-file "file-position-designator.txt")))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "hello" o))
    (with-open-file (s path)
      (list (%fpd (lambda () (file-position s -1)))
            (and (file-position s 2) t)
            (read-char s)
            (and (file-position s :start) t)
            (read-char s))))
  (:type-error t #\l t #\h))

(deftest file-position-designator.valid-string-stream
  (let ((s (make-string-input-stream "abcdef")))
    (list (and (file-position s 3) t) (read-char s) (file-position s)))
  (t #\d 4))
