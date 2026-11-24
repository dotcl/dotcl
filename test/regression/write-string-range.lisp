;;; WRITE-STRING / WRITE-LINE with :START and :END write the slice without making a substring; the
;;; text and the errors are the same on every kind of stream.

(deftest wsr.string-output-stream
  (list (with-output-to-string (s) (write-string "hello" s :start 1 :end 3))
        (with-output-to-string (s) (write-string "hello" s :start 2))
        (with-output-to-string (s) (write-string "hello" s :end 2))
        (with-output-to-string (s) (write-string "hello" s :start 5 :end 5))
        (with-output-to-string (s) (write-string "hello" s :start 0 :end 5))
        (with-output-to-string (s) (write-string "hello" s :start 1 :end nil))
        (with-output-to-string (s) (write-line "hello" s :start 1 :end 3)))
  ("el" "llo" "he" "" "hello" "ello" "el
"))

(deftest wsr.other-string-shapes
  (list (with-output-to-string (s) (write-string (coerce "hello" '(simple-array character (*))) s :start 1 :end 4))
        (with-output-to-string (s) (write-string (make-array 5 :element-type 'character :initial-contents "world" :adjustable t :fill-pointer 4) s :start 1 :end 3))
        (with-output-to-string (s) (write-string (string 'abc) s :start 1)))
  ("ell" "or" "BC"))

(deftest wsr.fill-pointer-and-two-way-targets
  (let ((v (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (with-output-to-string (s v)
      (write-string "abcdef" s :start 2 :end 4)
      (write-string "xyz" s :start 1))
    (list (coerce v 'simple-string)
          (let ((out (make-string-output-stream)))
            (let ((b (make-broadcast-stream out out)))
              (write-string "abcdef" b :start 1 :end 3))
            (get-output-stream-string out))
          (let ((out (make-string-output-stream)))
            (let ((*standard-output* out)) (write-string "abcdef" *standard-output* :start 4))
            (get-output-stream-string out))))
  ("cdyz" "bcbc" "ef"))

(deftest wsr.file-stream
  (let ((path (regression-temp-file "wsr-file.txt")))
    (with-open-file (f path :direction :output :if-exists :supersede)
      (write-string "0123456789" f :start 3 :end 7)
      (write-line "abcdef" f :start 4))
    (prog1 (with-open-file (f path) (list (read-line f) (read-line f nil :eof)))
      (ignore-errors (delete-file path))))
  ("3456ef" :eof))

(deftest wsr.errors
  (list (handler-case (write-string "abc" (make-string-output-stream) :start 5) (error (e) (type-of e)))
        (handler-case (write-string "abc" (make-string-output-stream) :start 2 :end 1) (error (e) (type-of e)))
        (handler-case (write-string "abc" (make-string-output-stream) :end 9) (error (e) (type-of e)))
        (handler-case (write-string "abc" (make-string-output-stream) :start -1) (error (e) (type-of e)))
        (handler-case (write-string 5 (make-string-output-stream) :start 0 :end 1) (error (e) (type-of e))))
  (type-error type-error type-error type-error type-error))
