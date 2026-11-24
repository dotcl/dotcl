;;; Values come from SBCL. A string stream has no encoding, so FILE-STRING-LENGTH answers NIL;
;;; the console stream counts UTF-8 bytes.

(deftest file-string-length-string-stream
  (list (file-string-length (make-string-output-stream) "abc")
        (file-string-length (make-string-output-stream) #\a)
        (file-string-length (make-string-input-stream "x") "abc")
        (file-string-length *standard-output* "abc")
        (file-string-length *standard-output* (string (code-char 955)))
        (handler-case (file-string-length (make-string-output-stream) 5) (type-error () :type-error)))
  (nil nil nil 3 2 :type-error))
