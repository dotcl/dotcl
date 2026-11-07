;;; After GET-OUTPUT-STREAM-STRING a string output stream is empty, so FRESH-LINE and the
;;; format directive ~& agree that it is at the start of a line.

(deftest string-output-stream-fresh-line-after-get.format-amp
  (let ((s (make-string-output-stream)))
    (write-string "ab" s)
    (get-output-stream-string s)
    (format s "~&x")
    (get-output-stream-string s))
  "x")

(deftest string-output-stream-fresh-line-after-get.fresh-line-and-amp-agree
  (let ((s (make-string-output-stream)))
    (write-string "ab" s)
    (get-output-stream-string s)
    (list (fresh-line s) (progn (format s "~&") (get-output-stream-string s))))
  (nil ""))

(deftest string-output-stream-fresh-line-after-get.unfinished-line-still-gets-newline
  (let ((s (make-string-output-stream)))
    (write-string "ab" s)
    (format s "~&c")
    (get-output-stream-string s))
  #.(format nil "ab~%c"))
