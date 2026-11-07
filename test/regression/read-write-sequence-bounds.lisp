;;; READ-SEQUENCE and WRITE-SEQUENCE check :START and :END against the sequence
;;; (0 <= start <= end <= length) on every kind of stream.

(defmacro %rwb-err (form)
  `(handler-case (progn ,form :no-error) (error () :error)))

(deftest read-write-sequence-bounds.invalid
  (values (%rwb-err (read-sequence (make-array 2) (make-string-input-stream "abc") :start 3))
          (%rwb-err (read-sequence (make-array 2) (make-string-input-stream "abc") :end 3))
          (%rwb-err (read-sequence (make-array 2) (make-string-input-stream "abc") :start 2 :end 1))
          (%rwb-err (read-sequence (make-string 2) (make-string-input-stream "abc") :start 3))
          (%rwb-err (write-sequence "abc" (make-broadcast-stream) :start 4))
          (%rwb-err (write-sequence "abc" (make-broadcast-stream) :start 2 :end 1))
          (%rwb-err (write-sequence #(1 2 3) (make-broadcast-stream) :end 4))
          (%rwb-err (write-sequence '(1 2 3) (make-broadcast-stream) :start 5))
          (%rwb-err (write-sequence "abc" (make-string-output-stream) :end 4)))
  :error :error :error :error :error :error :error :error :error)

(deftest read-write-sequence-bounds.valid
  (let ((buf (make-string 4 :initial-element #\_))
        (out (make-string-output-stream)))
    (list (read-sequence buf (make-string-input-stream "abcdef") :start 1 :end 3)
          buf
          (read-sequence (make-array 2) (make-string-input-stream "abc") :start 2)
          (read-sequence (make-array 0) (make-string-input-stream "abc"))
          (progn (write-sequence "abcdef" out :start 1 :end 3) (get-output-stream-string out))
          (progn (write-sequence "abc" out :start 3) (get-output-stream-string out))
          (progn (write-sequence '(#\x #\y) out :start 0 :end 2) (get-output-stream-string out))))
  (3 "_ab_" 2 0 "bc" "" "xy"))
