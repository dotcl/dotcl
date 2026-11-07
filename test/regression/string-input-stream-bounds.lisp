;;; MAKE-STRING-INPUT-STREAM and WITH-INPUT-FROM-STRING take START as an index
;;; and END as an index or NIL; a non-integer bound was accepted and :START
;;; 1.5 was ignored.

(defun %sisb (thunk)
  (handler-case (funcall thunk) (type-error () :type-error)))

(deftest string-input-stream-bounds.non-integer
  (list (%sisb (lambda () (make-string-input-stream "abc" 1.5)))
        (%sisb (lambda () (make-string-input-stream "abc" 0 2.5)))
        (%sisb (lambda () (with-input-from-string (s "abc" :start 1.5) (read-char s))))
        (%sisb (lambda () (with-input-from-string (s "abc" :end "2") (read-char s)))))
  (:type-error :type-error :type-error :type-error))

(deftest string-input-stream-bounds.out-of-range
  (list (%sisb (lambda () (make-string-input-stream "abc" -1)))
        (%sisb (lambda () (make-string-input-stream "abc" 4)))
        (%sisb (lambda () (make-string-input-stream "abc" 0 9)))
        (%sisb (lambda () (make-string-input-stream "abc" 2 1))))
  (:type-error :type-error :type-error :type-error))

(deftest string-input-stream-bounds.valid
  (list (read-line (make-string-input-stream "abcdef" 1 4))
        (read-line (make-string-input-stream "abc" 1 nil))
        (read-line (make-string-input-stream "abc" 3) nil "")
        (with-input-from-string (s "abcdef" :start 2 :end 4) (read-line s)))
  ("bcd" "bc" "" "cd"))
