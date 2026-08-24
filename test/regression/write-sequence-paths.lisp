;;; WRITE-SEQUENCE has a two-argument direct entry (no argument array per call)
;;; alongside the variadic one that parses :start/:end. Both reach the same core,
;;; and the core resolves composite streams by recursing on the resolved stream
;;; instead of rewriting its argument array -- these pin that the two entries and
;;; every composite path still agree.

(deftest write-sequence.two-arg-string
  (with-output-to-string (s) (write-sequence "hi there!" s))
  "hi there!")

(deftest write-sequence.start-end
  (list (with-output-to-string (s) (write-sequence "hi there!" s :start 3))
        (with-output-to-string (s) (write-sequence "hi there!" s :end 2))
        (with-output-to-string (s) (write-sequence "hi there!" s :start 3 :end 8)))
  ("there!" "hi" "there"))

(deftest write-sequence.return-value-is-the-sequence
  (let* ((seq "abc") (r nil))
    (with-output-to-string (s) (setf r (write-sequence seq s)))
    (eq r seq))
  t)

(deftest write-sequence.list-and-vector
  (list (with-output-to-string (s) (write-sequence (list #\a #\b #\c) s))
        (with-output-to-string (s) (write-sequence (vector #\x #\y) s))
        (with-output-to-string (s) (write-sequence (list #\a #\b #\c #\d) s :start 1 :end 3)))
  ("abc" "xy" "bc"))

(deftest write-sequence.through-broadcast
  (let* ((a (make-string-output-stream))
         (b (make-string-output-stream))
         (bc (make-broadcast-stream a b)))
    (write-sequence "one" bc)
    (write-sequence "two!" bc :start 1 :end 3)
    (list (get-output-stream-string a) (get-output-stream-string b)))
  ("onewo" "onewo"))

(deftest write-sequence.through-two-way
  (let* ((out (make-string-output-stream))
         (tw (make-two-way-stream (make-string-input-stream "") out)))
    (write-sequence "abc" tw)
    (write-sequence "defg" tw :start 1)
    (get-output-stream-string out))
  "abcefg")

(deftest write-sequence.through-synonym
  (let ((out (make-string-output-stream)))
    (progv '(*ws-syn-target*) (list out)
      (let ((syn (make-synonym-stream '*ws-syn-target*)))
        (write-sequence "abc" syn)
        (write-sequence "xyz" syn :end 1)))
    (get-output-stream-string out))
  "abcx")

(deftest write-sequence.empty-and-nil
  (list (with-output-to-string (s) (write-sequence "" s))
        (with-output-to-string (s) (write-sequence nil s))
        (with-output-to-string (s) (write-sequence "ab" s :start 2)))
  ("" "" ""))

(deftest write-sequence.rejects-non-sequence
  (handler-case (with-output-to-string (s) (write-sequence 42 s))
    (type-error () :type-error)
    (error () :other))
  :type-error)
