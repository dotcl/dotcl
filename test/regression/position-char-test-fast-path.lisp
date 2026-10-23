;;; POSITION and FIND of a character in a string or a character vector, with
;;; no :TEST or with EQL, CHAR= or CHAR-EQUAL and no :KEY, compare the
;;; characters directly instead of calling the test for each element.

(defparameter *pct-base* (coerce "0123456789abcdefABC" 'simple-base-string))
(defparameter *pct-str* (copy-seq "0123456789abcdefABC"))
(defparameter *pct-vec* (make-array 6 :element-type 'character :initial-contents "xAbaBy"))

(defun %pct-row (seq)
  (list (position #\f seq) (position #\F seq) (position #\F seq :test #'char-equal)
        (position #\b seq :test #'char-equal :from-end t)
        (position #\a seq :test #'char=) (position #\a seq :test #'eql)
        (position #\a seq :test #'char-equal :start 11) (position #\a seq :test #'char-equal :end 5)
        (find #\B seq :test #'char-equal) (find #\B seq) (find #\z seq :test #'char-equal)
        (position #\A seq :test #'char-equal :key #'char-downcase)
        (position #\a seq :test-not #'char-equal)))

(deftest position-char-test-fast-path.strings
  (list (%pct-row *pct-base*) (%pct-row *pct-str*))
  ((15 nil 15 17 10 10 16 nil #\b #\B nil 10 0)
   (15 nil 15 17 10 10 16 nil #\b #\B nil 10 0)))

(deftest position-char-test-fast-path.character-vector
  (list (position #\b *pct-vec* :test #'char-equal)
        (position #\B *pct-vec* :test #'char-equal :from-end t)
        (position #\B *pct-vec*)
        (find #\a *pct-vec* :test #'char-equal :start 2)
        (position #\a (subseq *pct-vec* 1) :test #'char-equal))
  (2 4 4 #\a 0))

(deftest position-char-test-fast-path.non-character-item
  (list (position 1 *pct-base*)
        (handler-case (position 1 *pct-base* :test #'char-equal) (type-error () :type-error)))
  (nil :type-error))
