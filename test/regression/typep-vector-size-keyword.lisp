;;; (TYPEP X '(VECTOR * N)) and (TYPEP X 'KEYWORD) with a constant specifier are compiled to plain
;;; tests (a vector whose total size is N, and KEYWORDP). The answers are the ones the general
;;; TYPEP gives; in particular the size of a vector with a fill pointer is its dimension, not its
;;; length (values from SBCL 2.6.8).
(defun tvsk-v2 (x) (typep x '(vector * 2)))
(defun tvsk-v0 (x) (typep x '(vector * 0)))
(defun tvsk-kw (x) (typep x 'keyword))

(deftest typep-vector-size-keyword.vector
  (let ((fp (make-array 5 :fill-pointer 2 :adjustable t))
        (fp1 (make-array 2 :fill-pointer 1)))
    (mapcar (lambda (x) (tvsk-v2 x))
            (list (vector 1 2) (vector 1) "ab" (list 1 2) nil (make-array '(1 2)) fp fp1
                  (make-array 2 :element-type 'bit) (make-array 2 :element-type '(unsigned-byte 8)))))
  (t nil t nil nil nil nil t t t))

(deftest typep-vector-size-keyword.empty
  (list (tvsk-v0 #()) (tvsk-v0 "") (tvsk-v0 (vector 1)))
  (t t nil))

(deftest typep-vector-size-keyword.keyword
  (list (tvsk-kw :a) (tvsk-kw 'a) (tvsk-kw nil) (tvsk-kw (make-symbol "A")) (tvsk-kw 1)
        (typep :k 'keyword) (typep 'k 'keyword))
  (t nil nil nil nil t nil))

;;; The same shapes as a declared parameter: the entry check is the inline test, and a value that
;;; fails it still signals a TYPE-ERROR (values from SBCL 2.6.8).
(defun tvsk-d2 (x) (declare (type (vector * 2) x)) (length x))
(defun tvsk-dk (x) (declare (type keyword x)) (symbol-name x))
(defun tvsk-dor (x) (declare (type (or keyword (vector * 2)) x)) (if (symbolp x) 1 2))

(deftest typep-vector-size-keyword.declared
  (list (tvsk-d2 (vector 1 2)) (tvsk-d2 "ab")
        (handler-case (tvsk-d2 (vector 1)) (type-error () :err))
        (handler-case (tvsk-d2 '(1 2)) (type-error () :err))
        (tvsk-dk :abc)
        (handler-case (tvsk-dk 'abc) (type-error () :err))
        (handler-case (tvsk-dk nil) (type-error () :err))
        (tvsk-dor :a) (tvsk-dor (vector 1 2))
        (handler-case (tvsk-dor 5) (type-error () :err)))
  (2 2 :err :err "ABC" :err :err 1 2 :err))
