;;; A list headed by a COMMON-LISP symbol is a type specifier only with the
;;; arguments that compound type takes (CLHS 4.2.3). TYPEP of (T), (T *),
;;; (SYMBOL), (FIXNUM), (INTEGER 0 1 2), (STRING 3 4), (MOD) and the like now
;;; signals, as on SBCL; they answered NIL. type-i grows (T), (T *), (T * *) ...
;;; while TYPEP accepts them, so its TYPE-TESTS never returned and its test run
;;; hit the 900 s bound. The atomic types SBCL also takes as a one-element list
;;; ((KEYWORD), (BOOLEAN) ...) mean the atomic type. Expected values are SBCL
;;; 2.6.8's answers (SBCL signals ARG-COUNT-ERROR for a wrong count and
;;; SIMPLE-ERROR for a non-compound head; here both are ERRORs).

(defun %tic-valid-p (spec)
  (handler-case (progn (typep 1 spec) t) (error () nil)))

(deftest typep-invalid-compound.not-compound
  (mapcar #'%tic-valid-p '((t) (t *) (nil) (symbol) (list) (null) (character) (fixnum)
                           (bignum) (standard-object) (hash-table) (sequence) (number)
                           (stream) (ratio) (random-state)))
  (nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(deftest typep-invalid-compound.too-many-arguments
  (mapcar #'%tic-valid-p '((mod 3 4) (not t t) (eql 1 2) (satisfies evenp oddp) (cons t t t)
                           (complex real t) (integer 0 1 2) (string 3 4) (simple-vector 3 4)
                           (array t (2) 3) (unsigned-byte 8 8) (bit-vector 3 3) (vector t 3 3)
                           (keyword *)))
  (nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(deftest typep-invalid-compound.too-few-arguments
  (mapcar #'%tic-valid-p '((mod) (not) (eql) (satisfies)))
  (nil nil nil nil))

(deftest typep-invalid-compound.valid
  (mapcar #'%tic-valid-p '((member) (and) (or) (cons) (cons t) (integer) (integer 0) (integer 0 1)
                           (string) (string 3) (array t (2)) (vector t 3) (mod 5) (unsigned-byte 8)
                           (keyword) (atom) (boolean) (bit) (base-char) (standard-char)
                           (compiled-function)))
  (t t t t t t t t t t t t t t t t t t t t t))

(deftest typep-invalid-compound.atomic-as-list
  (list (typep :a '(keyword)) (typep 'a '(keyword)) (typep 1 '(bit)) (typep 2 '(bit))
        (typep nil '(boolean)) (typep #\a '(standard-char)) (typep '(1) '(atom)))
  (t nil t nil t t nil))
