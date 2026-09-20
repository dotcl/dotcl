;;; (SCHAR s i) and (CHAR s i) in value position.
;;;
;;; The character comparison operators already read a string element as a code
;;; without building the character. This is the other half: the element read on
;;; its own, where the character IS the value -- bound to a variable, passed to
;;; a function, folded by CHAR-CODE. The typed entry has to answer exactly what
;;; the generic one answered, including for the arguments that are not a simple
;;; string or not a valid index.
;;;
;;; Both the declared and the undeclared shape are here. A declaration changes
;;; how the index reaches the element read; it must not change the value.

(defparameter *stv-text* "ab cd")

;;; --- value position, fully declared ---

(defun stv-decl-code (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (char-code (schar s i)))

(defun stv-decl-bind (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (schar s i)))
    (declare (character c))
    (list c (characterp c) (char-code c))))

(defun stv-decl-setq (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c #\?))
    (declare (character c))
    (setq c (schar s i))
    c))

(defun stv-decl-arg (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (list (schar s i) (char-upcase (schar s i))))

;;; --- the same shapes with no declarations at all ---

(defun stv-plain-code (s i) (char-code (schar s i)))
(defun stv-plain-bind (s i) (let ((c (schar s i))) (list c (characterp c))))
(defun stv-plain-setq (s i) (let ((c #\?)) (setq c (schar s i)) c))
(defun stv-plain-arg (s i) (list (schar s i) (char-upcase (schar s i))))

(deftest schar-typed-declared-value
  (list (stv-decl-code *stv-text* 0)
        (stv-decl-bind *stv-text* 1)
        (stv-decl-setq *stv-text* 2)
        (stv-decl-arg *stv-text* 3))
  (97 (#\b t 98) #\Space (#\c #\C)))

(deftest schar-typed-undeclared-value
  (list (stv-plain-code *stv-text* 0)
        (stv-plain-bind *stv-text* 1)
        (stv-plain-setq *stv-text* 2)
        (stv-plain-arg *stv-text* 3))
  (97 (#\b t) #\Space (#\c #\C)))

;;; The value is a character object, not a code dressed up as one: EQL against a
;;; literal and TYPEP both have to hold.

(deftest schar-typed-value-is-a-character
  (let ((c (stv-decl-setq *stv-text* 0)))
    (list (characterp c) (typep c 'character) (eql c #\a) (char= c #\a)))
  (t t t t))

;;; --- CHAR, and strings a simple-string read cannot serve ---
;;;
;;; CHAR takes the same two arguments and is lowered the same way, but its
;;; argument need not be simple. An adjustable string with a fill pointer is the
;;; shape that separates the two: the fast path must decline it rather than read
;;; the backing array past the fill pointer.

(defun stv-char-of (s i) (char s i))

(defun stv-adjustable ()
  (let ((v (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)))
    (vector-push-extend #\x v)
    (vector-push-extend #\y v)
    (vector-push-extend #\z v)
    v))

(deftest schar-typed-char-on-simple-string
  (list (stv-char-of *stv-text* 0) (stv-char-of *stv-text* 4))
  (#\a #\d))

(deftest schar-typed-char-on-adjustable-string
  (let ((v (stv-adjustable)))
    (list (stv-char-of v 0) (stv-char-of v 2) (length v)))
  (#\x #\z 3))

;;; --- the arguments the fast path must decline ---

(deftest schar-typed-index-out-of-range-signals
  (handler-case (progn (stv-char-of *stv-text* 99) :no-error)
    (error () :error))
  :error)

(deftest schar-typed-negative-index-signals
  (handler-case (progn (stv-char-of *stv-text* -1) :no-error)
    (error () :error))
  :error)

(deftest schar-typed-non-string-signals
  (handler-case (progn (stv-char-of 12 0) :no-error)
    (error () :error))
  :error)

;;; A subscript that is not an integer is a TYPE-ERROR. Which text the condition
;;; carries is not pinned here: the typed entry converts the subscript the way
;;; every other element read converts one, so the wording is that of a
;;; subscript, not of CHAR.

(deftest schar-typed-non-integer-index-signals
  (handler-case (progn (stv-char-of *stv-text* #\a) :no-error)
    (type-error () :type-error)
    (error () :other-error))
  :type-error)

;;; --- evaluation order and effectful subexpressions ---
;;;
;;; The string is evaluated before the index, and each exactly once, whether or
;;; not either is simple enough to push straight onto the stack.

(defparameter *stv-log* nil)

(defun stv-note (tag value)
  (push tag *stv-log*)
  value)

(deftest schar-typed-evaluation-order
  (let ((*stv-log* nil))
    (let ((c (schar (stv-note :string *stv-text*) (stv-note :index 1))))
      (list c (reverse *stv-log*))))
  (#\b (:string :index)))

(deftest schar-typed-index-evaluated-once
  (let ((n 0))
    (let ((c (schar *stv-text* (progn (incf n) 4))))
      (list c n)))
  (#\d 1))

;;; A computed index that is not statically a fixnum still reads the element.

(deftest schar-typed-computed-index
  (let ((xs (list 0 2 4)))
    (mapcar (lambda (k) (schar *stv-text* k)) xs))
  (#\a #\Space #\d))

;;; --- SETF of the place is unaffected ---

(deftest schar-typed-setf-still-stores
  (let ((s (copy-seq "abc")))
    (setf (schar s 1) #\Z)
    (list s (schar s 1)))
  ("aZc" #\Z))

;;; --- a scan reading every element agrees with the generic reader ---

(defun stv-scan-typed (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i (length s) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

(defun stv-scan-generic (s)
  (let ((acc 0))
    (map nil (lambda (c) (setq acc (+ acc (char-code c)))) s)
    acc))

(deftest schar-typed-scan-agrees-with-generic
  (let ((s (copy-seq "The quick brown fox, 12345!")))
    (list (= (stv-scan-typed s) (stv-scan-generic s)) (stv-scan-typed s)))
  (t 2175))
