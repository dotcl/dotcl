;;; COERCE to a simple array type copies a vector that is not simple (fill
;;; pointer, adjustable), even when its element type already matches, and a
;;; length in the result type is checked.

(defstruct coerce-sa-elem)

(defstruct (coerce-sa-bar (:constructor make-coerce-sa-bar (data end)))
  (data nil :type (simple-array coerce-sa-elem (*)))
  (end 0 :type fixnum))

(defun coerce-sa-fp (contents &optional (element-type t))
  (make-array (length contents) :element-type element-type :fill-pointer 2
                                :adjustable t :initial-contents contents))

(deftest coerce-simple-array.struct-elements
  (let ((v (make-array 0 :element-type 'coerce-sa-elem :fill-pointer 0 :adjustable t)))
    (vector-push-extend (make-coerce-sa-elem) v)
    (vector-push-extend (make-coerce-sa-elem) v)
    (let ((sv (coerce v '(simple-array coerce-sa-elem (*)))))
      (values (typep sv '(simple-array coerce-sa-elem (*)))
              (length sv)
              (coerce-sa-bar-p (make-coerce-sa-bar sv 0)))))
  t 2 t)

(deftest coerce-simple-array.element-t
  (let ((sv (coerce (coerce-sa-fp '(1 2 3)) '(simple-array t (*)))))
    (values (typep sv '(simple-array t (*))) (coerce sv 'list)))
  t (1 2))

(deftest coerce-simple-array.wildcard
  (typep (coerce (coerce-sa-fp '(1 2 3)) '(simple-array * (*))) 'simple-array)
  t)

(deftest coerce-simple-array.rank-and-dims
  (values (typep (coerce (coerce-sa-fp '(1 2 3)) '(simple-array t 1)) 'simple-vector)
          (typep (coerce (coerce-sa-fp '(1 2 3)) '(simple-array t (2))) 'simple-vector))
  t t)

(deftest coerce-simple-array.simple-vector-length
  (values (typep (coerce (coerce-sa-fp '(1 2 3)) '(simple-vector 2)) 'simple-vector)
          (signals-error (coerce (coerce-sa-fp '(1 2 3)) '(simple-vector 3)) error))
  t t)

(deftest coerce-simple-array.specialized
  (values (typep (coerce (coerce-sa-fp '(1 2 3) 'fixnum) '(simple-array fixnum (*)))
                 '(simple-array fixnum (*)))
          (typep (coerce (coerce-sa-fp '(1 2 3) 'fixnum) '(simple-array t (*)))
                 '(simple-array t (*)))
          (typep (coerce (coerce-sa-fp '(1 2 3) 'fixnum) '(vector t)) '(vector t)))
  t t t)

(deftest coerce-simple-array.character
  (let ((s (coerce (coerce-sa-fp "abc" 'character) '(simple-array character (*)))))
    (values (typep s 'simple-string) s))
  t "ab")

(deftest coerce-simple-array.simple-string-length
  (values (typep (coerce (coerce-sa-fp "abc" 'character) '(simple-string 2)) 'simple-string)
          (signals-error (coerce (coerce-sa-fp "abc" 'character) '(simple-string 3)) error))
  t t)

(deftest coerce-simple-array.bit
  (values (typep (coerce (coerce-sa-fp '(1 0 1) 'bit) '(simple-array bit (*))) 'simple-bit-vector)
          (typep (coerce (coerce-sa-fp '(1 0 1) 'bit) '(simple-bit-vector)) 'simple-bit-vector)
          (typep (coerce (coerce-sa-fp '(1 0 1) 'bit) '(simple-bit-vector 2)) 'simple-bit-vector)
          (signals-error (coerce (coerce-sa-fp '(1 0 1) 'bit) '(simple-bit-vector 3)) error)
          (coerce '(1 0 1) '(simple-bit-vector 3)))
  t t t t #*101)

(deftest coerce-simple-array.already-simple-is-returned
  (let ((v (vector 1 2)))
    (eq v (coerce v '(simple-array t (*)))))
  t)

(deftest coerce-simple-array.not-a-sequence-type
  (values (signals-error (coerce #(1 2) '(array t (2 2))) type-error)
          (signals-error (coerce 5 '(simple-array t (*))) type-error))
  t t)

(deftest coerce-simple-array.nil-element-is-type-error
  (mapcar (lambda (type)
            (handler-case (progn (coerce (list 1 nil) type) :no-error)
              (type-error () :type-error)))
          '((simple-array (unsigned-byte 8) (*))
            (vector (unsigned-byte 8))
            (simple-array fixnum (*))
            (simple-array double-float (*))
            (simple-array bit (*))))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest coerce-simple-array.base-char
  (flet ((try (obj type)
           (handler-case (let ((r (coerce obj type)))
                           (list r (typep r 'simple-base-string)))
             (type-error () :type-error))))
    (list (try '(#\a #\b) '(simple-array base-char (*)))
          (try (vector #\a #\b) '(vector base-char))
          (try (coerce-sa-fp '(#\a #\b #\c) 'character) 'simple-base-string)
          (try '() 'simple-base-string)
          (try '(#\a 1) '(simple-array base-char (*)))
          (try (list (code-char 955)) 'simple-base-string)
          (try '(#\a #\b) '(simple-array base-char (3)))))
  (("ab" t) ("ab" t) ("ab" t) ("" t) :type-error :type-error :type-error))
