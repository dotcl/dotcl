;;; A :NAMED (:TYPE (VECTOR element-type)) structure stores the structure's name, a
;;; symbol, in the vector, so the element type has to be able to hold a symbol: SYMBOL
;;; (or a supertype) is accepted; CHARACTER, numbers and bits are an error (SBCL).
;;; The check used to be the other way round.

(defun %dnet (form)
  (handler-case (progn (eval form) :ok) (error () :error)))

(deftest defstruct-named-element-type.symbol-accepted
  (list (%dnet '(defstruct (dnet-a (:type (vector symbol)) :named) x))
        (%dnet '(defstruct (dnet-b (:type (vector t)) :named) x))
        (%dnet '(defstruct (dnet-c (:type (vector (or symbol integer))) :named) x))
        (%dnet '(defstruct (dnet-d (:type vector) :named) x)))
  (:ok :ok :ok :ok))

(deftest defstruct-named-element-type.cannot-hold-a-symbol
  (list (%dnet '(defstruct (dnet-e (:type (vector character)) :named) x))
        (%dnet '(defstruct (dnet-f (:type (vector bit)) :named) x))
        (%dnet '(defstruct (dnet-g (:type (vector (unsigned-byte 8))) :named) x))
        (%dnet '(defstruct (dnet-h (:type (vector double-float)) :named) x)))
  (:error :error :error :error))

(deftest defstruct-named-element-type.symbol-vector-works
  (progn (eval '(defstruct (dnet-i (:type (vector symbol)) :named) x))
         (let ((o (make-dnet-i :x 'a)))
           (list (dnet-i-x o) (dnet-i-p o) (aref o 0))))
  (a t dnet-i))

(deftest defstruct-named-element-type.unnamed-character-vector-unchanged
  (progn (eval '(defstruct (dnet-j (:type (vector character))) x))
         (dnet-j-x (make-dnet-j :x #\a)))
  #\a)
