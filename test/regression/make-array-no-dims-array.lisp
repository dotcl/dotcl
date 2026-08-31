;;; MAKE-ARRAY of a one-dimensional array costs the vector and nothing else.
;;;
;;; Two things were paid on every call:
;;;
;;;   - an int[1] for the dimensions, which the rank-1 constructors never look
;;;     at (the field is documented as "null = 1D vector");
;;;   - the argument array, because MAKE-ARRAY had no direct entry at all.
;;;
;;; (make-array 3) cost 199.9 bytes and (make-array 0) -- an EMPTY array -- cost
;;; 176.0, against 136.0 and 111.9 for the vector itself.
;;;
;;; What must not change is every other shape: a dimension list, rank 0, each
;;; keyword, and the fall-through when a keyword the fast path does not know
;;; shows up.

;;; Vectors coerced to lists throughout: DEFTEST compares with EQUAL, which does
;;; not look inside a vector.

(deftest make-array-nda.plain
  (list (coerce (make-array 3) 'list) (coerce (make-array 0) 'list)
        (length (make-array 5)))
  ((nil nil nil) nil 5))

(deftest make-array-nda.initial-element
  (list (coerce (make-array 3 :initial-element 7) 'list)
        (coerce (make-array 0 :initial-element 7) 'list)
        (coerce (make-array 2 :initial-element nil) 'list))
  ((7 7 7) nil (nil nil)))

;;; :ELEMENT-TYPE is not on the fast path and must still work, alone and with
;;; :INITIAL-ELEMENT.
(deftest make-array-nda.element-type
  (list (array-element-type (make-array 3 :element-type 'bit))
        (coerce (make-array 3 :element-type 'bit :initial-element 1) 'list)
        (coerce (make-array 2 :element-type '(unsigned-byte 8) :initial-element 5) 'list)
        (length (make-array 3 :element-type 'character)))
  (bit (1 1 1) (5 5) 3))

(deftest make-array-nda.initial-contents
  (list (coerce (make-array 3 :initial-contents (list 1 2 3)) 'list)
        (array-dimensions (make-array (list 2 2)
                                      :initial-contents (list (list 1 2) (list 3 4)))))
  ((1 2 3) (2 2)))

;;; Rank is what the dimensions argument says, whether or not the int[1] is built.
(deftest make-array-nda.rank-and-dimensions
  (list (array-rank (make-array 3)) (array-dimensions (make-array 3))
        (array-rank (make-array (list 2 3))) (array-dimensions (make-array (list 2 3)))
        (array-rank (make-array nil)))
  (1 (3) 2 (2 3) 0))

(deftest make-array-nda.rank-zero
  (aref (make-array nil :initial-element :x))
  :x)

(deftest make-array-nda.fill-pointer
  (let ((v (make-array 5 :fill-pointer 2)))
    (list (length v) (array-dimension v 0)
          (progn (vector-push 9 v) (length v))))
  (2 5 3))

(deftest make-array-nda.adjustable
  (let ((v (make-array 2 :adjustable t :initial-element 0)))
    (list (adjustable-array-p v) (length v)))
  (t 2))

(deftest make-array-nda.displaced
  (let* ((b (make-array 5 :initial-contents (list 0 1 2 3 4)))
         (v (make-array 2 :displaced-to b :displaced-index-offset 2)))
    (list (aref v 0) (aref v 1)))
  (2 3))

(deftest make-array-nda.predicates
  (list (arrayp (make-array 3)) (vectorp (make-array 3))
        (typep (make-array 3) 'simple-vector))
  (t t t))

;;; A bad dimension is still a type error, and an unknown keyword still a
;;; program error unless :ALLOW-OTHER-KEYS says otherwise.
(deftest make-array-nda.errors
  (list (handler-case (progn (make-array -1) :no-error) (error () :error))
        (handler-case (progn (make-array 3 :bogus 1) :no-error) (error () :error))
        (length (make-array 3 :bogus 1 :allow-other-keys t)))
  (:error :error 3))

;;; Each call returns a fresh array.
(deftest make-array-nda.fresh-each-call
  (let ((a (make-array 2 :initial-element 0))
        (b (make-array 2 :initial-element 0)))
    (list (eq a b) (progn (setf (aref a 0) 9) (list (aref a 0) (aref b 0)))))
  (nil (9 0)))

;;; --- the point ---

(defun %mand-bytes () (nth 4 (dotcl:gc-stats)))

(defmacro %mand-per-call (name &body body)
  `(progn
     (defun ,name (n)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000)
     (let ((best nil))
       (dotimes (r 5 best)
         (let ((before (%mand-bytes)))
           (,name 50000)
           (let ((used (- (%mand-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

(defvar *mand-n* 3)
(defvar *mand-zero* 0)

;;; (MAKE-ARRAY n) costs what (VECTOR a b c) of the same length costs -- the
;;; vector object and its storage, nothing on the side. Within one byte a call.
(deftest-compiled-only make-array-nda.costs-only-the-vector
  (list (<= (abs (- (%mand-per-call %mand-plain (make-array *mand-n*))
                    (%mand-per-call %mand-vec (vector 1 2 3))))
            50000)
        (<= (abs (- (%mand-per-call %mand-init (make-array *mand-n* :initial-element *mand-zero*))
                    (%mand-per-call %mand-vec2 (vector 1 2 3))))
            50000))
  (t t))
