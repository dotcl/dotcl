;;; The reader of a DEFSTRUCT with (:TYPE (VECTOR ...)) is compiled as the AREF
;;; its DEFUN computes, so the typed paths see the element type of the vector the
;;; caller declared. It used to be a full call returning an unknown object, so a
;;; LET variable bound to it and declared (UNSIGNED-BYTE 32) stayed a LispObject
;;; local (ironclad's RIPEMD-160 and SHA registers).

(setf dotcl:*save-sil* t)

(defun %tvs-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defstruct (tvs-r32 (:type (vector (unsigned-byte 32))) (:constructor make-tvs-r32 ()))
  (a 1) (b 2) (c 3))

(deftype tvs-r32 () '(simple-array (unsigned-byte 32) (3)))

(defstruct (tvs-named (:type vector) :named (:initial-offset 2)
                      (:constructor make-tvs-named (x y)))
  x y)

(defstruct (tvs-list (:type list) (:constructor make-tvs-list (r s)))
  r s)

(defun %tvs-sum (regs)
  (declare (type tvs-r32 regs))
  (let ((a (tvs-r32-a regs)) (c (tvs-r32-c regs)))
    (declare (type (unsigned-byte 32) a c))
    (+ a c)))

(defun %tvs-notinline (regs)
  (declare (notinline tvs-r32-b))
  (tvs-r32-b regs))

(deftest-compiled-only tvs-reader-is-aref
  ;; No call of the readers, and the declared variables are raw Int64 slots.
  (list (%tvs-sil-count #'%tvs-sum "TVS-R32-A")
        (%tvs-sil-count #'%tvs-sum "TVS-R32-C")
        (%tvs-sil-count #'%tvs-sum " Int64)")
        (%tvs-sil-count #'%tvs-notinline "TVS-R32-B"))
  (0 0 2 1))

(deftest tvs-reader-values
  (let ((r (make-tvs-r32)))
    (setf (tvs-r32-b r) 7)
    (list (%tvs-sum r) (%tvs-notinline r)
          (tvs-r32-c r)
          ;; Any vector: the reader does not check the type.
          (tvs-r32-b (vector 10 20 30))
          (tvs-r32-a (make-array 3 :adjustable t :initial-contents '(4 5 6)))
          (funcall #'tvs-r32-c r)
          (mapcar #'tvs-r32-a (list r))))
  (4 7 3 20 4 3 (1)))

(deftest tvs-named-offset
  (let ((n (make-tvs-named 'p 'q)))
    (list (length n) (tvs-named-x n) (tvs-named-y n)
          (setf (tvs-named-y n) 'z) (aref n 4)))
  (5 p q z z))

(deftest tvs-list-type-reader
  (let ((l (make-tvs-list 1 2)))
    (list (tvs-list-r l) (tvs-list-s l)))
  (1 2))

(deftest tvs-reader-errors-on-short-vector
  (handler-case (progn (tvs-r32-c (vector 1)) :no-error)
    (error () :error))
  :error)

(deftest tvs-local-function-shadows-reader
  (flet ((tvs-r32-a (x) (list :local (length x))))
    (tvs-r32-a (make-tvs-r32)))
  (:local 3))

;; A DEFUN of the reader's name replaces it: calls compiled after it are calls.
(defstruct (tvs-redef (:type vector) (:constructor make-tvs-redef ()))
  (u 1))
(defun tvs-redef-u (x) (list :redefined x))
(defun %tvs-call-redef (x) (tvs-redef-u x))

(deftest tvs-redefined-reader-is-called
  (%tvs-call-redef 5)
  (:redefined 5))
