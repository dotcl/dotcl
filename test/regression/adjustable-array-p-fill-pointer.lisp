;;; ADJUSTABLE-ARRAY-P answers true for every array that is not simple, as in
;;; SBCL: :ADJUSTABLE T, a fill pointer, or displaced. ADJUST-ARRAY changes all
;;; of them in place, so the answer holds.
;;;
;;; Bug: a vector with a fill pointer but no :ADJUSTABLE T answered NIL, and
;;; ADJUST-ARRAY returned a fresh array for it. babel's portable
;;; WITH-SIMPLE-VECTOR takes "not adjustable" to mean "displacement ends in a
;;; simple array", so such a string reached code declared SIMPLE-ARRAY and
;;; failed (closure-template's DECODE-URL tests).

(deftest adjustable-array-p-fill-pointer.kinds
  (mapcar (lambda (a) (and (adjustable-array-p a) t))
          (list (make-array 3)
                (make-array 3 :adjustable t)
                (make-array 3 :fill-pointer 2)
                (make-array 3 :element-type 'character :fill-pointer 0)
                (make-array 3 :element-type '(unsigned-byte 8) :fill-pointer 0)
                (make-array 2 :displaced-to (make-array 3))
                (make-string 3)
                (make-array '(2 2))))
  (nil t t t t t nil nil))

(deftest adjustable-array-p-fill-pointer.adjust-in-place
  (let* ((a (make-array 3 :fill-pointer 2 :initial-contents '(1 2 3)))
         (b (adjust-array a 5 :initial-element 0)))
    (list (eq a b) (fill-pointer b) (array-dimensions b)
          (progn (setf (fill-pointer b) 5) (coerce b 'list))))
  (t 2 (5) (1 2 3 0 0)))

(deftest adjustable-array-p-fill-pointer.string-in-place
  (let* ((a (make-array 5 :fill-pointer 2 :element-type 'character
                          :initial-element #\x))
         (b (adjust-array a 8 :initial-element #\y)))
    (list (eq a b) (copy-seq b)
          (progn (setf (fill-pointer b) 8) (copy-seq b))))
  (t "xx" "xxxxxyyy"))

(deftest adjustable-array-p-fill-pointer.keeps-fill-pointer
  ;; :INITIAL-CONTENTS and :DISPLACED-TO keep the fill pointer too.
  (let* ((a (make-array 5 :fill-pointer 2 :initial-contents '(1 2 3 4 5)))
         (c (make-array 5 :fill-pointer 2 :initial-contents '(1 2 3 4 5)))
         (d (make-array 6 :initial-contents '(a b c d e f))))
    (adjust-array a 4 :initial-contents '(a b c d))
    (adjust-array c 4 :displaced-to d)
    (list (fill-pointer a) (coerce a 'list) (fill-pointer c) (coerce c 'list)))
  (2 (a b) 2 (a b)))

(deftest adjustable-array-p-fill-pointer.bit-growth
  ;; Growing a bit vector in place fills the new bits with the initial
  ;; element even when the old last word held set bits past the old size.
  (let* ((a (make-array 5 :fill-pointer 2 :element-type 'bit :initial-element 1))
         (b (adjust-array a 8 :initial-element 0)))
    (setf (fill-pointer b) 8)
    (list (eq a b) (coerce b 'list)))
  (t (1 1 1 1 1 0 0 0)))

(deftest adjustable-array-p-fill-pointer.shrink-below-fill-pointer
  (let ((a (make-array 3 :fill-pointer 3)))
    (handler-case (progn (adjust-array a 2) :no-error)
      (error () :error)))
  :error)

(deftest adjustable-array-p-fill-pointer.fill-pointer-on-simple
  (let ((a (make-array 3)))
    (handler-case (progn (adjust-array a 4 :fill-pointer 2) :no-error)
      (error () :error)))
  :error)

(deftest adjustable-array-p-fill-pointer.simple-stays-simple
  (let* ((a (make-array 3 :initial-contents '(1 2 3)))
         (b (adjust-array a 4 :initial-element 0)))
    (list (eq a b) (adjustable-array-p b) (coerce a 'list) (coerce b 'list)))
  (nil nil (1 2 3) (1 2 3 0)))
