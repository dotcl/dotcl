;;; A LET variable declared with a bounded integer type that fits int64 --
;;; (UNSIGNED-BYTE 32) say -- lives in a raw Int64 slot like one declared
;;; FIXNUM, also when it is assigned. ironclad's WITH-WORDS binds the two
;;; Blowfish halves that way and assigns them sixteen times per block; each
;;; assignment boxed the new value. The tight declared range still decides
;;; what can be computed raw: a product of two such variables that may not fit
;;; int64 takes the promoting path.

(setf dotcl:*save-sil* t)

(defun %bil-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %bil-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %bil-mix (p a b)
  (declare (type (simple-array (unsigned-byte 32) (*)) p)
           (type (unsigned-byte 32) a b))
  (let ((left a) (right b))
    (declare (type (unsigned-byte 32) left right))
    (dotimes (i 4)
      (setf left (logxor left (aref p i))
            right (logxor right left)))
    (list left right)))

(defun %bil-product (a b)
  (let ((x (logand a #xffffffff)) (y (logand b #xffffffff)))
    (declare (type (unsigned-byte 32) x y))
    (setf x (logior x 1))
    (* x y)))

(defparameter *bil-p*
  (make-array 4 :element-type '(unsigned-byte 32) :initial-contents '(1 2 4 #x80000000)))

(deftest bounded-int-let-long-slot.values
  (list (%bil-mix *bil-p* 0 0)
        (%bil-mix *bil-p* #xffffffff 7)
        (%bil-product #xffffffff #xffffffff))
  ((#x80000007 #x80000002) (#x7ffffff8 #x80000005) #.(* #xffffffff #xffffffff)))

;; The assignments store raw: LEFT and RIGHT are boxed only to build the
;; returned list (once in each of the two copies of the body), not per
;; assignment.
(deftest-emitting-only bounded-int-let-long-slot.raw
  (%bil-count "Fixnum.Make" (%bil-sil #'%bil-mix))
  4)
