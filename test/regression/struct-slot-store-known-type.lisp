;;; A store into a typed structure slot is not checked when the value is
;;; already known to be of the slot's type. The check boxed the value to test
;;; it -- 24 B for every (setf (s-n s) i) of an int64 loop counter into a
;;; FIXNUM slot, and the same for a double into a DOUBLE-FLOAT slot -- while
;;; the store itself took the raw value. A value not known to fit is still
;;; checked.

(defstruct %sskt
  (n 0 :type fixnum)
  (b 0 :type (unsigned-byte 8))
  (d 0d0 :type double-float)
  (tag nil))

(defun %sskt-fill (s n)
  (declare (fixnum n))
  (dotimes (i n)
    (setf (%sskt-n s) i
          (%sskt-b s) (logand i 255)
          (%sskt-d s) (+ (%sskt-d s) 1d0)))
  s)

(defun %sskt-set-b (s v)
  (declare (fixnum v))
  (setf (%sskt-b s) v))

(defun %sskt-set-n (s v)
  (setf (%sskt-n s) v))

(deftest struct-slot-store-known-type.values
  (let ((s (%sskt-fill (make-%sskt) 300)))
    (list (%sskt-n s) (%sskt-b s) (%sskt-d s)))
  (299 43 300d0))

;; A fixnum that may not fit (UNSIGNED-BYTE 8), and a value of unknown type,
;; are still checked.
(deftest struct-slot-store-known-type.still-checked
  (let ((s (make-%sskt)))
    (list (handler-case (progn (%sskt-set-b s 300) :stored)
            (type-error () :type-error))
          (handler-case (progn (%sskt-set-n s "x") :stored)
            (type-error () :type-error))
          (%sskt-set-b s 7)
          (%sskt-b s)))
  (:type-error :type-error 7 7))

(deftest-emitting-only struct-slot-store-known-type.no-allocation
  (let ((s (make-%sskt)))
    (%sskt-fill s 1000)
    (let ((best nil))
      (dotimes (k 3)
        (let ((b0 (nth 4 (dotcl:gc-stats))))
          (%sskt-fill s 10000)
          (let ((d (- (nth 4 (dotcl:gc-stats)) b0)))
            (setf best (if best (min best d) d)))))
      ;; 3 checked stores were 72 B an iteration, 720,000 B here.
      (< best 4000)))
  t)
