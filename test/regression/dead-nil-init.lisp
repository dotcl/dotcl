;;; A LET variable whose NIL init the body overwrites before it can be read
;;; (LOOP's (loop :for n = (f) ...) binds N to NIL, then assigns it first thing
;;; in the loop) is typed by its assignments alone, so an (UNSIGNED-BYTE 32)
;;; counter gets a raw Int64 slot instead of a box per value. A GO out of the
;;; assigned value, or a read of the variable in it, keeps the NIL.

(setf dotcl:*save-sil* t)

(defun %dni-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defstruct %dni-gen (state 0 :type (unsigned-byte 32)))

(declaim (inline %dni-next))
(defun %dni-next (g)
  (the (unsigned-byte 32)
       (setf (%dni-gen-state g) (ldb (byte 32 0) (+ (* (%dni-gen-state g) 1103515245) 12345)))))

(defun %dni-pick (g bound)
  (declare (type (integer 1 1000) bound))
  (loop :with threshold = (mod (expt 2 32) bound)
        :for n = (%dni-next g)
        :when (>= n threshold)
          :do (return (values (mod n bound)))))

(defun %dni-finish (k)
  (let ((i 0))
    (loop :for x = (progn (when (>= i k) (loop-finish)) (the (integer 0 100) (incf i)))
          :do (setf i (+ x 0))
          :finally (return x))))

(defun %dni-go (c)
  (let ((n nil))
    (tagbody
     top
       (setq n (progn (when (> c 0) (go skip)) (the (integer 0 9) 5)))
       (setq n (logand (+ n 1) 15))
     skip)
    n))

(defun %dni-self (c)
  (let ((n nil))
    (setq n (if n 1 (the (integer 0 9) c)))
    (+ n 1)))

(deftest dead-nil-init.values
  (list (let ((g (make-%dni-gen :state 7)))
          (loop repeat 5 collect (%dni-pick g 1000)))
        (%dni-finish 0) (%dni-finish 3)
        (%dni-go 1) (%dni-go 0)
        (%dni-self 4))
  ((764 333 938 571 640) nil 3 nil 6 5))

(deftest-compiled-only dead-nil-init.no-box
  (%dni-count "Fixnum.Make" #'%dni-pick)
  1)
