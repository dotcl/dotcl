;;; A generic function call that runs an :around method builds no delegate or closure of its
;;; own: the combination CALL-NEXT-METHOD reaches from the last :around is made once per
;;; dispatch-cache entry.

(defvar *ada-log* nil)

(defgeneric ada-1 (x))
(defmethod ada-1 :around ((x integer)) (call-next-method))
(defmethod ada-1 ((x integer)) x)

(defgeneric ada-2 (x y))
(defmethod ada-2 :around ((x integer) y) (list :around (call-next-method)))
(defmethod ada-2 :before ((x integer) y) (push :before *ada-log*))
(defmethod ada-2 ((x integer) y) (list :primary x y))
(defmethod ada-2 :after ((x integer) y) (push :after *ada-log*))
(defmethod ada-2 ((x (eql 7)) y) (list :seven (call-next-method)))

(defgeneric ada-3 (x))
(defmethod ada-3 :around ((x integer)) (call-next-method (1+ x)))
(defmethod ada-3 :around ((x number)) (list :number (call-next-method)))
(defmethod ada-3 ((x integer)) (list :int x))

(defun %ada-loop (n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i 10000) (setq r (ada-1 n)))
    r))

(deftest around-dispatch-allocation.values
  (progn
    (setq *ada-log* nil)
    (list (ada-1 5)
          (ada-2 3 4)
          (reverse *ada-log*)
          (ada-2 7 8)
          (ada-3 1)
          (ada-2 3 4)
          (ada-2 7 8)))
  (5 (:around (:primary 3 4)) (:before :after)
   (:around (:seven (:primary 7 8)))
   (:number (:int 2))
   (:around (:primary 3 4))
   (:around (:seven (:primary 7 8)))))

(deftest-compiled-only around-dispatch-allocation.bytes
  (progn
    (%ada-loop 5)
    (< (min-allocated-bytes (lambda () (%ada-loop 5))) (* 10000 64)))
  t)
