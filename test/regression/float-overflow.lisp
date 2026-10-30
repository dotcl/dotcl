;;; In safe code -- compiled at (safety 3) -- a float operation whose operands
;;; are finite and not NaN signals when its value is not: FLOATING-POINT-OVERFLOW
;;; for a result too large for its format, DIVISION-BY-ZERO for a quotient by
;;; zero, FLOATING-POINT-INVALID-OPERATION for a NaN, with the operation and
;;; operands. An infinite or NaN operand propagates. Below (safety 3), and in
;;; the interpreter, the value is the IEEE infinity or NaN.

(defun %fpo (thunk)
  (handler-case (funcall thunk)
    (floating-point-overflow () :overflow)))

(defun %fpo-inf-p (x)
  (and (floatp x) (> (abs x) most-positive-double-float)))

(defun %fpo-generic-3 ()
  (declare (optimize (safety 3)))
  (let ((mps most-positive-single-float) (mpd most-positive-double-float))
    (mapcar #'%fpo
            (list (lambda () (* mps 2.0))
                  (lambda () (* mpd 2d0))
                  (lambda () (+ mpd mpd))
                  (lambda () (- mps (- mps)))
                  (lambda () (/ 8 2.225073858507201d-308))
                  (lambda () (* 256 1.0e38))
                  (lambda () (* -98765432109876543210 1.0e20))
                  (lambda () (* -1.0e10 mps (- mps)))
                  (lambda () (* (- mpd) #C(-1 2)))
                  (lambda () (* (expt 10 400) 1d0))
                  (lambda () (/ mpd 0.5d0))))))

(defun %fpo-functions-3 ()
  (declare (optimize (safety 3)))
  (let ((big 1d300))
    (mapcar #'%fpo
            (list (lambda () (scale-float 1.0 200))
                  (lambda () (scale-float 1d308 5))
                  (lambda () (sinh 1.0e10))
                  (lambda () (sinh 1d10))
                  (lambda () (cosh -1d10))
                  (lambda () (exp 100))
                  (lambda () (exp 1000d0))
                  (lambda () (float big 0.0))
                  (lambda () (float (expt 10 400) 1d0))
                  (lambda () (coerce big 'single-float))
                  (lambda () (coerce (expt 10 400) 'double-float))))))

(deftest-compiled-only float-overflow.safety-3-generic
  (%fpo-generic-3)
  (:overflow :overflow :overflow :overflow :overflow :overflow
   :overflow :overflow :overflow :overflow :overflow))

(deftest-compiled-only float-overflow.safety-3-functions
  (%fpo-functions-3)
  (:overflow :overflow :overflow :overflow :overflow :overflow
   :overflow :overflow :overflow :overflow :overflow))

(defun %fpo-operands-3 (x)
  (declare (optimize (safety 3)))
  (handler-case (* x 2d0)
    (floating-point-overflow (c)
      (list (arithmetic-error-operation c) (arithmetic-error-operands c)))))

(deftest-compiled-only float-overflow.operation-and-operands
  (%fpo-operands-3 most-positive-double-float)
  (* (1.7976931348623157d308 2d0)))

;; What does not signal at (safety 3): an infinite or NaN operand, and a
;; result in range.
(defun %fpo-not-signalled-3 (nan)
  (declare (optimize (safety 3)))
  (let ((inf dotcl:double-float-positive-infinity)
        (sinf dotcl:single-float-positive-infinity)
        (one 1d0))
    (list (%fpo-inf-p (* inf 2d0))
          (%fpo-inf-p (+ sinf 1.0))
          (%fpo-inf-p (exp inf))
          (%fpo-inf-p (coerce inf 'single-float))
          (let ((r (+ nan one))) (/= r r))
          (let ((r (sin nan))) (/= r r))
          (* most-positive-double-float one)
          (< (expt 10 400) one))))

(deftest-compiled-only float-overflow.safety-3-not-signalled
  (%fpo-not-signalled-3 (- dotcl:double-float-positive-infinity
                           dotcl:double-float-positive-infinity))
  (t t t t t t 1.7976931348623157d308 nil))

;;; Division by zero and NaN results at (safety 3).

(defun %fpo-kind (thunk)
  (handler-case (progn (funcall thunk) :none)
    (division-by-zero (c) (list :div0 (arithmetic-error-operation c)))
    (floating-point-invalid-operation (c) (list :invalid (arithmetic-error-operation c)))
    (floating-point-overflow () :overflow)))

(defun %fpo-faults-3 (one zero inf)
  (declare (optimize (safety 3)))
  (mapcar #'%fpo-kind
          (list (lambda () (/ one zero))
                (lambda () (/ 1.0 (float zero 1.0)))
                (lambda () (/ one 0))
                (lambda () (/ zero zero))
                (lambda () (- inf inf))
                (lambda () (* inf zero))
                (lambda () (sin inf))
                (lambda () (expt zero -1))
                (lambda () (log zero))
                (lambda () (expt 10d0 (+ 400d0 zero))))))

(defun %fpo-d-div (a b) (declare (double-float a b) (optimize (safety 3))) (/ a b))
(defun %fpo-s-sub (a b) (declare (single-float a b) (optimize (safety 3))) (- a b))
(defun %fpo-d-cos (a) (declare (double-float a) (optimize (safety 3))) (the double-float (+ 1d0 (cos a))))

(deftest-compiled-only float-overflow.safety-3-faults
  (list (%fpo-faults-3 1d0 0d0 dotcl:double-float-positive-infinity)
        (%fpo-kind (lambda () (%fpo-d-div 1d0 0d0)))
        (%fpo-kind (lambda () (%fpo-d-div 0d0 0d0)))
        (%fpo-kind (lambda () (%fpo-s-sub dotcl:single-float-positive-infinity
                                          dotcl:single-float-positive-infinity)))
        (%fpo-kind (lambda () (%fpo-d-cos dotcl:double-float-positive-infinity))))
  (((:div0 /) (:div0 /) (:div0 /) (:invalid /) (:invalid -) (:invalid *)
    (:invalid sin) (:div0 expt) (:div0 log) :overflow)
   (:div0 /) (:invalid /) (:invalid -) (:invalid cos)))

;;; Declared operands at (safety 3): the native float lanes.

(defun %fpo-d* (a b) (declare (double-float a b) (optimize (safety 3))) (* a b))
(defun %fpo-d/ (a b) (declare (double-float a b) (optimize (safety 3))) (/ a b))
(defun %fpo-s- (a b) (declare (single-float a b) (optimize (safety 3))) (- a b))
(defun %fpo-di* (a i) (declare (double-float a) (fixnum i) (optimize (safety 3))) (* a i))
(defun %fpo-narrow (a)
  (declare (double-float a) (optimize (safety 3)))
  (the single-float (+ 1.0 (coerce a 'single-float))))
(defun %fpo-sinh (a)
  (declare (double-float a) (optimize (safety 3)))
  (the double-float (+ 1d0 (sinh a))))

(deftest-compiled-only float-overflow.safety-3-declared
  (list (%fpo (lambda () (%fpo-d* 1d300 1d300)))
        (%fpo (lambda () (%fpo-d/ 1d300 1d-300)))
        (%fpo (lambda () (%fpo-s- most-positive-single-float (- most-positive-single-float))))
        (%fpo (lambda () (%fpo-di* 1d308 100)))
        (%fpo (lambda () (%fpo-narrow 1d300)))
        (%fpo (lambda () (%fpo-sinh 1d10)))
        (%fpo-inf-p (%fpo-d* dotcl:double-float-positive-infinity 2d0))
        (%fpo-d* 1.5d0 2d0))
  (:overflow :overflow :overflow :overflow :overflow :overflow t 3d0))

;;; A nest of declared operations is checked at its value, and computed again
;;; with every operation checked when that value is not finite. An overflow in
;;; a divisor, whose infinity would give a finite quotient, is still signalled.

(defun %fpo-nest (zr zi cr)
  (declare (double-float zr zi cr) (optimize (safety 3)))
  (+ (- (* zr zr) (* zi zi)) cr))
(defun %fpo-nest-s (x acc)
  (declare (single-float x acc) (optimize (safety 3)))
  (the single-float (+ (* acc 0.999f0) (/ (* x x) (+ 1.0f0 (* x x))))))
(defun %fpo-nest-div (a b c)
  (declare (double-float a b c) (optimize (safety 3)))
  (+ c (/ 1d0 (* a b))))
(defun %fpo-nest-arr (v i)
  (declare (type (simple-array double-float (*)) v) (fixnum i) (optimize (safety 3)))
  (* (aref v i) (aref v i) (aref v i)))

(deftest-compiled-only float-overflow.safety-3-nest
  (list (%fpo-nest 1d0 2d0 3d0)
        (%fpo (lambda () (%fpo-nest 1d200 1d0 0d0)))
        (%fpo (lambda () (%fpo-nest 1d0 1d0 most-positive-double-float)))
        (%fpo-inf-p (%fpo-nest dotcl:double-float-positive-infinity 0d0 0d0))
        (let ((r (%fpo-nest 0d0 0d0 (- dotcl:double-float-positive-infinity
                                       dotcl:double-float-positive-infinity))))
          (/= r r))
        (%fpo-nest-s 0.5 1.0)
        (%fpo (lambda () (%fpo-nest-s 1e20 1.0)))
        (%fpo (lambda () (%fpo-nest-div 1d200 1d200 1d0)))
        (%fpo-nest-div 2d0 4d0 1d0)
        (let ((v (make-array 2 :element-type 'double-float :initial-contents '(1d150 2d0))))
          (list (%fpo (lambda () (%fpo-nest-arr v 0))) (%fpo-nest-arr v 1))))
  (0d0 :overflow 1.7976931348623157d308 t t 1.199 :overflow :overflow 1.125d0 (:overflow 8d0)))

;;; Below (safety 3) -- the default (safety 1) and (safety 0) -- and in the
;;; interpreter, the value is the IEEE infinity or NaN, declared or not.

(defun %fpo-default (x y)
  (list (* x 2d0) (+ y y) (scale-float x 5) (sinh 1d10) (exp 1000d0)
        (coerce x 'single-float) (float x 1.0)))
(defun %fpo-default-d (a b) (declare (double-float a b)) (+ (* a b) (* a b)))
(defun %fpo-0-d (a b) (declare (double-float a b) (optimize (safety 0))) (* a b))
(defun %fpo-0-s (a b) (declare (single-float a b) (optimize (safety 0))) (+ a b))

(defun %fpo-default-ieee (one zero inf)
  (list (/ one zero) (let ((r (/ zero zero))) (/= r r)) (let ((r (- inf inf))) (/= r r))
        (let ((r (sin inf))) (/= r r))
        (log zero) (expt zero -1) (expt 10d0 400d0)))
(defun %fpo-0-div (a b) (declare (double-float a b) (optimize (safety 0))) (/ a b))

(deftest float-overflow.below-safety-3
  (list (every #'%fpo-inf-p (%fpo-default 1d308 most-positive-single-float))
        (%fpo-inf-p (%fpo-default-d 1d300 1d300))
        (%fpo-inf-p (%fpo-0-d 1d300 1d300))
        (%fpo-inf-p (%fpo-0-s most-positive-single-float most-positive-single-float))
        (%fpo-inf-p (funcall #'* 1d300 1d300))
        (%fpo-default-ieee 1d0 0d0 dotcl:double-float-positive-infinity)
        (%fpo-inf-p (%fpo-0-div 1d0 0d0))
        (let ((r (%fpo-0-div 0d0 0d0))) (/= r r)))
  (t t t t t (#.dotcl:double-float-positive-infinity t t t
             #.dotcl:double-float-negative-infinity
             #.dotcl:double-float-positive-infinity
             #.dotcl:double-float-positive-infinity)
   t t))
