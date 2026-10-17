;;; (MOD X 2^K), K <= 64, of modular arithmetic is computed in the 64-bit lane,
;;; also through INLINE helpers whose parameters are undeclared (cl-pcg's
;;; (% (*+ state multiplier increment))). Operands that are not integers take
;;; the generic path.

(setf dotcl:*save-sil* t)

(defconstant +u64ml-modulus+ (expt 2 64))
(defconstant +u64ml-mult+ 6364136223846793005)

(declaim (inline %u64ml-% %u64ml-*+ %u64ml-checked+))
(defun %u64ml-% (n) (mod n +u64ml-modulus+))
(defun %u64ml-*+ (a b increment) (+ (* a b) increment))
(defun %u64ml-checked+ (a b)
  (declare (type (unsigned-byte 64) a b))
  (mod (+ a b) +u64ml-modulus+))

(defstruct %u64ml-gen
  (state 0 :type (unsigned-byte 64))
  (increment 0 :type (unsigned-byte 64)))

(defun %u64ml-advance (g)
  (declare (type %u64ml-gen g))
  (setf (%u64ml-gen-state g)
        (%u64ml-% (%u64ml-*+ (%u64ml-gen-state g) +u64ml-mult+ (%u64ml-gen-increment g)))))

(defun %u64ml-step (s m inc) (%u64ml-% (%u64ml-*+ s m inc)))
(defun %u64ml-direct (x) (mod x (expt 2 64)))
(defun %u64ml-byte (a b) (mod (* a b) 256))
(defun %u64ml-wide (a b) (mod (* a b) (expt 2 65)))
(defun %u64ml-checked (a b) (%u64ml-checked+ a b))

(defun %u64ml-sil-has (fn text)
  (and (search text (%flat-text (dotcl:function-sil fn))) t))

(deftest-compiled-only u64-mod-lane-sil
  (list (%u64ml-sil-has #'%u64ml-advance "Runtime.StructRefU64")
        (%u64ml-sil-has #'%u64ml-advance "Runtime.BoxU64")
        (%u64ml-sil-has #'%u64ml-step "Runtime.Low64")
        ;; A lone operand: Runtime.Mod returns a fixnum as it is.
        (%u64ml-sil-has #'%u64ml-direct "Runtime.Low64"))
  (t t t nil))

(deftest u64-mod-lane-struct
  (let ((g (make-%u64ml-gen :state (1- (expt 2 64)) :increment (1- (expt 2 64)))))
    (list (%u64ml-advance g) (%u64ml-advance g) (%u64ml-gen-state g)))
  (12082607849862758610 4561710125552423657 4561710125552423657))

(deftest u64-mod-lane-values
  (list (%u64ml-step (1- (expt 2 64)) (1- (expt 2 64)) (1- (expt 2 64)))
        (%u64ml-step (1- (expt 2 64)) 1 1)
        (%u64ml-step 0 0 -1)
        (%u64ml-step 5 7 (- (expt 2 70)))
        (%u64ml-step (- (expt 2 64)) 3 2)
        (%u64ml-step 1.5 2 0)
        (%u64ml-step 1/2 1 0)
        (%u64ml-step 1.5d0 2 (expt 2 64))
        (%u64ml-direct -1)
        (%u64ml-direct (+ (expt 2 70) 5))
        (%u64ml-direct (expt 2 64))
        (%u64ml-direct 2.5d0)
        (%u64ml-byte 255 255)
        (%u64ml-byte -3 5)
        (%u64ml-byte 2.5 3)
        (%u64ml-wide (expt 2 64) 3)
        (handler-case (%u64ml-step :x 1 0) (type-error () :type-error))
        (handler-case (%u64ml-direct "x") (type-error () :type-error)))
  (0 0 18446744073709551615 35 2 3.0 1/2 0.0d0
   18446744073709551615 5 0 2.5d0
   1 241 7.5 18446744073709551616
   :type-error :type-error))

;; A parameter declared (unsigned-byte 64) is checked (safety 1).
(deftest-emitting-only u64-mod-lane-declared
  (list (%u64ml-checked (1- (expt 2 64)) 1)
        (%u64ml-checked (1- (expt 2 64)) (1- (expt 2 64)))
        (handler-case (%u64ml-checked 1 -1) (type-error () :type-error))
        (handler-case (%u64ml-checked (expt 2 64) 1) (type-error () :type-error))
        (handler-case (%u64ml-checked 1.0 1) (type-error () :type-error)))
  (0 18446744073709551614 :type-error :type-error :type-error))
