;;; The constructor of a (:TYPE (VECTOR ET)) structure makes the vector and stores
;;; each value, instead of building a list for :INITIAL-CONTENTS. The values are
;;; computed in order before the vector is made, and a value outside ET is a
;;; TYPE-ERROR after all of them are computed.

(setf dotcl:*save-sil* t)

(defstruct (%dtvc-regs (:type (vector (unsigned-byte 64))) (:constructor %dtvc-initial ())
                       (:copier nil))
  (a #x6a09e667f3bcc908) (b #xbb67ae8584caa73b))

(defvar *%dtvc-log* nil)

(defstruct (%dtvc-r8 (:type (vector (unsigned-byte 8))) (:copier nil)
                     (:constructor %dtvc-mk (&key (x (progn (push :x *%dtvc-log*) 1))
                                                  (y (progn (push :y *%dtvc-log*) 2)))))
  x y)

(defstruct (%dtvc-kw (:type (vector (unsigned-byte 16))) (:copier nil)) (p 7) (q 8))

(deftest defstruct-typed-vector-ctor.values
  (list (coerce (%dtvc-initial) 'list) (type-of (%dtvc-initial))
        (progn (setf *%dtvc-log* nil) (list (coerce (%dtvc-mk) 'list) (reverse *%dtvc-log*)))
        (coerce (%dtvc-mk :x 9 :y 8) 'list)
        (coerce (make-%dtvc-kw :q 300) 'list) (type-of (make-%dtvc-kw)))
  ((7640891576956012808 13503953896175478587) (simple-array (unsigned-byte 64) (2))
   ((1 2) (:x :y)) (9 8) (7 300) (simple-array (unsigned-byte 16) (2))))

(deftest defstruct-typed-vector-ctor.type-error-after-all-values
  (progn (setf *%dtvc-log* nil)
         (handler-case (%dtvc-mk :x 300 :y (progn (push :yy *%dtvc-log*) 3))
           (type-error () (list :type-error *%dtvc-log*))))
  (:type-error (:yy)))

(deftest-compiled-only defstruct-typed-vector-ctor.stores
  (and (search "STELEM-I8" (%flat-text (dotcl:function-sil #'%dtvc-initial))) t)
  t)
