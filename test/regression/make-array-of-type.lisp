;;; (MAKE-ARRAY dims :ELEMENT-TYPE 'spec) with a constant spec that names a
;;; standard specialized element type builds the array without the generic
;;; MAKE-ARRAY call.
;;;
;;; The call went through the global function: the symbol was looked up, the
;;; arguments were put in an array, and the element type was parsed again on
;;; every call. ironclad makes a 16-word (unsigned-byte 32) scratch array this
;;; way on every Salsa20 block. The element type name is now resolved when the
;;; form is compiled, and only for specs whose meaning cannot change at run
;;; time ((unsigned-byte N), (signed-byte N), FIXNUM, BIT, the float and the
;;; character types). Every other shape is the generic call, as before.

(setf dotcl:*save-sil* t)

(defun %maot-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %maot-u32 () (make-array 16 :element-type '(unsigned-byte 32)))
(defun %maot-u8 (n) (make-array n :element-type '(unsigned-byte 8)))
(defun %maot-s16 () (make-array 3 :element-type '(signed-byte 16)))
(defun %maot-u64 () (make-array 2 :element-type '(unsigned-byte 64)))
(defun %maot-fix () (make-array 2 :element-type 'fixnum))
(defun %maot-bit () (make-array 5 :element-type 'bit))
(defun %maot-df () (make-array 2 :element-type 'double-float))
(defun %maot-sf () (make-array 2 :element-type 'single-float))
(defun %maot-lf () (make-array 2 :element-type 'long-float))
(defun %maot-ch () (make-array 2 :element-type 'character))
(defun %maot-list-dims () (make-array '(4) :element-type '(unsigned-byte 32)))
(defun %maot-2d () (make-array '(2 3) :element-type '(unsigned-byte 32)))
(defun %maot-dim (d) (make-array d :element-type '(unsigned-byte 32)))
;; Not resolved here: a DEFTYPE name, a keyword more, a non-constant spec.
(deftype %maot-octet () '(unsigned-byte 8))
(defun %maot-deftype () (make-array 2 :element-type '%maot-octet))
(defun %maot-adj () (make-array 2 :element-type '(unsigned-byte 32) :adjustable t))
(defun %maot-var (et) (make-array 2 :element-type et))

(deftest-emitting-only make-array-of-type.sil
  (list (mapcar (lambda (f) (%maot-count "MakeArrayOfType" f))
                (list #'%maot-u32 #'%maot-u8 #'%maot-s16 #'%maot-u64 #'%maot-fix
                      #'%maot-bit #'%maot-df #'%maot-sf #'%maot-lf #'%maot-ch
                      #'%maot-list-dims #'%maot-dim))
        (mapcar (lambda (f) (%maot-count "MakeArrayOfType" f))
                (list #'%maot-deftype #'%maot-adj #'%maot-var)))
  ((1 1 1 1 1 1 1 1 1 1 1 1) (0 0 0)))

(defun %maot-describe (a)
  (list (array-element-type a) (array-dimensions a) (adjustable-array-p a)
        (array-has-fill-pointer-p a) (typep a 'simple-array)
        (coerce (make-array (array-total-size a) :displaced-to a
                                                 :element-type (array-element-type a))
                'list)))

(defun %maot-generic (dims et)
  (apply #'make-array dims (list :element-type et)))

;; Each array is what the generic MAKE-ARRAY makes for the same arguments.
(deftest make-array-of-type.same-as-generic
  (loop for (f dims et) in `((%maot-u32 16 (unsigned-byte 32))
                             (%maot-s16 3 (signed-byte 16))
                             (%maot-u64 2 (unsigned-byte 64))
                             (%maot-fix 2 fixnum)
                             (%maot-bit 5 bit)
                             (%maot-df 2 double-float)
                             (%maot-sf 2 single-float)
                             (%maot-lf 2 long-float)
                             (%maot-ch 2 character)
                             (%maot-list-dims (4) (unsigned-byte 32))
                             (%maot-2d (2 3) (unsigned-byte 32)))
        unless (equal (%maot-describe (funcall f))
                      (%maot-describe (%maot-generic dims et)))
          collect f)
  nil)

(deftest make-array-of-type.values
  (let ((a (%maot-u32)) (b (%maot-u8 10)))
    (setf (aref a 15) #xffffffff (aref b 9) 255)
    (list (aref a 15) (aref a 0) (length b) (aref b 9)
          (array-element-type (%maot-deftype))
          (adjustable-array-p (%maot-adj))
          (array-element-type (%maot-var '(unsigned-byte 16)))
          (length (%maot-dim 0))
          (handler-case (progn (setf (aref a 0) (expt 2 32)) :stored)
            (type-error () :type-error))))
  (#xffffffff 0 10 255 (unsigned-byte 8) t (unsigned-byte 16) 0 :type-error))

;; A bad dimension still signals what the generic call signals.
(deftest make-array-of-type.bad-dimension
  (list (handler-case (%maot-dim -1) (type-error () :type-error))
        (handler-case (%maot-dim :x) (type-error () :type-error)))
  (:type-error :type-error))

(setf dotcl:*save-sil* nil)
