;;; From (safety 1) up a type declaration on a function's parameter is checked
;;; on entry: a value of another type is a TYPE-ERROR. (safety 0) trusts it.

(defun %adc-te (thunk)
  (handler-case (list :value (funcall thunk))
    (type-error (e) (list :type-error (type-error-datum e)))))

(defstruct adc-pt a)
(defun %adc-fix (x) (declare (fixnum x)) (list x))
(defun %adc-u8 (x) (declare (type (unsigned-byte 8) x)) (logand x 255))
(defun %adc-dbl (x) (declare (double-float x)) (list x))
(defun %adc-u64 (x) (declare (type (unsigned-byte 64) x)) (list x))
(defun %adc-str (x) (declare (simple-string x)) (list x))
(defun %adc-struct (x) (declare (type adc-pt x)) (list x))
(defun %adc-or (x) (declare (type (or null fixnum) x)) (list x))
(defun %adc-vec (x) (declare (type (simple-array double-float (*)) x)) (length x))
(defun %adc-opt (&optional (x 0)) (declare (fixnum x)) (list x))
(defun %adc-key (&key (x 0)) (declare (fixnum x)) (list x))
(defun %adc-rest (a &rest r) (declare (fixnum a)) (list a r))
(declaim (ftype (function (fixnum) t) %adc-ftype))
(defun %adc-ftype (x) (list x))
(defun %adc-safety0 (x) (declare (optimize (safety 0)) (fixnum x)) (list x))
(defmethod %adc-gf ((x t)) (declare (fixnum x)) (list x))

(deftest arg-decl-check.wrong-type
  (mapcar (lambda (thunk) (car (%adc-te thunk)))
          (list (lambda () (%adc-fix 1.5))
                (lambda () (%adc-u8 300))
                (lambda () (%adc-dbl 1))
                (lambda () (%adc-u64 -1))
                (lambda () (%adc-str 3))
                (lambda () (%adc-struct 3))
                (lambda () (%adc-or 1.5))
                (lambda () (%adc-vec (vector 1d0)))
                (lambda () (%adc-opt 1.5))
                (lambda () (%adc-key :x 1.5))
                (lambda () (%adc-rest 1.5))
                (lambda () (%adc-ftype 1.5))
                (lambda () (%adc-gf 1.5))
                (lambda () (funcall (lambda (x) (declare (fixnum x)) x) (read-from-string "1.5")))
                (lambda () (flet ((f (x) (declare (fixnum x)) (list x)))
                             (f (read-from-string "1.5"))))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :type-error :type-error :type-error :type-error
   :type-error))

(deftest arg-decl-check.right-type
  (list (%adc-fix 1) (%adc-u8 200) (%adc-dbl 1d0) (%adc-u64 (1- (expt 2 64))) (%adc-str "a")
        (adc-pt-a (car (%adc-struct (make-adc-pt :a 1)))) (%adc-or nil)
        (%adc-vec (make-array 2 :element-type 'double-float)) (%adc-opt) (%adc-key :x 2)
        (%adc-rest 1 2) (%adc-ftype 3) (%adc-gf 4))
  ((1) 200 (1d0) (18446744073709551615) ("a") 1 (nil) 2 (0) (2) (1 (2)) (3) (4)))

(deftest arg-decl-check.datum
  (%adc-te (lambda () (%adc-fix 1.5)))
  (:type-error 1.5))

(deftest arg-decl-check.safety-0-trusts
  (%adc-safety0 1.5)
  (1.5))

(defun %adc-undeclared (x) (list x))
(defun %adc-t (x) (declare (type t x)) (list x))
(defun %adc-ignored (x) (declare (fixnum x) (ignore x)) :ok)

(deftest arg-decl-check.no-declaration-no-check
  (list (%adc-undeclared 1.5) (%adc-t 1.5) (%adc-ignored 1.5))
  ((1.5) (1.5) :ok))

(defstruct adc-base a)
(defstruct (adc-mid (:include adc-base)) b)
(defstruct (adc-leaf (:include adc-mid)) c)
(defstruct adc-other a)
(defun %adc-base (x) (declare (type adc-base x)) (adc-base-a x))
(defun %adc-mid (x) (declare (type adc-mid x)) (adc-base-a x))

(deftest arg-decl-check.struct-subtypes
  (list (%adc-base (make-adc-base :a 1)) (%adc-base (make-adc-leaf :a 3))
        (%adc-mid (make-adc-leaf :a 4))
        (car (%adc-te (lambda () (%adc-mid (make-adc-base :a 1)))))
        (car (%adc-te (lambda () (%adc-base (make-adc-other :a 1))))))
  (1 3 4 :type-error :type-error))

(deftype adc-list-of-fixnums () '(satisfies %adc-fixnum-list-p))
(defun %adc-fixnum-list-p (l) (and (listp l) (every #'integerp l)))
(defun %adc-sat (x) (declare (type adc-list-of-fixnums x)) (length x))

(deftest arg-decl-check.satisfies
  (list (%adc-sat '(1 2)) (car (%adc-te (lambda () (%adc-sat '(a))))))
  (2 :type-error))

(defun %adc-both (form &rest args)
  (flet ((run (mode)
           (let ((dotcl:*evaluator-mode* mode))
             (handler-case (list :value (apply (eval form) args))
               (type-error (e) (list :type-error (type-error-datum e)))))))
    (list (run :compile) (run :interpret))))

(deftest arg-decl-check.both-evaluators
  (list (%adc-both '(lambda (x) (declare (fixnum x)) x) 1.5)
        (%adc-both '(lambda (&optional (x 1)) (declare (fixnum x)) x) 1.5)
        (%adc-both '(lambda (&key (x 1)) (declare (fixnum x)) x) :x 1.5)
        (%adc-both '(lambda (a &rest r) (declare (list r) (fixnum a)) (list a r)) 1 2)
        (%adc-both '(lambda (x) "doc" (declare (type (or null adc-pt) x)) x) 3)
        (%adc-both '(lambda (x) (flet ((f (y) (declare (fixnum y)) y)) (f x))) 1.5)
        (%adc-both '(lambda (x) (declare (optimize (safety 0)) (fixnum x)) x) 1.5)
        (%adc-both '(lambda (x y) (declare (type t x)) (list x y)) 1.5 :a)
        (%adc-both '(lambda (x &aux (y x)) (declare (fixnum y)) y) 1.5))
  (((:type-error 1.5) (:type-error 1.5))
   ((:type-error 1.5) (:type-error 1.5))
   ((:type-error 1.5) (:type-error 1.5))
   ((:value (1 (2))) (:value (1 (2))))
   ((:type-error 3) (:type-error 3))
   ((:type-error 1.5) (:type-error 1.5))
   ((:value 1.5) (:value 1.5))
   ((:value (1.5 :a)) (:value (1.5 :a)))
   ((:value 1.5) (:value 1.5))))

(declaim (ftype (function (fixnum) t) %adc-ftype-interp))
(deftest arg-decl-check.interpreted-defun
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(defun %adc-ftype-interp (x) (list x)))
    (eval '(defun %adc-decl-interp (x) (declare (fixnum x)) (list x)))
    (eval '(defmethod %adc-gf-interp ((x t)) (declare (fixnum x)) (list x)))
    (list (%adc-te (lambda () (%adc-ftype-interp 1.5)))
          (%adc-te (lambda () (%adc-decl-interp 1.5)))
          (%adc-te (lambda () (%adc-gf-interp 1.5)))
          (%adc-decl-interp 2)))
  ((:type-error 1.5) (:type-error 1.5) (:type-error 1.5) (2)))

;; A DEFTYPE that expands to a FUNCTION type (serapeum's ->) is checked as
;; FUNCTION, as SBCL does, also inside AND / OR. TYPEP would refuse it.
(deftype adc-fn (args vals) `(function ,args ,vals))
(deftype adc-fn0 () '(function (t) t))

(deftest arg-decl-check.deftype-function
  (list (%adc-both '(lambda (a) (declare (type (adc-fn ((unsigned-byte 32)) (values (unsigned-byte 8) &optional)) a))
                     (funcall a 1))
                   #'1+)
        (%adc-both '(lambda (a) (declare (type adc-fn0 a)) (funcall a 1)) #'1+)
        (%adc-both '(lambda (a) (declare (type adc-fn0 a)) a) 5)
        (%adc-both '(lambda (a) (declare (type (or null (adc-fn (t) t)) a)) (and a (funcall a 1))) nil)
        (%adc-both '(lambda (a) (declare (type (or null (adc-fn (t) t)) a)) a) 5)
        (%adc-both '(lambda (a) (declare (type (and (adc-fn (t) t) (not null)) a)) (funcall a 1)) #'1+)
        (%adc-both '(lambda (a) (flet ((g (f) (declare (type (adc-fn (t) t) f)) (funcall f 1))) (g a))) #'1+))
  (((:value 2) (:value 2)) ((:value 2) (:value 2)) ((:type-error 5) (:type-error 5))
   ((:value nil) (:value nil)) ((:type-error 5) (:type-error 5)) ((:value 2) (:value 2))
   ((:value 2) (:value 2))))
