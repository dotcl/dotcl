(defstruct (dsc-ro (:conc-name dsc-ro-)) (x 1 :read-only t))
(defstruct (dsc-ro-child (:include dsc-ro) (:conc-name dsc-ro-child-)) (y 2))

(deftest defstruct-conformance.read-only-lexical
  (list (handler-case (let ((s (make-dsc-ro))) (setf (dsc-ro-x s) 5))
          (error () :error))
        (handler-case
            (eval '(let ((s (make-dsc-ro))) (setf (dsc-ro-x s) 5)))
          (error () :error))
        (handler-case
            (let ((s (make-dsc-ro-child))) (setf (dsc-ro-child-x s) 5))
          (error () :error))
        (dsc-ro-x (make-dsc-ro)))
  (:error :error :error 1))

(defstruct (dsc-typed-parent (:type list)) (x 1))
(defstruct (dsc-typed-child-ok (:type list) (:include dsc-typed-parent)) (e 2))
(defstruct (dsc-untyped-parent) (x 1))

(deftest defstruct-conformance.include-type
  (list (handler-case
            (eval '(defstruct (dsc-typed-child (:include dsc-typed-parent))
                     (e 2)))
          (error () :error))
        (handler-case
            (eval '(defstruct (dsc-untyped-typed-child
                                 (:type list)
                                 (:include dsc-untyped-parent))
                     (e 2)))
          (error () :error))
        (coerce (make-dsc-typed-child-ok) 'list))
  (:error :error (1 2)))

(defstruct (dsc-sv-fix (:type (vector fixnum)) (:initial-offset 2))
  (x 1) (y 2))
(defstruct (dsc-sv-u8 (:type (vector (unsigned-byte 8))) (:initial-offset 1))
  (x 1) (y 2))
(defstruct (dsc-sv-double (:type (vector double-float)) (:initial-offset 1))
  (x 1d0) (y 2d0))
(defstruct (dsc-sv-single (:type (vector single-float)) (:initial-offset 1)
                          (:constructor dsc-make-sv-single (x y)))
  (x 1s0) (y 2s0))

(deftest defstruct-conformance.specialized-vector-initial-offset
  (let ((fix (make-dsc-sv-fix))
        (u8 (make-dsc-sv-u8))
        (double (make-dsc-sv-double))
        (single (dsc-make-sv-single 3s0 4s0)))
    (list (list (dsc-sv-fix-x fix) (dsc-sv-fix-y fix) (length fix)
                (type-of fix))
          (list (dsc-sv-u8-x u8) (dsc-sv-u8-y u8) (length u8)
                (type-of u8))
          (list (dsc-sv-double-x double) (dsc-sv-double-y double)
                (length double) (type-of double))
          (list (dsc-sv-single-x single) (dsc-sv-single-y single)
                (length single) (type-of single))))
  ((1 2 4 (simple-array fixnum (4)))
   (1 2 3 (simple-array (unsigned-byte 8) (3)))
   (1.0d0 2.0d0 3 (simple-array double-float (3)))
   (3.0 4.0 3 (simple-array single-float (3)))))

(defstruct (dsc-sharp) (x 1) (y (list :y)))
(defstruct (dsc-sharp-nil-ctor (:constructor nil)) (x 1))
(defstruct (dsc-sharp-boa (:constructor dsc-make-sharp-boa (x))) (x 1))

(deftest defstruct-conformance.sharp-s-initforms
  (let ((empty (read-from-string "#S(DSC-SHARP)"))
        (one (read-from-string "#S(DSC-SHARP :X 9)")))
    (list (dsc-sharp-x empty) (dsc-sharp-y empty)
          (dsc-sharp-x one) (dsc-sharp-y one)))
  (1 (:y) 9 (:y)))

(deftest defstruct-conformance.sharp-s-unknown-slot
  (handler-case (progn (read-from-string "#S(DSC-SHARP :UNKNOWN 5)") :no-error)
    (error () :error))
  :error)

(deftest defstruct-conformance.sharp-s-no-standard-constructor
  (list (handler-case (progn (read-from-string "#S(DSC-SHARP-NIL-CTOR :X 5)")
                             :no-error)
          (error () :error))
        (handler-case (progn (read-from-string "#S(DSC-SHARP-BOA :X 5)")
                             :no-error)
          (error () :error)))
  (:error :error))

(defstruct (dsc-inc-rw) (x 1))
(defstruct (dsc-inc-ro) (x 1 :read-only t))
(defstruct (dsc-inc-ro-child (:include dsc-inc-ro) (:conc-name dsc-inc-ro-child-))
  (e 2))
(defstruct (dsc-inc-rw-to-ro (:include dsc-inc-rw (x 0 :read-only t))
                             (:conc-name dsc-inc-rw-to-ro-)))

(deftest defstruct-conformance.include-read-only
  (list (handler-case
            (eval '(defstruct (dsc-inc-ro-to-rw
                                (:include dsc-inc-ro (x 0 :read-only nil)))
                     (e 2)))
          (error () :error))
        (let ((s (make-dsc-inc-ro-child)))
          (handler-case (progn (setf (dsc-inc-ro-child-x s) 5) :no-error)
            (error () :error)))
        (dsc-inc-rw-to-ro-x (make-dsc-inc-rw-to-ro)))
  (:error :error 0))

(deftest defstruct-conformance.include-read-only-write
  (let ((s (make-dsc-inc-rw-to-ro)))
    (handler-case (progn (setf (dsc-inc-rw-to-ro-x s) 5) :no-error)
      (error () :error)))
  :error)

(deftest defstruct-conformance.both-print-options
  (handler-case
      (eval '(defstruct (dsc-both-print
                          (:print-object
                           (lambda (o stream)
                             (declare (ignore o))
                             (princ "o" stream)))
                          (:print-function
                           (lambda (o stream depth)
                             (declare (ignore o depth))
                             (princ "f" stream))))
                 (x 1)))
    (error () :error))
  :error)

(defun dsc-print-depth (object stream depth)
  (declare (ignore object))
  (format stream "depth=~a" depth))
(defstruct (dsc-print-depth-s (:print-function dsc-print-depth)) (x 1))

(deftest defstruct-conformance.print-function-depth
  (list (write-to-string (make-dsc-print-depth-s))
        (write-to-string (list (make-dsc-print-depth-s))))
  ("depth=0" "(depth=1)"))

(defstruct (dsc-copier-a) (x 1) (y 2))
(defstruct (dsc-copier-b) (x 3) (y 4))
(defstruct (dsc-copier-child (:include dsc-copier-a)) (z 5))

(deftest defstruct-conformance.copier-argument-type
  (list (handler-case (copy-dsc-copier-a (make-dsc-copier-b))
          (type-error () :type-error)
          (error () :error))
        (let ((c (copy-dsc-copier-a (make-dsc-copier-a :x 9))))
          (list (dsc-copier-a-x c) (dsc-copier-a-y c)))
        (typep (copy-dsc-copier-a (make-dsc-copier-child)) 'dsc-copier-child))
  (:type-error (9 2) t))
