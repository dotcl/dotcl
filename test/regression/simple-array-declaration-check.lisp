;;; A binding declared with a SIMPLE-* array type (SIMPLE-ARRAY, SIMPLE-STRING,
;;; SIMPLE-BASE-STRING, SIMPLE-VECTOR, SIMPLE-BIT-VECTOR, bare or compound) must
;;; not hold an adjustable, fill-pointered or displaced array. Above (safety 0)
;;; that is a TYPE-ERROR where the variable is bound, as in SBCL. At (safety 0)
;;; the declaration is trusted: no error, and the value is still read correctly.
;;;
;;; Before this, the declaration-driven paths fell back to the per-element
;;; helper without a word, so a wrong declaration was never noticed.
;;;
;;; Three routes reach the check, and each is asked below in both evaluators
;;; (the emit-free build runs this same file interpreting everything):
;;;   * a hoisted element buffer, whose prologue fetch makes the test itself
;;;     (integer SIMPLE-ARRAY kinds, and SIMPLE-STRING read in a loop)
;;;   * any other binding: an explicit test after the declarations
;;;   * the interpreter's declaration scan

(defun %sdc-run (mode lambda-form &rest args)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (list :value (apply (eval lambda-form) args))
      (type-error () :type-error)
      (error (e) (list :other (type-of e))))))

(defun %sdc-both (lambda-form &rest args)
  (list (apply #'%sdc-run :compile lambda-form args)
        (apply #'%sdc-run :interpret lambda-form args)))

(defun %sdc-strings ()
  "A simple string, a simple character vector that is not a LispString, and
   the three non-simple kinds."
  (let ((base (make-array 6 :element-type 'character :initial-element #\z)))
    (list (make-string 3 :initial-element #\a)
          (make-array 3 :element-type 'character :initial-element #\b)
          (make-array 3 :element-type 'character :adjustable t :initial-element #\c)
          (make-array 3 :element-type 'character :displaced-to base)
          (make-array 3 :element-type 'character :fill-pointer 3 :initial-element #\d))))

(defun %sdc-vectors (element-type initial)
  (let ((base (make-array 6 :element-type element-type :initial-element initial)))
    (list (make-array 3 :element-type element-type :initial-element initial)
          (make-array 3 :element-type element-type :adjustable t :initial-element initial)
          (make-array 3 :element-type element-type :displaced-to base)
          (make-array 3 :element-type element-type :fill-pointer 3
                                                   :initial-element initial))))

(defun %sdc-over (lambda-form objects)
  (mapcar (lambda (o) (%sdc-both lambda-form o)) objects))

;;; --- no hoist: a declared parameter that is only measured

(deftest simple-array-declaration-check.simple-string-param
  (%sdc-over '(lambda (s) (declare (simple-string s)) (length s)) (%sdc-strings))
  (((:value 3) (:value 3))
   ((:value 3) (:value 3))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.simple-vector-param
  (%sdc-over '(lambda (v) (declare (simple-vector v)) (length v)) (%sdc-vectors t 0))
  (((:value 3) (:value 3))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.simple-bit-vector-type-spelling
  (%sdc-over '(lambda (v) (declare (type simple-bit-vector v)) (length v))
             (%sdc-vectors 'bit 1))
  (((:value 3) (:value 3))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.compound-simple-array-t
  (%sdc-over '(lambda (v) (declare (type (simple-array t (*)) v)) (length v))
             (%sdc-vectors t 0))
  (((:value 3) (:value 3))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.rank-2
  (%sdc-over '(lambda (a) (declare (type (simple-array t (2 2)) a)) (array-total-size a))
             (list (make-array '(2 2) :initial-element 0)
                   (make-array '(2 2) :initial-element 0 :adjustable t)))
  (((:value 4) (:value 4))
   (:type-error :type-error)))

;;; --- hoisted integer buffer: the prologue fetch answers for the declaration

(deftest simple-array-declaration-check.hoisted-u8-loop
  (%sdc-over '(lambda (v)
               (declare (type (simple-array (unsigned-byte 8) (*)) v))
               (let ((s 0)) (dotimes (i (length v) s) (incf s (aref v i)))))
             (%sdc-vectors '(unsigned-byte 8) 2))
  (((:value 6) (:value 6))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.hoisted-fixnum-straight-line
  (%sdc-over '(lambda (v) (declare (type (simple-array fixnum (*)) v)) (aref v 0))
             (%sdc-vectors 'fixnum 5))
  (((:value 5) (:value 5))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

;;; --- hoisted char[]: SIMPLE-STRING read inside a loop

(deftest simple-array-declaration-check.hoisted-string-loop
  (%sdc-over '(lambda (s)
               (declare (simple-string s))
               (let ((n 0))
                 (dotimes (i (length s) n) (incf n (char-code (schar s i))))))
             (%sdc-strings))
  (((:value 291) (:value 291))
   ((:value 294) (:value 294))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

;;; --- other binding forms

(deftest simple-array-declaration-check.let
  (%sdc-over '(lambda (x) (let ((s x)) (declare (simple-string s)) (length s)))
             (%sdc-strings))
  (((:value 3) (:value 3))
   ((:value 3) (:value 3))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.let*
  (%sdc-over '(lambda (x) (let* ((a 1) (v x)) (declare (simple-vector v)) (+ a (length v))))
             (%sdc-vectors t 0))
  (((:value 4) (:value 4))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.optional-param
  (%sdc-over '(lambda (&optional (v (vector 1))) (declare (simple-vector v)) (length v))
             (%sdc-vectors t 0))
  (((:value 3) (:value 3))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.closure-param
  (%sdc-over '(lambda (x)
               (funcall (let ((k 1))
                          (lambda (v) (declare (simple-vector v)) (+ k (length v))))
                        x))
             (%sdc-vectors t 0))
  (((:value 4) (:value 4))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

(deftest simple-array-declaration-check.multiple-value-bind
  (%sdc-over '(lambda (x) (multiple-value-bind (v n) (values x 1)
                            (declare (simple-vector v))
                            (+ n (length v))))
             (%sdc-vectors t 0))
  (((:value 4) (:value 4))
   (:type-error :type-error)
   (:type-error :type-error)
   (:type-error :type-error)))

;;; The error names the declared type and carries the value.
(deftest simple-array-declaration-check.condition-slots
  (mapcar (lambda (mode)
            (let* ((v (make-array 3 :adjustable t))
                   (c (let ((dotcl:*evaluator-mode* mode))
                        (handler-case
                            (funcall (eval '(lambda (v) (declare (simple-vector v)) (length v)))
                                     v)
                          (type-error (e) e)))))
              (list (eq (type-error-datum c) v) (type-error-expected-type c))))
          '(:compile :interpret))
  ((t simple-vector) (t simple-vector)))

;;; --- (safety 0): trusted, not checked, and still read correctly

(deftest simple-array-declaration-check.safety-0-unchecked
  (append
   (%sdc-over '(lambda (s) (declare (optimize (safety 0)) (simple-string s))
                (let ((n 0)) (dotimes (i (length s) n) (incf n (char-code (schar s i))))))
              (cddr (%sdc-strings)))
   (%sdc-over '(lambda (v) (declare (optimize (safety 0))
                                    (type (simple-array (unsigned-byte 8) (*)) v))
                (let ((s 0)) (dotimes (i (length v) s) (incf s (aref v i)))))
              (cdr (%sdc-vectors '(unsigned-byte 8) 2)))
   (%sdc-over '(lambda (v) (declare (optimize (safety 0)) (simple-vector v)) (length v))
              (cdr (%sdc-vectors t 0))))
  (((:value 297) (:value 297))
   ((:value 366) (:value 366))
   ((:value 300) (:value 300))
   ((:value 6) (:value 6))
   ((:value 6) (:value 6))
   ((:value 6) (:value 6))
   ((:value 3) (:value 3))
   ((:value 3) (:value 3))
   ((:value 3) (:value 3))))

;;; --- not this check's business: a value that is not an array at all is left
;;; to the operations that use it, as every other declaration is.
(deftest simple-array-declaration-check.non-array-unchanged
  (%sdc-both '(lambda (v) (declare (simple-vector v)) (if (numberp v) :number (length v))) 7)
  ((:value :number) (:value :number)))
