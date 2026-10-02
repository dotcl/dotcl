;;; TYPEP against a symbol known only at run time, and structure slot type
;;; checks, answer from a test built once per type specifier. These pin that
;;; the built tests agree with the specifier's meaning across object kinds,
;;; and that they are rebuilt when a DEFTYPE or a structure is (re)defined.

(defstruct tbt-a x)
(defstruct (tbt-b (:include tbt-a)) y)
(defstruct tbt-other z)
(deftype tbt-small () '(integer 0 (10)))
(deftype tbt-maybe-a () '(or null tbt-a))

(defun tbt-typep (obj type) (typep obj type))

(defparameter *tbt-objects*
  (list nil t 'sym :kw 0 1 -5 10 (expt 2 70) 1.5d0 #\a "str" (vector 1 2)
        (list 1) (make-tbt-a) (make-tbt-b) (make-tbt-other)
        (make-hash-table) #'car))

(defun tbt-row (type)
  (mapcar (lambda (o) (if (tbt-typep o type) 1 0)) *tbt-objects*))

(deftest typep-built-tests.cl-names
  (mapcar #'tbt-row '(null cons list symbol atom boolean keyword fixnum
                      integer character string simple-string vector))
  ((1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)
   (1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)
   (1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1)
   (1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0)))

(deftest typep-built-tests.structures-and-deftypes
  (mapcar #'tbt-row '(tbt-a tbt-b tbt-other tbt-small tbt-maybe-a))
  ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
   (0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0)))

(deftest typep-built-tests.compounds
  (mapcar #'tbt-row '((integer -5 1) (integer (-5) (10)) (member :kw 1 #\a)
                      (eql 1) (not symbol) (and integer (not (eql 0)))
                      (or tbt-other null)))
  ((0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)))

(deftest typep-built-tests.deftype-redefined
  (progn
    (eval '(deftype tbt-changing () 'integer))
    (list (tbt-typep 3 'tbt-changing)
          (progn (eval '(deftype tbt-changing () 'string)) (tbt-typep 3 'tbt-changing))
          (tbt-typep "x" 'tbt-changing)))
  (t nil t))

(defun tbt-typep-or-error (obj type)
  (handler-case (tbt-typep obj type) (error () :error)))

;;; A name that is not a type yet is an unknown type specifier (an error, as
;;; in SBCL); that answer must not be kept once the name is defined.
(deftest typep-built-tests.structure-defined-later
  (list (tbt-typep-or-error nil 'tbt-late)
        (tbt-typep-or-error (make-tbt-a) 'tbt-late)
        (progn (eval '(defstruct tbt-late q))
               (tbt-typep (funcall 'make-tbt-late) 'tbt-late))
        (tbt-typep nil 'tbt-late)
        (tbt-typep (make-tbt-a) 'tbt-late))
  (:error :error t nil nil))

(deftest typep-built-tests.deftype-defined-later
  (list (tbt-typep-or-error 3 'tbt-late-type)
        (progn (eval '(deftype tbt-late-type () 'integer))
               (tbt-typep 3 'tbt-late-type))
        (tbt-typep "x" 'tbt-late-type))
  (:error t nil))

(defstruct tbt-slots
  (next nil :type (or null tbt-slots))
  (n 0 :type tbt-small)
  (tag nil :type (member nil :a :b)))

(defun tbt-slot-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (type-error () t)))

(deftest typep-built-tests.slot-checks
  (let ((s (make-tbt-slots)))
    (list (tbt-slot-error-p (lambda () (setf (tbt-slots-next s) (make-tbt-slots))))
          (tbt-slot-error-p (lambda () (setf (tbt-slots-next s) (make-tbt-a))))
          (tbt-slot-error-p (lambda () (setf (tbt-slots-n s) 9)))
          (tbt-slot-error-p (lambda () (setf (tbt-slots-n s) 10)))
          (tbt-slot-error-p (lambda () (setf (tbt-slots-tag s) :b)))
          (tbt-slot-error-p (lambda () (setf (tbt-slots-tag s) :c)))))
  (nil t nil t nil t))
