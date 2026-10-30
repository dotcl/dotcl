;;; Argument checks found by differential runs against SBCL: COMPILE, a SETF of
;;; a symbol macro that is not a place, PROGV, MAKE-ARRAY, DEFPARAMETER /
;;; DEFCONSTANT without an initial value, ARRAY-IN-BOUNDS-P.

(defmacro %eac-signals (form)
  `(handler-case (progn ,form :no-error)
     (type-error () :type-error)
     (program-error () :program-error)
     (error () :error)))

;;; ---- COMPILE ----

(deftest-emitting-only eac.compile-bad-name
  (list (%eac-signals (compile 42 '(lambda () 1)))
        (%eac-signals (compile '(setf a b) '(lambda (v) v)))
        (%eac-signals (compile '(foo bar) '(lambda () 1))))
  (:type-error :type-error :type-error))

(deftest-emitting-only eac.compile-bad-definition
  (list (%eac-signals (compile nil 42))
        (%eac-signals (compile nil '(foo 1)))
        (%eac-signals (compile 'eac-bad-def "x")))
  (:type-error :type-error :type-error))

(deftest-emitting-only eac.compile-good-arguments
  (list (funcall (compile nil '(lambda (x) (* x 2))) 21)
        (functionp (compile nil #'car))
        (let ((name (compile 'eac-good-fn '(lambda () :good))))
          (list name (eac-good-fn)))
        (let ((name (compile '(setf eac-good-place) '(lambda (v c) (setf (car c) v)))))
          (list name (let ((c (list 1))) (funcall #'(setf eac-good-place) 9 c) c))))
  (42 t (eac-good-fn :good) ((setf eac-good-place) (9))))

(defmacro eac-mac (x) `(list ,x))

(deftest-emitting-only eac.compile-macro-name
  (let ((name (compile 'eac-mac '(lambda (form env)
                                  (declare (ignore env))
                                  `(quote (:compiled ,(cadr form)))))))
    (list name
          (fboundp 'eac-mac)
          (not (null (macro-function 'eac-mac)))
          (macroexpand-1 '(eac-mac 5))
          (eval '(eac-mac 6))))
  (eac-mac t t (quote (:compiled 5)) (:compiled 6)))

;;; ---- SETF of a symbol macro (CLHS 5.1.2.8) ----

(define-symbol-macro eac-const-sm 10)
(define-symbol-macro eac-place-sm (car eac-cell))
(defvar eac-cell (list 1))

(deftest eac.setf-symbol-macro-constant
  (list (%eac-signals (eval '(setf eac-const-sm 3)))
        eac-const-sm)
  (:program-error 10))

(define-symbol-macro eac-epoch-sm 0)

;;; The shape of SBCL's gc.lisp under its cross-build, where *GC-EPOCH* is a
;;; symbol macro for 0: the SETF sits in a function that is never called.
(deftest eac.setf-symbol-macro-constant-not-run
  (progn
    (eval '(defun eac-epoch-bump () (setf eac-epoch-sm (cons 0 0)) :bumped))
    (list (fboundp 'eac-epoch-bump)
          (%eac-signals (eac-epoch-bump))
          eac-epoch-sm))
  (t :program-error 0))

(deftest-emitting-only eac.setf-symbol-macro-constant-compile-file
  (let ((src (regression-temp-file "eac-epoch.lisp")))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (write-string "(defun eac-epoch-bump-2 () (setf eac-epoch-sm (cons 0 0)) :bumped)" o))
    (multiple-value-bind (fasl warnings-p failure-p) (compile-file src)
      (declare (ignore warnings-p))
      (load fasl)
      (list (not (null fasl)) failure-p (%eac-signals (eac-epoch-bump-2)))))
  (t nil :program-error))

(deftest eac.setf-symbol-macro-place
  (progn (eval '(setf eac-place-sm 7))
         (list eac-place-sm eac-cell))
  (7 (7)))

;;; ---- PROGV ----

(deftest eac.progv-not-a-list
  (list (%eac-signals (eval '(progv 'eac-pv-a '(1) 1)))
        (%eac-signals (eval '(progv '(eac-pv-a) 1 1)))
        (%eac-signals (progv (list* 'eac-pv-a 'eac-pv-b) '(1) 1))
        (%eac-signals (progv '(eac-pv-a) (list* 1 2) 1)))
  (:type-error :type-error :type-error :type-error))

(defvar eac-pv-a 0)
(deftest eac.progv-good
  (list (progv '(eac-pv-a) '(5) (symbol-value 'eac-pv-a))
        (progv '(eac-pv-a) '() (boundp 'eac-pv-a))
        (progv '() '(1 2) :empty)
        eac-pv-a)
  (5 nil :empty 0))

;;; ---- MAKE-ARRAY ----

(deftest eac.make-array-fill-pointer-range
  (list (%eac-signals (make-array 3 :fill-pointer -1))
        (%eac-signals (make-array 3 :fill-pointer 4))
        (%eac-signals (make-array 3 :fill-pointer 1.0))
        (%eac-signals (make-array 3 :fill-pointer (expt 2 70)))
        (%eac-signals (make-array 3 :fill-pointer :yes)))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest eac.make-array-fill-pointer-rank
  (list (%eac-signals (make-array '(2 2) :fill-pointer 1))
        (%eac-signals (make-array '(2 2) :fill-pointer t))
        (%eac-signals (make-array '() :fill-pointer t))
        (array-dimensions (make-array '(2 2) :fill-pointer nil)))
  (:error :error :error (2 2)))

(deftest eac.make-array-fill-pointer-good
  (list (length (make-array 3 :fill-pointer 0))
        (length (make-array 3 :fill-pointer 3))
        (length (make-array 3 :fill-pointer t))
        (array-has-fill-pointer-p (make-array 3 :fill-pointer nil))
        (let ((v (make-array 4 :element-type 'character :fill-pointer 2
                               :initial-element #\c)))
          (list (length v) (array-has-fill-pointer-p v) (array-element-type v))))
  (0 3 3 nil (2 t character)))

(deftest eac.make-array-initial-element-type
  (list (%eac-signals (make-array 3 :element-type '(integer 0 100) :initial-element 101))
        (%eac-signals (make-array 3 :element-type 'double-float :initial-element 1))
        (%eac-signals (make-array 3 :element-type 'bit :initial-element nil))
        (%eac-signals (make-array 3 :element-type 'character :initial-element 0))
        (%eac-signals (make-array '(2 2) :element-type '(unsigned-byte 8) :initial-element -1))
        (%eac-signals (let ((et '(integer 0 100))) (make-array 3 :element-type et :initial-element 101))))
  (:type-error :type-error :type-error :type-error :type-error :type-error))

(deftest eac.make-array-initial-element-good
  (list (coerce (make-array 3 :element-type '(integer 0 100) :initial-element 100) 'list)
        (coerce (make-array 2 :element-type 'double-float :initial-element 1d0) 'list)
        (coerce (make-array 2 :element-type 'fixnum :initial-element 7) 'list)
        (coerce (make-array 2 :element-type 'bit :initial-element 1) 'list)
        (coerce (make-array 2 :element-type 'character :initial-element #\z) 'string)
        (coerce (make-array 2 :element-type 'symbol :initial-element 5) 'list))
  ((100 100 100) (1d0 1d0) (7 7) (1 1) "zz" (5 5)))

(deftest eac.make-array-initial-contents-length
  (list (%eac-signals (make-array 3 :initial-contents '(1 2)))
        (%eac-signals (make-array 3 :initial-contents '(1 2 3 4)))
        (%eac-signals (make-array 3 :initial-contents #(1 2)))
        (%eac-signals (make-array 2 :element-type 'character :initial-contents "abc"))
        (%eac-signals (make-array '(2 2) :initial-contents '((1 2) (3))))
        (%eac-signals (make-array '(2 2) :initial-contents '((1 2))))
        (%eac-signals (make-array '(2 2) :initial-contents '(1 2)))
        (%eac-signals (make-array 2 :initial-contents 5))
        (%eac-signals (make-array 2 :initial-contents (list* 1 2))))
  (:error :error :error :error :error :error :type-error :type-error :type-error))

(defun %eac-shape (a)
  (list (array-dimensions a)
        (loop for i below (array-total-size a) collect (row-major-aref a i))))

(deftest eac.make-array-initial-contents-good
  (list (%eac-shape (make-array 3 :initial-contents '(1 2 3)))
        (%eac-shape (make-array 3 :initial-contents #(a b c)))
        (make-array 3 :element-type 'character :initial-contents "abc")
        (%eac-shape (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
        (%eac-shape (make-array '(2 3) :initial-contents #(#(1 2 3) (4 5 6))))
        (%eac-shape (make-array '(2 2) :element-type 'character :initial-contents '("ab" "cd")))
        (%eac-shape (make-array '() :initial-contents '(1 2)))
        (%eac-shape (make-array 0 :initial-contents '()))
        (%eac-shape (make-array '(0 2) :initial-contents '()))
        (%eac-shape (make-array 2 :initial-contents
                                (make-array 4 :fill-pointer 2 :initial-contents '(x y z w)))))
  (((3) (1 2 3)) ((3) (a b c)) "abc" ((2 3) (1 2 3 4 5 6)) ((2 3) (1 2 3 4 5 6))
   ((2 2) (#\a #\b #\c #\d)) (() ((1 2))) ((0) ()) ((0 2) ()) ((2) (x y))))

(deftest eac.make-array-both-initials
  (%eac-signals (make-array 3 :initial-element 1 :initial-contents '(1 2 3)))
  :error)

;;; ---- DEFPARAMETER / DEFCONSTANT without an initial value ----

(deftest eac.defparameter-defconstant-missing-value
  (list (%eac-signals (macroexpand '(defparameter eac-no-value)))
        (%eac-signals (macroexpand '(defconstant eac-no-value)))
        (%eac-signals (eval '(defparameter eac-no-value)))
        (%eac-signals (eval '(defconstant eac-no-value)))
        (boundp 'eac-no-value)
        (%eac-signals (macroexpand '(defvar eac-no-value))))
  (:program-error :program-error :program-error :program-error nil :no-error))

;;; ---- ARRAY-IN-BOUNDS-P / ARRAY-ROW-MAJOR-INDEX ----

(deftest eac.array-subscript-count
  (let ((a (make-array '(2 3))))
    (list (%eac-signals (array-in-bounds-p a 1))
          (%eac-signals (array-in-bounds-p a 1 1 1))
          (%eac-signals (array-row-major-index a 1))
          (array-in-bounds-p a 1 2)
          (array-in-bounds-p a 2 0)
          (array-row-major-index a 1 2)))
  (:program-error :program-error :program-error t nil 5))
