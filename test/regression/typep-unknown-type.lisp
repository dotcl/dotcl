;;; TYPEP on a type specifier that names nothing (no built-in type, class or
;;; DEFTYPE) signals an error instead of answering NIL, as SBCL does. So do the
;;; forms built on it (CHECK-TYPE, TYPECASE). A type that is only defined after
;;; the code naming it was compiled is looked up when the code runs, so the
;;; forward reference still works.

(defun tut-typep (obj type) (typep obj type))

(deftest typep-unknown-type.symbol
  (mapcar (lambda (ty)
            (handler-case (progn (tut-typep nil ty) :no-error)
              (error () :error)))
          '(tut-undefined-type (tut-undefined-type)
            (and tut-undefined-type null) (not tut-undefined-type)))
  (:error :error :error :error))

(deftest typep-unknown-type.or-short-circuit
  (tut-typep nil '(or null tut-undefined-type))
  t)

(deftest typep-unknown-type.instance
  (progn
    (defclass tut-some-class () ())
    (handler-case (progn (tut-typep (make-instance 'tut-some-class) 'tut-undefined-type) :no-error)
      (error () :error)))
  :error)

(deftest typep-unknown-type.check-type-typecase
  (list (handler-case (let ((x 1)) (check-type x tut-undefined-type) :no-error)
          (error () :error))
        (handler-case (typecase 1 (tut-undefined-type :a) (t :b))
          (error () :error)))
  (:error :error))

(defun tut-fwd (x) (typecase x (tut-fwd-class :inst) (t :other)))
(defun tut-fwd-check (x) (check-type x tut-fwd-struct) :ok)
(defun tut-fwd-deftype (x) (typep x 'tut-fwd-deftype))
(defclass tut-fwd-class () ())
(defstruct tut-fwd-struct a)
(deftype tut-fwd-deftype () 'string)

(deftest typep-unknown-type.forward-reference
  (list (tut-fwd (make-instance 'tut-fwd-class)) (tut-fwd 1)
        (tut-fwd-check (make-tut-fwd-struct))
        (tut-fwd-deftype "a") (tut-fwd-deftype 1))
  (:inst :other :ok t nil))

;; A class that a DEFCLASS earlier in the file being compiled names is a known
;; type while that file is compiled, before the class itself exists.
(defvar *tut-compile-time-result* nil)
(deftest-compiled-only typep-unknown-type.compile-time-class
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "tut-ctc.lisp" dir))
         (fasl (merge-pathnames "tut-ctc.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (prin1 '(defclass tut-ctc-class () ()) s)
      (prin1 '(defmacro tut-ctc-macro ()
               (list 'quote (handler-case (typep nil 'tut-ctc-class)
                              (error () :error))))
             s)
      (prin1 '(setf *tut-compile-time-result* (tut-ctc-macro)) s))
    (setf *tut-compile-time-result* :unset)
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (load fasl)
    *tut-compile-time-result*)
  nil)
