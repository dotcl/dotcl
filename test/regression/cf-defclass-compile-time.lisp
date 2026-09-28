;;; COMPILE-FILE does not create the classes a file's top-level DEFCLASS forms
;;; define. CLHS asks the compiler only to recognize the name for later
;;; declarations, specializers and :METACLASS options in the same file; the
;;; class itself is made when the fasl is loaded. COMPILE-FILE used to run the
;;; whole DEFCLASS at compile time, so the class existed after compiling, and
;;; the LOAD that followed reinitialized it: a metaclass saw
;;; REINITIALIZE-INSTANCE (without :NAME) where it expected
;;; INITIALIZE-INSTANCE. Expected values were checked against SBCL.

(defvar *cfdct-log* nil)

(defun %cfdct-compile-and-load (name text)
  (let* ((dir (format nil "~a/dotcl-cfdct-~a/" (regression-temp-dir)
                      (get-internal-real-time)))
         (dir (substitute #\/ #\\ dir))
         (src (concatenate 'string dir name ".lisp"))
         (fasl (concatenate 'string dir name ".fasl")))
    (ensure-directories-exist dir)
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (write-string text s))
    (setf *cfdct-log* nil)
    (compile-file src :output-file fasl)
    (let ((after-compile (reverse *cfdct-log*)))
      (setf *cfdct-log* nil)
      (lambda ()
        (load fasl)
        (list after-compile (reverse *cfdct-log*))))))

(deftest-compiled-only cf-defclass-compile-time.no-class-after-compile
  (let ((load (%cfdct-compile-and-load
               "cfdct-a"
               "(defclass cfdct-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c cfdct-meta) (s standard-class)) t)
(defmethod initialize-instance :after ((c cfdct-meta) &key name)
  (push (list :ii name) *cfdct-log*))
(defmethod reinitialize-instance :after ((c cfdct-meta) &key)
  (push :ri *cfdct-log*))
(defclass cfdct-plain () ((x :initarg :x :reader cfdct-plain-x)))
(defclass cfdct-foo () ((x :initarg :x :reader cfdct-foo-x))
  (:metaclass cfdct-meta))
")))
    (let ((before (list (find-class 'cfdct-plain nil)
                        (find-class 'cfdct-foo nil)
                        (find-class 'cfdct-meta nil)
                        (fboundp 'cfdct-plain-x))))
      (let ((logs (funcall load)))
        (list before logs
              (cfdct-foo-x (make-instance 'cfdct-foo :x 1))
              (cfdct-plain-x (make-instance 'cfdct-plain :x 2))))))
  ((nil nil nil nil) (nil ((:ii cfdct-foo))) 1 2))

;;; The name is still usable by the forms after it in the same file: a
;;; subclass, a method specializer, a declaration and a :METACLASS option.
(deftest-compiled-only cf-defclass-compile-time.name-usable-later-in-file
  (let ((load (%cfdct-compile-and-load
               "cfdct-b"
               "(defclass cfdct-meta2 (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c cfdct-meta2) (s standard-class)) t)
(defclass cfdct-base () ((a :initarg :a :initform 1)))
(defclass cfdct-sub (cfdct-base) ((b :initform 2)) (:metaclass cfdct-meta2))
(defgeneric cfdct-f (x))
(defmethod cfdct-f ((x cfdct-base)) (list :base (slot-value x 'a)))
(defun cfdct-g (x) (declare (type cfdct-base x)) (slot-value x 'a))
")))
    (funcall load)
    (list (cfdct-f (make-instance 'cfdct-sub :a 5))
          (cfdct-g (make-instance 'cfdct-base))
          (class-name (class-of (find-class 'cfdct-sub)))))
  ((:base 5) 1 cfdct-meta2))

;;; Compiling a redefinition leaves the live class alone until the fasl loads.
(defclass cfdct-live () ((a :initform 1)))

(deftest-compiled-only cf-defclass-compile-time.redefinition-waits-for-load
  (let ((load (%cfdct-compile-and-load
               "cfdct-c"
               "(defclass cfdct-live () ((a :initform 1) (b :initform 2)))
")))
    (let ((before (slot-exists-p (make-instance 'cfdct-live) 'b)))
      (funcall load)
      (list before (slot-value (make-instance 'cfdct-live) 'b))))
  (nil 2))

;;; CLHS DEFCLASS: while the rest of the file is compiled, FIND-CLASS given a
;;; macro's environment returns the class definition. Without the environment
;;; it answers from the global table, which does not have the class yet. SBCL
;;; returns NIL in both cases (ANSI FIND-CLASS.24 fails there), so this value
;;; follows the standard rather than SBCL.
(defvar *cfdct-env-result* nil)

(deftest-compiled-only cf-defclass-compile-time.find-class-with-environment
  (let ((load (%cfdct-compile-and-load
               "cfdct-d"
               "(defclass cfdct-env () ())
(defmacro cfdct-env-probe (&environment env)
  (let ((c (find-class 'cfdct-env nil env)))
    `(list ',(and c (class-name c)) ',(find-class 'cfdct-env nil))))
(setf *cfdct-env-result* (cfdct-env-probe))
")))
    (funcall load)
    (list *cfdct-env-result* (class-name (find-class 'cfdct-env))))
  ((cfdct-env nil) cfdct-env))

;;; The name is a known type for the declarations after the DEFCLASS in the same
;;; file, so declaring it warns about nothing (COMPILE-FILE's WARNINGS-P is NIL,
;;; as in SBCL). local-time's (declare (type timestamp ...)) warned "names no
;;; known type" once the class stopped being made at compile time.
(deftest-compiled-only cf-defclass-compile-time.declared-type-known
  (let* ((dir (format nil "~a/dotcl-cfdct-~a/" (regression-temp-dir)
                      (get-internal-real-time)))
         (dir (substitute #\/ #\\ dir))
         (src (concatenate 'string dir "cfdct-e.lisp"))
         (fasl (concatenate 'string dir "cfdct-e.fasl")))
    (ensure-directories-exist dir)
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (write-string "(defclass cfdct-decl-type () ((a :initarg :a :initform 3)))
(defun cfdct-decl-f (x) (declare (type cfdct-decl-type x)) (slot-value x 'a))
" s))
    (multiple-value-bind (out warnings-p failure-p) (compile-file src :output-file fasl)
      (declare (ignore out))
      (load fasl)
      (list warnings-p failure-p
            (funcall 'cfdct-decl-f (make-instance 'cfdct-decl-type)))))
  (nil nil 3))
