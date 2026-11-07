;;; Literals made by MAKE-LOAD-FORM are made afresh each time a fasl is loaded, even when a
;;; second fasl has the same name and source text as the first, and an object two slots of one
;;; literal contain stays one object.

(defclass mrs-c () ((x :initarg :x :accessor mrs-x) (y :initarg :y :accessor mrs-y)))
(defmethod make-load-form ((o mrs-c) &optional env) (make-load-form-saving-slots o :environment env))
(defstruct (mrs-s (:constructor mrs-make (a))) a)
(defmethod make-load-form ((o mrs-s) &optional env) (make-load-form-saving-slots o :environment env))

(defvar *mrs-obj* nil)
(defmacro mrs-lit () `',*mrs-obj*)

(defun %mrs-load (obj)
  (setf *mrs-obj* obj)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "mrs-same.lisp" dir))
         (fasl (merge-pathnames "mrs-same.fasl" dir)))
    ;; the same text every time: only the literal differs
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 '(defun mrs-get () (mrs-lit)) s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (load fasl)
    (funcall 'mrs-get)))

(deftest-compiled-only cf-mlf-reload.instance
  (list (mrs-x (%mrs-load (make-instance 'mrs-c :x 1 :y 0)))
        (mrs-x (%mrs-load (make-instance 'mrs-c :x 2 :y 0)))
        (mrs-x (%mrs-load (make-instance 'mrs-c :x 3 :y 0))))
  (1 2 3))

(deftest-compiled-only cf-mlf-reload.structure
  (list (mrs-s-a (%mrs-load (mrs-make 1)))
        (mrs-s-a (%mrs-load (mrs-make 2))))
  (1 2))

(deftest-compiled-only cf-mlf-slot-sharing
  (let* ((l (list 1 2))
         (r (%mrs-load (make-instance 'mrs-c :x l :y l)))
         (v (%mrs-load (make-instance 'mrs-c :x (vector l) :y (list l)))))
    (list (eq (mrs-x r) (mrs-y r))
          (eq (aref (mrs-x v) 0) (car (mrs-y v)))))
  (t t))

;;; An object shared between a literal and an instance's slots, or between two instances, stays one object.
(deftest-compiled-only cf-mlf-sharing-across-instances
  (let* ((l (list 1 2))
         (b (make-instance 'mrs-c :x l :y 0))
         (r1 (%mrs-load (make-instance 'mrs-c :x l :y b)))
         (r2 (%mrs-load (list l b)))
         (r3 (%mrs-load (list (make-instance 'mrs-c :x l :y 0) l))))
    (list (eq (mrs-x r1) (mrs-x (mrs-y r1)))
          (eq (first r2) (mrs-x (second r2)))
          (eq (second r3) (mrs-x (first r3)))))
  (t t t))
