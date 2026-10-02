;;; Consecutive MAKE-LOAD-FORM-SAVING-SLOTS instance literals in a fasl are
;;; created by one shared method rather than one method each. The objects must
;;; come back exactly as before: slot values, EQ between objects that refer to
;;; each other, a hash table in a slot, and an object that cannot be batched
;;; (a structure in a slot) between batched ones.

(defpackage :cmb-user (:use :cl))

(defclass cmb-node ()
  ((name :initarg :name :accessor cmb-name)
   (next :initarg :next :accessor cmb-next)
   (table :initarg :table :accessor cmb-table)
   (unset)))

(defmethod make-load-form ((n cmb-node) &optional env)
  (make-load-form-saving-slots n :environment env))

(defstruct cmb-box value)
(defmethod make-load-form ((b cmb-box) &optional env)
  (make-load-form-saving-slots b :environment env))

(defun %cmb-objects (n)
  (let ((prev nil) (all nil))
    (dotimes (i n)
      (let ((h (make-hash-table :test 'eq)))
        (setf (gethash 'cmb-user::k h) i)
        (setf prev (make-instance 'cmb-node
                                  :name (list i (intern (format nil "S~d" i) :cmb-user))
                                  :next (if (= i 7) (make-cmb-box :value i) prev)
                                  :table h))
        (push prev all)))
    (nreverse all)))

(defvar *cmb-objs* nil)
(defmacro cmb-lit () `',*cmb-objs*)

(defun %cmb-compile (name n)
  (setf *cmb-objs* (%cmb-objects n))
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 `(defun ,(intern (string-upcase name)) () (cmb-lit)) s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only cmb.values
  (progn
    (load (%cmb-compile "cmb-small" 20))
    (let ((objs (funcall (intern "CMB-SMALL"))))
      (list (length objs)
            (mapcar (lambda (o) (car (cmb-name o))) objs)
            (eq (cadr (cmb-name (nth 3 objs))) (find-symbol "S3" :cmb-user))
            ;; each node's NEXT is the object before it, as one object
            (loop for (a b) on objs while b
                  always (or (typep (cmb-next b) 'cmb-box) (eq (cmb-next b) a)))
            (cmb-box-value (cmb-next (nth 7 objs)))
            (null (cmb-next (first objs)))
            (gethash 'cmb-user::k (cmb-table (nth 12 objs)))
            (slot-boundp (nth 5 objs) 'unset))))
  (20 #.(loop for i below 20 collect i) t t 7 t 12 nil))

;; 200 objects are not 200 methods to JIT-compile at load.
(defun %cmb-jit-count ()
  (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t))

(deftest-compiled-only cmb.shared-methods
  (let* ((fasl (%cmb-compile "cmb-many" 200))
         (j0 (%cmb-jit-count))
         (objs (progn (load fasl) (funcall (intern "CMB-MANY")))))
    (list (< (- (%cmb-jit-count) j0) 100)
          (length objs)
          (car (cmb-name (nth 199 objs)))))
  (t 200 199))
