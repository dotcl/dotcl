;;; An instance literal whose MAKE-LOAD-FORM is MAKE-LOAD-FORM-SAVING-SLOTS is
;;; rebuilt at load by direct calls (allocate the instance, set each slot), not
;;; by handing its creation form to EVAL, which compiled and JIT-compiled a
;;; method per object at every load. cl-environments dumps thousands of such
;;; objects into a generic-cl fasl.

(defclass cssld-point ()
  ((x :initarg :x :accessor cssld-x)
   (y :initarg :y :accessor cssld-y)
   (unbound-slot)))

(defmethod make-load-form ((p cssld-point) &optional env)
  (make-load-form-saving-slots p :environment env))

(defvar *cssld-a* (make-instance 'cssld-point :x 1 :y (list "two" 'three)))
(defvar *cssld-b* (make-instance 'cssld-point :x *cssld-a* :y 2))

(defmacro cssld-lit (name) `',(symbol-value name))

(defun %cssld-compile (name forms)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(defvar *cssld-fasl* nil)

(deftest-compiled-only cssld.values
  (progn
    (setf *cssld-fasl* (%cssld-compile "cssld" '((defun cssld-a () (cssld-lit *cssld-a*))
                                                 (defun cssld-b () (cssld-lit *cssld-b*)))))
    (load *cssld-fasl*)
    (let ((a (funcall (intern "CSSLD-A"))) (b (funcall (intern "CSSLD-B"))))
      (list (cssld-x a) (cssld-y a) (slot-boundp a 'unbound-slot)
            (eq (cssld-x b) a) (cssld-y b)
            (eq a *cssld-a*))))
  (1 ("two" three) nil t 2 nil))

;; No code is generated per object at load: the methods JIT-compiled while
;; loading grow by about one per object (its creation method), not by the
;; extra compiled form EVAL made for each.
(defun %cssld-many (name n)
  (let ((objs (loop for i below n collect (make-instance 'cssld-point :x i :y (list i)))))
    (setf (symbol-value (intern (format nil "*~a*" (string-upcase name)))) objs)
    (%cssld-compile name `((defun ,(intern (string-upcase name)) ()
                             (cssld-lit ,(intern (format nil "*~a*" (string-upcase name)))))))))

(defun %cssld-jit-count ()
  (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t))

(deftest-compiled-only cssld.no-code-per-object
  (let* ((f10 (%cssld-many "cssld-ten" 10))
         (f50 (%cssld-many "cssld-fifty" 50))
         (j0 (%cssld-jit-count))
         (d10 (progn (load f10) (funcall (intern "CSSLD-TEN")) (- (%cssld-jit-count) j0)))
         (j1 (%cssld-jit-count))
         (d50 (progn (load f50) (funcall (intern "CSSLD-FIFTY")) (- (%cssld-jit-count) j1))))
    (list (< (- d50 d10) 60)
          (mapcar #'cssld-x (funcall (intern "CSSLD-FIFTY")))))
  (t #.(loop for i below 50 collect i)))

;; As before: loading the same fasl again finds the objects already made and
;; leaves them alone.
(deftest-compiled-only cssld.reload-keeps-objects
  (let ((a (funcall (intern "CSSLD-A"))))
    (setf (cssld-x a) 99)
    (load *cssld-fasl*)
    (list (eq a (funcall (intern "CSSLD-A"))) (cssld-x (funcall (intern "CSSLD-A")))))
  (t 99))
