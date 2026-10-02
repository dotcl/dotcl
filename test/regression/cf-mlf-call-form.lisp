;;; A MAKE-LOAD-FORM whose creation form is a call of a global function on
;;; constants -- (make-thing 'name 3 :k) -- is run at load by calling the
;;; function, not by handing the form to EVAL, which compiled each one. Forms
;;; that are anything else still go to EVAL: a macro call, an argument that
;;; is a variable.

(defvar *cfmc-calls* 0)

(defclass cfmc-thing ()
  ((name :initarg :name :reader cfmc-thing-name)
   (n :initarg :n :reader cfmc-thing-n)))

(defun %make-cfmc-thing (name n) (make-instance 'cfmc-thing :name name :n n))

(defun make-cfmc-thing (name n &key (bump 1))
  (incf *cfmc-calls* bump)
  (%make-cfmc-thing name n))

(defmacro make-cfmc-thing-m (name n) `(make-cfmc-thing ,name ,n))

(defvar *cfmc-n* 42)

(defmethod make-load-form ((x cfmc-thing) &optional env)
  (declare (ignore env))
  (case (cfmc-thing-name x)
    (:macro `(make-cfmc-thing-m :macro ,(cfmc-thing-n x)))
    (:var `(make-cfmc-thing :var *cfmc-n*))
    (t `(make-cfmc-thing ',(cfmc-thing-name x) ,(cfmc-thing-n x) :bump 1))))

(defvar *cfmc-objs* nil)
(defmacro cfmc-lit () `',*cfmc-objs*)

(defun %cfmc-compile (name)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 `(defun ,(intern (string-upcase name)) () (cfmc-lit)) s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only cf-mlf-call-form.values
  (progn
    (setf *cfmc-objs* (list (%make-cfmc-thing 'alpha 1) (%make-cfmc-thing "beta" 2.5d0)
                            (%make-cfmc-thing :macro 3) (%make-cfmc-thing :var 0)))
    (let ((fasl (%cfmc-compile "cfmc-a")))
      (setf *cfmc-calls* 0)
      (load fasl)
      (let ((objs (funcall (intern "CFMC-A"))))
        (list *cfmc-calls*
              (mapcar #'cfmc-thing-name objs)
              (mapcar #'cfmc-thing-n objs)))))
  (4 (alpha "beta" :macro :var) (1 2.5d0 3 42)))

(defun %cfmc-jit-count ()
  (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t))

;; 200 objects are not 200 forms compiled at load.
(deftest-compiled-only cf-mlf-call-form.not-compiled
  (progn
    (setf *cfmc-objs* (loop for i below 200 collect (%make-cfmc-thing (intern (format nil "T~d" i)) i)))
    (let* ((fasl (%cfmc-compile "cfmc-many"))
           (j0 (%cfmc-jit-count)))
      (load fasl)
      (let ((objs (funcall (intern "CFMC-MANY"))))
        (list (< (- (%cfmc-jit-count) j0) 100)
              (cfmc-thing-n (nth 199 objs))))))
  (t 199))
