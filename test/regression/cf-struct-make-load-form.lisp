;;; A structure literal in a fasl whose type has its own MAKE-LOAD-FORM is
;;; made by that method's creation form, evaluated when the fasl is loaded
;;; (CLHS 3.2.4.4), and the method is called once per object in the file.
;;; The creation form used to run only when the literal was first used, a
;;; structure inside a hash table literal was rebuilt from its slots without
;;; the method, and the method was called several times per object.

(defvar *csmlf-calls* 0)
(defvar *csmlf-made* 0)

(defstruct (csmlf-box (:constructor %make-csmlf-box (v))) v)

(defun make-csmlf-box-counted (v) (incf *csmlf-made*) (%make-csmlf-box v))

(defmethod make-load-form ((b csmlf-box) &optional env)
  (declare (ignore env))
  (incf *csmlf-calls*)
  `(make-csmlf-box-counted ',(csmlf-box-v b)))

(defvar *csmlf-list* (list (%make-csmlf-box 1) (%make-csmlf-box "two")))
(defvar *csmlf-table*
  (let ((h (make-hash-table))) (setf (gethash :k h) (%make-csmlf-box :in-table)) h))
(defmacro csmlf-list () `',*csmlf-list*)
(defmacro csmlf-one () `',(first *csmlf-list*))
(defmacro csmlf-table () `',*csmlf-table*)

(deftest-compiled-only cf-struct-make-load-form.creation-at-load
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "csmlf.lisp" dir))
         (fasl (merge-pathnames "csmlf.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f '((defun csmlf-get-list () (csmlf-list))
                     (defun csmlf-get-one () (csmlf-one))
                     (defun csmlf-get-table () (csmlf-table))))
          (prin1 f s) (terpri s))))
    (setf *csmlf-calls* 0)
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (let ((calls *csmlf-calls*))
      (setf *csmlf-made* 0)
      (load fasl)
      (let ((made-at-load *csmlf-made*)
            (l (funcall (intern "CSMLF-GET-LIST"))))
        (list calls
              made-at-load
              (mapcar #'csmlf-box-v l)
              (eq (first l) (funcall (intern "CSMLF-GET-ONE")))
              (csmlf-box-v (gethash :k (funcall (intern "CSMLF-GET-TABLE"))))
              (eq (first l) (first *csmlf-list*))
              *csmlf-made*))))
  (3 3 (1 "two") t :in-table nil 3))
