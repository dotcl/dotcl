;;; An object that two instances made by MAKE-LOAD-FORM-SAVING-SLOTS hold in their
;;; slots is one object after the fasl is loaded, as it is in the source (CLHS
;;; 3.2.4.4 asks for the same object to stay one object in a file). Each instance
;;; used to be written on its own, so the list both held came back as two copies,
;;; and a later function whose literal held the list came back with a third.

(defclass mlfs-box () ((x :initarg :x :accessor mlfs-x) (y :initarg :y :accessor mlfs-y)))

(defmethod make-load-form ((o mlfs-box) &optional env)
  (make-load-form-saving-slots o :environment env))

(defvar *mlfs-l* (list 1 2 3))
(defvar *mlfs-inner* (make-instance 'mlfs-box :x *mlfs-l* :y 0))
(defvar *mlfs-outer* (make-instance 'mlfs-box :x *mlfs-l* :y *mlfs-inner*))
(defmacro mlfs-list-and-inner () `',(list *mlfs-l* *mlfs-inner*))
(defmacro mlfs-outer () `',*mlfs-outer*)
(defmacro mlfs-just-list () `',*mlfs-l*)

(deftest-compiled-only make-load-form-shared-across-instances.one-object
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames "mlfs.lisp" dir))
         (fasl (merge-pathnames "mlfs.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f '((defun mlfs-get-pair () (mlfs-list-and-inner))
                     (defun mlfs-get-outer () (mlfs-outer))
                     (defun mlfs-get-list () (mlfs-just-list))))
          (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (load fasl)
    (let* ((pair (funcall (intern "MLFS-GET-PAIR")))
           (outer (funcall (intern "MLFS-GET-OUTER")))
           (lst (funcall (intern "MLFS-GET-LIST"))))
      (list (equal (first pair) '(1 2 3))
            (eq (first pair) (mlfs-x (second pair)))
            (eq (mlfs-x outer) (mlfs-x (mlfs-y outer)))
            (eq (mlfs-y outer) (second pair))
            (eq lst (mlfs-x outer))
            (eq lst (first pair))
            (eq (first pair) *mlfs-l*))))
  (t t t t t t nil))
