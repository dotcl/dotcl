;;; A fasl whose first MAKE-LOAD-FORM object is already interned (the state a reload is in after
;;; a load that stopped part way) still loads: the values of the interned object are read, not
;;; skipped, so the shared literal it defines is there for the objects after it.

(defclass mpi-c () ((x :initarg :x :accessor mpi-x) (y :initarg :y :accessor mpi-y)))
(defmethod make-load-form ((o mpi-c) &optional env) (make-load-form-saving-slots o :environment env))

(defvar *mpi-lit* nil)
(defmacro mpi-lit () `',*mpi-lit*)

(defun %mpi-key (fasl)
  "The key of the first object the fasl creates: NAME.inst.0, found in its bytes."
  (with-open-file (s fasl :element-type '(unsigned-byte 8))
    (let* ((n (file-length s))
           (v (make-array n :element-type '(unsigned-byte 8)))
           (txt (progn (read-sequence v s) (map 'string #'code-char v)))
           (end (search ".inst.0" txt)))
      (when end
        (let ((start end))
          (loop while (and (> start 0)
                           (let ((c (char txt (1- start))))
                             (or (alphanumericp c) (find c "_.-"))))
                do (decf start))
          (subseq txt start (+ end (length ".inst.0"))))))))

(defun %mpi-run (preintern)
  (let* ((c (list 1 2 3 4 5 6 7))
         (d (list 8 9 10 11 12 13 14))
         (dir (regression-temp-dir/))
         (src (merge-pathnames "mpi-partial.lisp" dir))
         (fasl (merge-pathnames (if preintern "mpi-partial-a.fasl" "mpi-partial-b.fasl") dir)))
    (setf *mpi-lit* (list (make-instance 'mpi-c :x c :y c)
                          (make-instance 'mpi-c :x d :y d)
                          (make-instance 'mpi-c :x c :y c)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 '(defun mpi-get () (mpi-lit)) s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (when preintern
      (let ((key (%mpi-key fasl)))
        (dotnet:static "DotCL.LispInstance" "InternViaEval" key
                       '(make-instance 'mpi-c :x (list 1 2 3 4 5 6 7) :y (list 1 2 3 4 5 6 7)))))
    (load fasl)
    (let ((r (funcall 'mpi-get)))
      (list (mpi-x (first r)) (mpi-x (second r)) (mpi-x (third r)) (mpi-y (third r))
            (eq (mpi-x (third r)) (mpi-y (third r)))))))

(deftest-compiled-only cf-mlf-partial-intern.baseline
  (%mpi-run nil)
  ((1 2 3 4 5 6 7) (8 9 10 11 12 13 14) (1 2 3 4 5 6 7) (1 2 3 4 5 6 7) t))

(deftest-compiled-only cf-mlf-partial-intern.first-object-interned
  (%mpi-run t)
  ((1 2 3 4 5 6 7) (8 9 10 11 12 13 14) (1 2 3 4 5 6 7) (1 2 3 4 5 6 7) t))
