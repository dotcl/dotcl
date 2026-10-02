;;; Literals that a fasl builds once per load -- objects described by
;;; MAKE-LOAD-FORM-SAVING-SLOTS and large hash table literals -- are stored
;;; as data and rebuilt by the runtime, not by code JIT-compiled at load to run
;;; once. The objects must come back as the code route built them: every kind
;;; of slot value, instances reached only through a hash table or a vector
;;; (created by their MAKE-LOAD-FORM before the literal that holds them), and
;;; one object per instance across literals.

(defpackage :cfdl-user (:use :cl))

(defclass cfdl-node ()
  ((name :initarg :name :accessor cfdl-name)
   (data :initarg :data :accessor cfdl-data)))

(defmethod make-load-form ((n cfdl-node) &optional env)
  (make-load-form-saving-slots n :environment env))

(defun %cfdl-compile (name forms)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(defvar *cfdl-gensym* (make-symbol "CFDL-G"))

(defvar *cfdl-leaf* (make-instance 'cfdl-node :name :leaf :data nil))

(defvar *cfdl-values*
  (list 1.5f0 2.25d0 (expt 2 100) 3/7 #c(1 2) #\x "str" *cfdl-gensym* :kw
        'cfdl-user::sym '(1 2 . 3) (vector 1 *cfdl-leaf* "v") -5
        (let ((h (make-hash-table :test 'equal)))
          (setf (gethash "k" h) *cfdl-leaf*)
          h)))

(defvar *cfdl-node* (make-instance 'cfdl-node :name 'top :data *cfdl-values*))

;; A table large enough for the prototype route, whose values are instances
;; nothing else in the file names.
(defvar *cfdl-table*
  (let ((h (make-hash-table :test 'eq)))
    (dotimes (i 40)
      (setf (gethash (intern (format nil "K~d" i) :cfdl-user) h)
            (make-instance 'cfdl-node :name i :data (if (= i 3) *cfdl-leaf* nil))))
    h))

(defmacro cfdl-lit (x) `',(symbol-value x))

(deftest-compiled-only cf-fasl-data-literals.values
  (progn
    (load (%cfdl-compile "cfdl-a" '((defun cfdl-node () (cfdl-lit *cfdl-node*))
                                    (defun cfdl-leaf () (cfdl-lit *cfdl-leaf*)))))
    (let* ((n (funcall (intern "CFDL-NODE")))
           (d (cfdl-data n))
           (leaf (funcall (intern "CFDL-LEAF"))))
      (list (cfdl-name n)
            (subseq d 0 7)
            (symbol-name (nth 7 d)) (symbol-package (nth 7 d))
            (nth 8 d) (nth 9 d) (nth 10 d)
            (aref (nth 11 d) 0) (eq (aref (nth 11 d) 1) leaf) (aref (nth 11 d) 2)
            (nth 12 d)
            (eq (gethash "k" (nth 13 d)) leaf)
            (hash-table-test (nth 13 d)))))
  (top (1.5f0 2.25d0 #.(expt 2 100) 3/7 #c(1 2) #\x "str")
   "CFDL-G" nil :kw cfdl-user::sym (1 2 . 3) 1 t "v" -5 t equal))

(deftest-compiled-only cf-fasl-data-literals.table-of-instances
  (progn
    (load (%cfdl-compile "cfdl-b" '((defun cfdl-table () (cfdl-lit *cfdl-table*))
                                    (defun cfdl-leaf2 () (cfdl-lit *cfdl-leaf*)))))
    (let ((h (funcall (intern "CFDL-TABLE"))))
      (list (hash-table-count h)
            (cfdl-name (gethash 'cfdl-user::k7 h))
            (eq (cfdl-data (gethash 'cfdl-user::k3 h)) (funcall (intern "CFDL-LEAF2")))
            (eq (gethash 'cfdl-user::k7 h) (gethash 'cfdl-user::k7 (funcall (intern "CFDL-TABLE"))))
            (eq h (funcall (intern "CFDL-TABLE"))))))
  (40 7 t t t))

;; Building forty large tables at load does not JIT-compile code per table.
(defvar *cfdl-many*
  (loop for j below 40
        collect (let ((h (make-hash-table :test 'equal)))
                  (dotimes (i 30) (setf (gethash (format nil "t~d-~d" j i) h) (list j i)))
                  h)))
(defmacro cfdl-many (j) `',(nth j *cfdl-many*))

(defun %cfdl-jit-count ()
  (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t))

(deftest-compiled-only cf-fasl-data-literals.tables-not-code
  (let* ((fasl (%cfdl-compile "cfdl-many"
                              (list `(defun cfdl-all ()
                                       (list ,@(loop for j below 40 collect `(cfdl-many ,j)))))))
         (j0 (%cfdl-jit-count))
         (tables (progn (load fasl) (funcall (intern "CFDL-ALL")))))
    (list (< (- (%cfdl-jit-count) j0) 40)
          (length tables)
          (gethash "t39-29" (nth 39 tables))))
  (t 40 (39 29)))

;; Objects whose slots hold large, nearly equal tables (a code walker's
;; environment, saved per scope): each table is a copy of a prototype the
;; fasl keeps once, not the whole table again per object.
(defun %cfdl-env-nodes (n)
  (let ((base (make-hash-table :test 'eq)))
    (dotimes (i 300) (setf (gethash (intern (format nil "E~d" i) :cfdl-user) base) (list i)))
    (loop for j below n
          collect (let ((h (make-hash-table :test 'eq)))
                    (maphash (lambda (k v) (setf (gethash k h) v)) base)
                    (setf (gethash 'cfdl-user::own h) j)
                    (make-instance 'cfdl-node :name j :data h)))))

(defvar *cfdl-env* nil)
(defmacro cfdl-env () `',*cfdl-env*)

(defun %cfdl-env-fasl (name n)
  (setf *cfdl-env* (%cfdl-env-nodes n))
  (%cfdl-compile name (list `(defun ,(intern (string-upcase name)) () (cfdl-env)))))

(defun %cfdl-size (fasl)
  (with-open-file (s fasl :element-type '(unsigned-byte 8)) (file-length s)))

(deftest-compiled-only cf-fasl-data-literals.env-tables
  (let* ((one (%cfdl-size (%cfdl-env-fasl "cfdl-env1" 1)))
         (fasl (%cfdl-env-fasl "cfdl-env30" 30))
         (thirty (%cfdl-size fasl)))
    (load fasl)
    (let ((objs (funcall (intern "CFDL-ENV30"))))
      (list (< thirty (* 2 one))
            (length objs)
            (gethash 'cfdl-user::own (cfdl-data (nth 29 objs)))
            (gethash 'cfdl-user::e150 (cfdl-data (nth 29 objs)))
            (eq (cfdl-data (nth 1 objs)) (cfdl-data (nth 2 objs))))))
  (t 30 29 (150) nil))

;; Sharing: an object that occurs twice in one literal, and one that two
;; literals of the same form contain, stays one object (also when the literal
;; holds an instance, which only the data route takes).
(defvar *cfdl-sub* (list 1 2 3 4 5 6 7 *cfdl-leaf*))
(defmacro cfdl-twice () `',(list *cfdl-sub* *cfdl-sub* (list :x *cfdl-leaf*)))
(defmacro cfdl-cross () `(list ',*cfdl-sub* '(:wrap 1 2 3 4 5 6 ,*cfdl-sub*)))

(deftest-compiled-only cf-fasl-data-literals.sharing
  (progn
    (load (%cfdl-compile "cfdl-share" '((defun cfdl-twice () (cfdl-twice))
                                        (defun cfdl-cross () (cfdl-cross))
                                        (defun cfdl-leaf3 () (cfdl-lit *cfdl-leaf*)))))
    (let ((tw (funcall (intern "CFDL-TWICE")))
          (cr (funcall (intern "CFDL-CROSS")))
          (leaf (funcall (intern "CFDL-LEAF3"))))
      (list (eq (first tw) (second tw))
            (subseq (first tw) 0 7)
            (eq (car (last (first tw))) leaf)
            (eq (first cr) (car (last (second cr))))
            (eq (car (last (first cr))) leaf))))
  (t (1 2 3 4 5 6 7) t t t))
