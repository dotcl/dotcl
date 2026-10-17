;;; Structure literals in a fasl, built from data at load rather than by code:
;;; structures without a MAKE-LOAD-FORM, with MAKE-LOAD-FORM-SAVING-SLOTS for
;;; every slot or for some slots, and with a creation form of their own, inside
;;; lists, vectors and other structures, shared within one literal and across
;;; literals, and in a cycle.

(defpackage :cfds-user (:use :cl))

(defstruct cfds-plain a b)

(defstruct cfds-saving x y)
(defmethod make-load-form ((s cfds-saving) &optional env)
  (make-load-form-saving-slots s :environment env))

(defstruct cfds-subset p q)
(defmethod make-load-form ((s cfds-subset) &optional env)
  (make-load-form-saving-slots s :slot-names '(p) :environment env))

(defstruct cfds-custom n)
(defmethod make-load-form ((s cfds-custom) &optional env)
  (declare (ignore env))
  `(make-cfds-custom :n ,(1+ (cfds-custom-n s))))

(defun %cfds-compile (name forms)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(defvar *cfds-shared* (make-cfds-plain :a 1 :b "s"))

(defvar *cfds-tree*
  (list (make-cfds-plain :a (make-cfds-saving :x 1.5d0 :y '(a b))
                         :b (vector *cfds-shared* :k))
        *cfds-shared*
        (make-cfds-subset :p 'p1 :q 'q1)
        (make-cfds-custom :n 41)
        (list 1 2 3 4 5 6)))

(defvar *cfds-root* (make-cfds-plain :a (list *cfds-shared* *cfds-shared* 3 4 5 6)
                                     :b (make-cfds-saving :x #\c :y 7)))

(defvar *cfds-cycle*
  (let* ((s (make-cfds-plain :a 1))
         (l (list 1 2 3 4 5 s)))
    (setf (cfds-plain-b s) l)
    l))

(defmacro cfds-lit (x) `',(symbol-value x))

(deftest-compiled-only cf-fasl-data-structs.tree
  (progn
    (load (%cfds-compile "cfds-a" '((defun cfds-tree () (cfds-lit *cfds-tree*))
                                    (defun cfds-shared () (cfds-lit *cfds-shared*)))))
    (let* ((tr (funcall (intern "CFDS-TREE")))
           (p (first tr))
           (sv (cfds-plain-a p)))
      (list (cfds-saving-x sv) (cfds-saving-y sv)
            (eq (aref (cfds-plain-b p) 0) (second tr))
            (eq (second tr) (funcall (intern "CFDS-SHARED")))
            (cfds-plain-a (second tr)) (cfds-plain-b (second tr))
            (cfds-subset-p (third tr))
            (cfds-custom-n (fourth tr))
            (fifth tr)
            (eq tr (funcall (intern "CFDS-TREE"))))))
  (1.5d0 (a b) t t 1 "s" p1 42 (1 2 3 4 5 6) t))

(deftest-compiled-only cf-fasl-data-structs.root
  (progn
    (load (%cfds-compile "cfds-b" '((defun cfds-root () (cfds-lit *cfds-root*)))))
    (let ((r (funcall (intern "CFDS-ROOT"))))
      (list (type-of r)
            (eq (first (cfds-plain-a r)) (second (cfds-plain-a r)))
            (cfds-plain-a (first (cfds-plain-a r)))
            (cfds-saving-x (cfds-plain-b r))
            (eq r (funcall (intern "CFDS-ROOT"))))))
  (cfds-plain t 1 #\c t))

(deftest-compiled-only cf-fasl-data-structs.cycle
  (progn
    (load (%cfds-compile "cfds-c" '((defun cfds-cycle () (cfds-lit *cfds-cycle*)))))
    (let* ((l (funcall (intern "CFDS-CYCLE")))
           (s (sixth l)))
      (list (cfds-plain-a s) (eq (cfds-plain-b s) l))))
  (1 t))

;;; The same in an image that only loads the fasl, where no structure the
;;; compiler saw is there to be found by its key.

(defparameter *cfds-child-src* "(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct cfdx-plain a b)
  (defstruct cfdx-saving x y)
  (defmethod make-load-form ((s cfdx-saving) &optional env)
    (make-load-form-saving-slots s :environment env))
  (defstruct cfdx-subset p q)
  (defmethod make-load-form ((s cfdx-subset) &optional env)
    (make-load-form-saving-slots s :slot-names '(p) :environment env))
  (defvar *cfdx-shared* (make-cfdx-plain :a 1 :b \"s\"))
  (defvar *cfdx-tree*
    (list (make-cfdx-plain :a (make-cfdx-saving :x 1.5d0 :y '(a b))
                           :b (vector *cfdx-shared* :k))
          *cfdx-shared*
          (make-cfdx-subset :p 'p1 :q 'q1)
          (list 1 2 3 4 5 6))))
(defmacro cfdx-lit (x) `',(symbol-value x))
(defun cfdx-tree () (cfdx-lit *cfdx-tree*))
(defun cfdx-shared () (cfdx-lit *cfdx-shared*))
")

(defun %cfds-child-run ()
  (let* ((dir (regression-temp-dir))
         (src (namestring (merge-pathnames "cfdx.lisp" dir)))
         (fasl (namestring (merge-pathnames "cfdx.fasl" dir)))
         (exe (dotnet:static "System.Environment" "ProcessPath"))
         (core (regression-child-core)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *cfds-child-src* s))
    (let ((r1 (dotcl:run-process exe (list "--core" core "--eval"
                                           (format nil "(compile-file ~s :output-file ~s)" src fasl)))))
      (if (/= (first r1) 0)
          r1
          (let* ((r2 (dotcl:run-process
                      exe (list "--core" core "--eval"
                                (format nil "(progn (load ~s) (let* ((tr (cfdx-tree)) (p (first tr)) (sv (cfdx-plain-a p))) (format t \"~~%CFDX ~~s\" (list (type-of p) (cfdx-saving-x sv) (cfdx-saving-y sv) (eq (aref (cfdx-plain-b p) 0) (second tr)) (eq (second tr) (cfdx-shared)) (cfdx-plain-b (second tr)) (cfdx-subset-p (third tr)) (fourth tr) (eq tr (cfdx-tree))))))" fasl))))
                 (out (second r2))
                 (at (search "CFDX " out)))
            (if at (values (read-from-string out nil out :start (+ at 5))) r2))))))

(deftest-compiled-only cf-fasl-data-structs.fresh-image
  (%cfds-child-run)
  (cfdx-plain 1.5d0 (a b) t t "s" p1 (1 2 3 4 5 6) t))

;;; A structure without a MAKE-LOAD-FORM method is dumped from its slots. The
;;; compiler asks for the method first and falls back when there is none; the
;;; error that asking raises is its own business, not the user's, so a
;;; HANDLER-BIND or *DEBUGGER-HOOK* around COMPILE-FILE does not see it.
(deftest-compiled-only cf-fasl-data-structs.no-method-signals-nothing
  (let ((seen '()))
    (block out
      (let ((*debugger-hook* (lambda (c h) (declare (ignore h))
                               (push (list :hook (type-of c)) seen)
                               (return-from out))))
        (handler-bind ((error (lambda (c) (push (type-of c) seen))))
          (%cfds-compile "cfds-quiet" '((defun cfds-quiet () (cfds-lit *cfds-shared*)))))))
    seen)
  nil)
