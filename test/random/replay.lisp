;;;; Random integer form test, replay side.
;;;;
;;;; Reads CASES written by generate.lisp, compiles each case's optimized
;;;; lambda form on this implementation and writes one line per case:
;;;;   (INDEX RESULT-1 RESULT-2 ...)
;;;; where a result is an integer or :ERROR.  Run it on dotcl and on SBCL
;;;; and diff the two files: any differing line is a case where the two
;;;; implementations disagree.
;;;;
;;;; A case whose form mentions FIXNUM or BIGNUM (the generator emits
;;;; (TYPEP x 'BIGNUM)) is written as (INDEX :FIXNUM-WIDTH) instead: the
;;;; fixnum range is implementation-defined (dotcl 64 bits, SBCL 62), so the
;;;; two answers may legitimately differ.  The in-implementation check in
;;;; generate.lisp still covers these cases.
;;;;
;;;; Parameters (set with --eval before --load):
;;;;   cl-user::*rf-case-timeout*  seconds per case on SBCL (default 120)
;;;;   cl-user::*rf-from*   resume: skip cases with a smaller index, append results
;;;;   cl-user::*rf-fasl*   true: compile each case with COMPILE-FILE and LOAD
;;;;   cl-user::*rf-opaque*  true: wrap call arguments in RF-OPAQUE (extra-defs.lisp)
;;;;   cl-user::*rf-out*     directory holding cases.lsp (default "out/random-forms/")
;;;;   cl-user::*rf-result*  output file name inside it (required)

(in-package :cl-user)

(defvar *rf-out* "out/random-forms/")
(defvar *rf-result*)

(load "test/random/load.lisp")
(load "test/random/extra-defs.lisp")

(in-package :cl-test)

(defun rf-fixnum-width-dependent-p (form)
  (let ((names '("FIXNUM" "BIGNUM" "MOST-POSITIVE-FIXNUM" "MOST-NEGATIVE-FIXNUM")))
    (labels ((walk (x)
               (cond ((symbolp x) (member (symbol-name x) names :test #'string=))
                     ((consp x) (or (walk (car x)) (walk (cdr x))))
                     (t nil))))
      (walk form))))

(defvar cl-user::*rf-case-timeout* 120)

(defmacro rf-with-case-timeout ((out index) &body body)
  "On SBCL, a case whose compile or run takes longer than
   CL-USER::*RF-CASE-TIMEOUT* seconds is written as (INDEX :TIMEOUT) instead
   of stopping the replay (SBCL 2.6.8 has been seen not to finish compiling a
   generated form). The Makefile does not count such a line as a difference."
  #+sbcl
  `(handler-case (sb-ext:with-timeout cl-user::*rf-case-timeout* ,@body)
     (sb-ext:timeout ()
       (format ,out "~S~%" (list ,index :timeout))
       (finish-output ,out)))
  #-sbcl
  (progn out index `(progn ,@body)))

(defvar cl-user::*rf-opaque* nil)

(defun rf-opaque-form (x)
  "X with each argument of a call to a global function wrapped in RF-OPAQUE."
  (cond ((atom x) x)
        ((member (car x) '(quote function declare)) x)
        ;; Clause heads that name a type or a restart (which may also name a
        ;; function: ERROR, CONS, USE-VALUE) are not calls.
        ((member (car x) '(handler-case restart-case))
         (list* (car x) (rf-opaque-form (second x))
                (mapcar (lambda (cl) (list* (first cl) (second cl)
                                            (mapcar #'rf-opaque-form (cddr cl))))
                        (cddr x))))
        ;; Places are left as written: a wrapped subform of a place (the
        ;; plist of GETF, the integer of LDB) is no longer a place.
        ((member (car x) '(setf psetf))
         (cons (car x) (loop for (p v) on (cdr x) by #'cddr
                             collect p collect (rf-opaque-form v))))
        ((member (car x) '(incf decf))
         (list* (car x) (second x) (mapcar #'rf-opaque-form (cddr x))))
        ((member (car x) '(push pushnew))
         (list* (car x) (rf-opaque-form (second x)) (third x) (mapcar #'rf-opaque-form (cdddr x))))
        ((member (car x) '(pop rotatef)) x)
        ((eq (car x) 'shiftf)
         (append (butlast x) (list (rf-opaque-form (car (last x))))))
        ((eq (car x) 'handler-bind)
         (list* (car x)
                (mapcar (lambda (b) (list (first b) (rf-opaque-form (second b)))) (second x))
                (mapcar #'rf-opaque-form (cddr x))))
        ((member (car x) '(typecase etypecase ctypecase))
         (list* (car x) (rf-opaque-form (second x))
                (mapcar (lambda (cl) (cons (first cl) (mapcar #'rf-opaque-form (rest cl))))
                        (cddr x))))
        ((and (symbolp (car x)) (fboundp (car x)) (not (macro-function (car x)))
              (not (special-operator-p (car x))))
         (cons (car x) (mapcar (lambda (a) `(rf-opaque ,(rf-opaque-form a))) (cdr x))))
        (t (let ((r '()) (c x))
             (loop while (consp c) do (push (rf-opaque-form (car c)) r) (setf c (cdr c)))
             (let ((l (nreverse r))) (if c (append l c) l))))))

(defvar cl-user::*rf-fasl* nil)
(defvar cl-user::*rf-from* nil
  "When an index, skip the cases before it and append to the result file: to
   resume a replay the implementation itself died in.")

(defun rf-compile-through-fasl (src dir)
  "The function SRC (a LAMBDA form) as COMPILE-FILE and LOAD make it: written
   as a DEFUN to a source file, compiled to a fasl, loaded. NIL when no fasl
   is written."
  (let ((source (merge-pathnames
                 ;; One file per implementation: the two replays may run at once.
                 (format nil "fasl-case-~(~A~).lisp"
                         (substitute #\- #\Space (lisp-implementation-type)))
                 dir))
        (name (intern "RF-FASL-CASE" :cl-test)))
    (fmakunbound name)
    (with-open-file (o source :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* (find-package :cl-test)) (*print-circle* t)
              (*print-readably* nil))
          (prin1 '(in-package :cl-test) o) (terpri o)
          (prin1 `(defun ,name ,@(cdr src)) o) (terpri o))))
    ;; FAILURE-P is ignored: SBCL sets it for any WARNING (an unused-value
    ;; or a type mismatch in dead code of a generated form), while the plain
    ;; COMPILE path uses such a function anyway.
    (multiple-value-bind (fasl warnings-p failure-p)
        (let ((*compile-verbose* nil) (*compile-print* nil))
          (compile-file source))
      (declare (ignore warnings-p failure-p))
      (when fasl
        (let ((*load-verbose* nil)) (load fasl))
        (and (fboundp name) (fdefinition name))))))

(defmacro rf-with-printer-defaults (&body body)
  "The printer variables at their standard values while a case runs: their
   global values differ between implementations (SBCL starts with
   *PRINT-PRETTY* true), and a case that prints compares lengths."
  `(let ((*print-pretty* nil) (*print-circle* nil) (*print-base* 10)
         (*print-radix* nil) (*print-length* nil) (*print-level* nil)
         (*print-escape* t) (*print-readably* nil) (*print-case* :upcase))
     ,@body))

(defun rf-replay ()
  (let* ((dir (merge-pathnames cl-user::*rf-out* (truename ".")))
         (n 0))
    (with-open-file (in (merge-pathnames "cases.lsp" dir))
      (with-open-file (out (merge-pathnames cl-user::*rf-result* dir)
                           :direction :output
                           :if-exists (if cl-user::*rf-from* :append :supersede))
        (let ((*package* (find-package :cl-test)))
          (loop for c = (read in nil in)
                until (eq c in)
                do (incf n)
                   (cond
                     ((and cl-user::*rf-from* (< (getf c :index) cl-user::*rf-from*)))
                     ((rf-fixnum-width-dependent-p (getf c :form))
                       (format out "~S~%" (list (getf c :index) :fixnum-width)))
                     (t
                   (rf-with-case-timeout (out (getf c :index))
                   (let* ((src (make-optimized-lambda-form
                                (if cl-user::*rf-opaque*
                                    (rf-opaque-form (getf c :form))
                                    (getf c :form))
                                (getf c :vars)
                                (getf c :var-types) (getf c :decls1)))
                          (fn (cl:handler-case
                                  (cl:handler-bind ((warning #'muffle-warning))
                                    (let ((*error-output* (make-broadcast-stream)))
                                      (if cl-user::*rf-fasl*
                                          (rf-compile-through-fasl src dir)
                                          (compile nil src))))
                                (error () nil)))
                          (results
                            (if (null fn)
                                (list :compile-error)
                                (loop for vals in (getf c :vals-list)
                                      collect (cl:handler-case
                                                  (cl:handler-bind ((warning #'muffle-warning))
                                                    (rf-with-printer-defaults (apply fn vals)))
                                                (error () :error))))))
                     (let ((*print-pretty* nil) (*print-base* 10) (*print-radix* nil))
                       (format out "~S~%" (cons (getf c :index) results))
                       (finish-output out))))))))))
    (format t "~&replay: ~D cases -> ~A~%" n cl-user::*rf-result*)))

(rf-replay)
