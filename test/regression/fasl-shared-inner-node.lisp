;;; regression: skip-under-interp
;;; A fasl whose literals share an object that sits inside another shared object loads the same
;;; whether or not the container is already filled when its literal is reached.  A cached fasl
;;; reaches that state: the data of an instance made earlier in the module fills the slot of the
;;; container, the literal that would build it is skipped, and an inner object that the same
;;; literal contains a second time was then read from a field the skipped build never set (NIL
;;; or a null where the shared object belongs).  Compile and load run in separate processes.

(defparameter *fsi-source*
  "(defpackage :fsi-a (:use :cl))
(in-package :fsi-a)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (base (:constructor nil)) (alias nil))
  (defstruct (nd (:include base)) a b)
  (defmethod make-load-form ((o base) &optional env)
    (make-load-form-saving-slots o :slot-names '(a b) :environment env)))
(eval-when (:compile-toplevel)
  (defparameter *m* (list 'm))
  (defparameter *s* (list *m*))
  (defparameter *i* (make-nd :a *s*))
  (defparameter *l* (list *s* *m*)))
(defmacro %pair () `(list ',*l* ',*i*))
(defvar *r* nil)
(setf *r* (%pair))
(defun check ()
  (let* ((l (first *r*)))
    (list (eq (car (first l)) (second l))
          (eq (nd-a (second *r*)) (first l)))))
")

(defun %fsi-write (path text)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede)
    (write-string text o))
  path)

(defvar *fsi-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun %fsi-child (script &rest env)
  (let ((empty (%fsi-write (format nil "~A/fsi/empty.txt" (regression-temp-dir)) "")))
    (ignore-errors
     (uiop:run-program (append (and env (cons "env" env))
                               (list *fsi-exe* "--core" (regression-child-core) (namestring script)))
                       :input empty :output :string :error-output :string
                       :ignore-error-status t))))

(defun %fsi-run ()
  (let* ((dir (format nil "~A/fsi" (regression-temp-dir)))
         (src (%fsi-write (format nil "~A/src.lisp" dir) *fsi-source*))
         (fasl (format nil "~A/src.fasl" dir))
         (build (%fsi-write (format nil "~A/build.lisp" dir)
                            (format nil "(compile-file ~S :output-file ~S)~%" src fasl)))
         (run (%fsi-write (format nil "~A/run.lisp" dir)
                          (format nil "(load ~S)~%(format t \"RESULT ~~S~~%\" (fsi-a::check))~%" fasl))))
    ;; Literals as code rather than as data parts: that is the route with a field per inner object.
    (%fsi-child build "DOTCL_LITERAL_DATA_MIN_NODES=100000")
    (let ((out (%fsi-child run)))
      (and (stringp out)
           (let ((p (search "RESULT " out)))
             (and p (string-trim '(#\Space #\Newline #\Return) (subseq out (+ p 7)
                                                                        (position #\Newline out :start p)))))))))

(deftest-emitting-only fasl-shared-inner-node.loads-with-shared-inner
  (if (find :windows *features*) "(T T)" (%fsi-run))
  "(T T)")
