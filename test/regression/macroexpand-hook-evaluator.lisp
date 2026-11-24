;;; COMPILE-FILE announces the DEFCLASS / DEFGENERIC / DEFMETHOD forms it flattens
;;; at the top level to *MACROEXPAND-HOOK*. The evaluator that expands them does
;;; not call the hook, so the hook is told, and the hook's own body, which the
;;; evaluator also expands, must not announce itself again.

(require "dotcl-thread")

(defvar *mhe-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))
(defvar *mhe-core* (regression-child-core))

(defun mhe-write (path forms)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (let ((*package* (find-package :cl-user)))
      (dolist (f forms) (prin1 f s) (terpri s)))))

(defun mhe-heads (hook source-forms name)
  "The heads of the forms HOOK was shown while NAME's source was compiled."
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (mhe-write src source-forms)
    (let ((*macroexpand-hook* hook) (*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))))

(defvar *mhe-seen* nil)
(defun mhe-record (f fm env)
  (when (consp fm) (push (car fm) *mhe-seen*))
  (funcall f fm env))

(deftest-emitting-only macroexpand-hook-evaluator.definers
  (progn
    (setf *mhe-seen* nil)
    (mhe-heads #'mhe-record
               '((defclass mhe-c1 () ())
                 (defgeneric mhe-g1 (x))
                 (defmethod mhe-g1 ((x mhe-c1)) x))
               "mhe-definers")
    (mapcar (lambda (s) (and (member s *mhe-seen*) t)) '(defclass defgeneric defmethod)))
  (t t t))

;; An error, a throw and a RETURN-FROM out of the hook leave nothing behind: the
;; next compile still announces.
(deftest-emitting-only macroexpand-hook-evaluator.hook-leaves-by-error
  (let ((first (handler-case
                   (mhe-heads (lambda (f fm env) (declare (ignore f fm env)) (error "hook failed"))
                              '((defclass mhe-c2 () ())) "mhe-err")
                 (error () :error))))
    (declare (ignorable first))
    (setf *mhe-seen* nil)
    (mhe-heads #'mhe-record '((defclass mhe-c3 () ())) "mhe-after-error")
    (and (member 'defclass *mhe-seen*) t))
  t)
(deftest-emitting-only macroexpand-hook-evaluator.hook-leaves-by-throw
  (progn
    (catch 'mhe-out
      (mhe-heads (lambda (f fm env) (declare (ignore f fm env)) (throw 'mhe-out :thrown))
                 '((defclass mhe-c4 () ())) "mhe-throw"))
    (setf *mhe-seen* nil)
    (mhe-heads #'mhe-record '((defclass mhe-c5 () ())) "mhe-after-throw")
    (and (member 'defclass *mhe-seen*) t))
  t)
(deftest-emitting-only macroexpand-hook-evaluator.hook-leaves-by-return-from
  (progn
    (block out
      (mhe-heads (lambda (f fm env) (declare (ignore f fm env)) (return-from out :left))
                 '((defclass mhe-c6 () ())) "mhe-return"))
    (setf *mhe-seen* nil)
    (mhe-heads #'mhe-record '((defclass mhe-c7 () ())) "mhe-after-return")
    (and (member 'defclass *mhe-seen*) t))
  t)

;; Each thread binds its own hook and sees its own forms only.
(deftest-emitting-only macroexpand-hook-evaluator.threads
  (let* ((n 4) (go nil)
         (results (make-array n :initial-element nil))
         (threads
           (loop for i below n
                 collect (let ((i i))
                           (dotcl-thread:make-thread
                            (lambda ()
                              (loop until go do (dotcl-thread:thread-yield))
                              (let* ((seen '())
                                     (name (intern (format nil "MHE-T~D" i) :cl-user)))
                                (mhe-heads (lambda (f fm env)
                                             (when (and (consp fm) (eq (car fm) 'defclass))
                                               (push (second fm) seen))
                                             (funcall f fm env))
                                           `((defclass ,name () ()))
                                           (format nil "mhe-thread-~D" i))
                                (setf (aref results i) (equal seen (list name))))))))))
    (setf go t)
    (mapc #'dotcl-thread:thread-join threads)
    (coerce results 'list))
  (t t t t))

;; An interpreted hook whose body holds INCF, LOOP and WHEN is expanded by the
;; evaluator on every call. It runs in a child: if the announcement re-entered,
;; the process would die of a stack overflow that nothing can catch.
(deftest-emitting-only macroexpand-hook-evaluator.interpreted-hook-in-child
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames "mhe-child-src.lisp" dir))
         (fasl (merge-pathnames "mhe-child-src.fasl" dir))
         (driver (merge-pathnames "mhe-child-driver.lisp" dir)))
    (mhe-write src '((defclass mhe-ch () ()) (defgeneric mhe-chg (x)) (defmethod mhe-chg ((x mhe-ch)) x)))
    (mhe-write driver
               `((defvar *count* 0)
                 (defvar *seen* nil)
                 (defvar *hook*
                   (let ((dotcl:*evaluator-mode* :interpret))
                     (eval '(lambda (f fm env)
                             (incf *count*)
                             (loop for i below 1
                                   do (when (consp fm) (push (car fm) *seen*)))
                             (funcall f fm env)))))
                 (let ((*macroexpand-hook* *hook*) (*error-output* (make-broadcast-stream)))
                   (compile-file ,(namestring src) :output-file ,(namestring fasl)))
                 (format t "~%MHE-RESULT ~S~%"
                         (list (and (member 'defclass *seen*) t)
                               (and (member 'defgeneric *seen*) t)
                               (and (member 'defmethod *seen*) t)
                               (< *count* 1000)))))
    (let ((r (dotcl:run-process *mhe-exe* (list "--core" *mhe-core* (namestring driver)))))
      (unwind-protect
           (if (and (consp r) (eql (first r) 0))
               (let* ((out (second r)) (at (search "MHE-RESULT " out)))
                 (if at (values (read-from-string out nil out :start (+ at 11))) out))
               r)
        (dolist (f (list src fasl driver)) (ignore-errors (delete-file f))))))
  (t t t t))
