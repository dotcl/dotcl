;;; COMPILE-FILE does not evaluate at compile time a form whose macro expansion
;;; failed: it compares a count of failed expansions before and after compiling
;;; the form. The count was one global variable, so a failure in a file another
;;; thread was compiling at the same time also stopped the evaluation, and a
;;; DEFMACRO or EVAL-WHEN in this file silently did nothing at compile time.
;;; Here thread A's macro fails while thread B is inside an EVAL-WHEN form.

(require "dotcl-thread")

(defvar *mxf-b-inside* nil)
(defvar *mxf-a-failed* nil)
(defvar *mxf-ran* nil)

(defun %mxf-wait (var)
  (loop repeat 2000 until (symbol-value var) do (sleep 0.005))
  (symbol-value var))

(defmacro %mxf-sync ()
  (setf *mxf-b-inside* t)
  (%mxf-wait '*mxf-a-failed*)
  t)

(defmacro %mxf-fail ()
  (%mxf-wait '*mxf-b-inside*)
  (error "mxf: this expansion fails"))

(defmacro %mxf-mark ()
  (setf *mxf-a-failed* t)
  nil)

(defun %mxf-write (path lines)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (dolist (l lines) (write-line l s))))

(deftest-compiled-only cf-expansion-failure-threads.other-file
  (let* ((dir (regression-temp-dir))
         (src-a (merge-pathnames "mxf-a.lisp" dir))
         (src-b (merge-pathnames "mxf-b.lisp" dir)))
    (setf *mxf-b-inside* nil *mxf-a-failed* nil *mxf-ran* nil)
    (%mxf-write src-a '("(defun mxf-a1 () (cl-user::%mxf-fail))"
                        "(defun mxf-a2 () (cl-user::%mxf-mark))"))
    (%mxf-write src-b '("(eval-when (:compile-toplevel :load-toplevel :execute)"
                        "  (setq cl-user::*mxf-ran* (cl-user::%mxf-sync)))"))
    (flet ((compile-quietly (src)
             (let ((*error-output* (make-broadcast-stream)))
               (handler-bind ((warning #'muffle-warning))
                 (compile-file src :output-file (merge-pathnames
                                                 (make-pathname :type "fasl") src))))))
      (let ((ta (dotcl-thread:make-thread (lambda () (compile-quietly src-a))))
            (tb (dotcl-thread:make-thread (lambda () (compile-quietly src-b)))))
        (dotcl-thread:thread-join ta)
        (dotcl-thread:thread-join tb)))
    (list *mxf-a-failed* *mxf-ran*))
  (t t))
