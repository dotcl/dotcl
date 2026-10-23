;;; COMPILE-FILE and LOAD bind *PACKAGE*, *READTABLE*, *COMPILE-FILE-PATHNAME*
;;; and the compiler's own per-file state on the thread that calls them. They
;;; used to assign the global values and put them back at the end, so a second
;;; thread read its file in the first one's package, and whichever compile-file
;;; finished first switched the other out of compile-file mode. Here two threads
;;; with no bindings of their own compile a file each: a macro in the first file
;;; holds it until the second thread is inside the second file, and a macro
;;; there waits for the first file to finish before the rest of the second one
;;; is compiled.

(require "dotcl-thread")

(defvar *cftb-held* nil)
(defvar *cftb-go* nil)
(defvar *cftb-a-done* nil)

(defun %cftb-wait (var)
  (loop repeat 2000 until (symbol-value var) do (sleep 0.005))
  (symbol-value var))

(defmacro %cftb-hold ()
  (setf *cftb-held* t)
  (%cftb-wait '*cftb-go*)
  nil)

(defmacro %cftb-release-and-wait ()
  (setf *cftb-go* t)
  (%cftb-wait '*cftb-a-done*)
  nil)

(defun %cftb-write (path lines)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (dolist (l lines) (write-line l s))))

(defun %cftb-global (sym)
  (let ((v nil))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread (lambda () (setf v (symbol-value sym)))))
    v))

(deftest-compiled-only cf-thread-bindings.two-threads
  (let* ((dir (regression-temp-dir))
         (src-a (merge-pathnames "cftb-a.lisp" dir))
         (fasl-a (merge-pathnames "cftb-a.fasl" dir))
         (src-b (merge-pathnames "cftb-b.lisp" dir))
         (fasl-b (merge-pathnames "cftb-b.fasl" dir))
         (package-before (%cftb-global '*package*))
         (package-seen nil))
    (setf *cftb-held* nil *cftb-go* nil *cftb-a-done* nil)
    (%cftb-write src-a '("(defpackage :cftb-a (:use :cl))"
                         "(in-package :cftb-a)"
                         "(defun hold () (cl-user::%cftb-hold))"
                         "(defstruct pt x)"
                         "(defun f () (pt-x (make-pt :x 'here)))"))
    (%cftb-write src-b '("(defpackage :cftb-b (:use :cl))"
                         "(in-package :cftb-b)"
                         "(defun hold () (cl-user::%cftb-release-and-wait))"
                         "(defstruct qt y)"
                         "(defun g () (qt-y (make-qt :y 'there)))"))
    (let ((ta (dotcl-thread:make-thread
               (lambda ()
                 (let ((*error-output* (make-broadcast-stream)))
                   (compile-file src-a :output-file fasl-a))
                 (setf *cftb-a-done* t)))))
      (%cftb-wait '*cftb-held*)
      (setf package-seen (%cftb-global '*package*))
      (dotcl-thread:thread-join
       (dotcl-thread:make-thread
        (lambda ()
          (let ((*error-output* (make-broadcast-stream)))
            (compile-file src-b :output-file fasl-b)))))
      (dotcl-thread:thread-join ta))
    (load fasl-a)
    (load fasl-b)
    (flet ((call (name pkg)
             (let ((f (find-symbol name pkg)))
               (if (and f (fboundp f))
                   (let ((v (funcall f)))
                     (list (symbol-name v) (package-name (symbol-package v))))
                   :undefined))))
      (list (eq package-seen package-before)
            (eq (%cftb-global '*package*) package-before)
            (call "F" "CFTB-A")
            (call "G" "CFTB-B"))))
  (t t ("HERE" "CFTB-A") ("THERE" "CFTB-B")))
