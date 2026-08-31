;;; A make-load-form literal has to survive a garbage collection.
;;;
;;; A reference to such a literal compiles to a lookup by key: the creation form
;;; runs once, in its own top-level method, and every use reads the cache. That
;;; cache held the result weakly, so a collection between load and use dropped it
;;; -- and the lookup then answered NIL rather than complaining. A fasl that was
;;; right when freshly compiled started handing out NIL later in the same session.
;;;
;;; It surfaced through CFFI: defcallback embeds a foreign-type object as a
;;; literal, so after a collection an argument declared :string stopped being
;;; translated and a raw pointer reached Lisp code as an integer.

(defclass fasl-load-form-thing ()
  ((k :initarg :k :reader fasl-load-form-thing-k)))

(defmethod make-load-form ((x fasl-load-form-thing) &optional environment)
  (declare (ignore environment))
  (list 'make-instance ''fasl-load-form-thing :k (fasl-load-form-thing-k x)))

(defvar *fllf-source*
  (concatenate 'string
               (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp"))
               "/dotcl-fasl-load-form-literal.lisp"))

(with-open-file (out *fllf-source* :direction :output :if-exists :supersede)
  ;; #. so the instance is a literal in the compiled file, not something the
  ;; loaded code builds for itself.
  (write-line "(in-package :cl-user)" out)
  (write-line "(defun fllf-literal () '#.(make-instance 'cl-user::fasl-load-form-thing :k 7))"
              out))

(load (compile-file *fllf-source*))

(defun fllf-collect ()
  (dotimes (i 3)
    (dotnet:static "System.GC" "Collect")
    (dotnet:static "System.GC" "WaitForPendingFinalizers")))

(deftest fllf-literal-survives-collection
  (progn
    (fllf-literal)                      ; the reference before collecting
    (fllf-collect)
    (let ((again (fllf-literal)))
      (list (type-of again)
            (and (typep again 'fasl-load-form-thing)
                 (fasl-load-form-thing-k again)))))
  (fasl-load-form-thing 7))

;;; Every reference is the same object, which is what interning by key is for.
(deftest fllf-literal-is-shared
  (progn (fllf-collect) (eq (fllf-literal) (fllf-literal)))
  t)
