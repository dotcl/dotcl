;;; While a DEFMACRO is compiled, the compiler registers an expander so that the
;;; rest of the file can use the macro. That expander is now compiled only when
;;; something expands a call: a top level DEFMACRO is also run at compile time
;;; and replaces it before any use, so compiling it up front doubled the work.

(defvar *cfdme-expansions* 0)
(defmacro cfdme-counted (x) (incf *cfdme-expansions*) x)

(defun %cfdme-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

;; The expander body is compiled once, for the fasl.
(deftest-compiled-only cf-deferred-macro-expander.compiled-once
  (let* ((fasl (%cfdme-compile "cfdme-a"
                               '("(defmacro cfdme-a-m (x) (cl-user::cfdme-counted (list 'quote x)))")))
         (after-compile *cfdme-expansions*))
    (load fasl)
    (list after-compile (macroexpand-1 '(cfdme-a-m 7))))
  (1 '7))

;; A DEFMACRO that a macro call produces is not run at compile time on its own;
;; the registered expander is what the rest of the file uses.
(deftest-compiled-only cf-deferred-macro-expander.generated-defmacro-used-later
  (let ((fasl (%cfdme-compile
               "cfdme-b"
               '("(defmacro cfdme-b-def (name value) `(defmacro ,name (&optional (k 1)) (list '* k ,value)))"
                 "(cfdme-b-def cfdme-b-six 6)"
                 "(defparameter *cfdme-b-seen* (list (cfdme-b-six) (cfdme-b-six 7)))"))))
    (load fasl)
    (symbol-value (intern "*CFDME-B-SEEN*")))
  (6 42))

;; LOAD of source: the macro works right after its definition.
(deftest cf-deferred-macro-expander.load-source
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "cfdme-c.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defmacro cfdme-c-m (a b) `(list ,b ,a))" s)
      (write-line "(defparameter *cfdme-c-seen* (cfdme-c-m 1 2))" s))
    (load src)
    (symbol-value (intern "*CFDME-C-SEEN*")))
  (2 1))
