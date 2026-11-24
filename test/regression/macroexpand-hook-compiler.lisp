;;; COMPILE, COMPILE-FILE and EVAL expand macros through *MACROEXPAND-HOOK*,
;;; as MACROEXPAND-1 does.

(defvar *mhc-seen* nil)
(defun mhc-hook (f fm env)
  (when (consp fm) (push (car fm) *mhc-seen*))
  (funcall f fm env))
(defmacro mhc-mac (x) `(list ,x))
(defun mhc-seen-by (thunk)
  (setf *mhc-seen* nil)
  (let ((*macroexpand-hook* #'mhc-hook)) (funcall thunk))
  (reverse *mhc-seen*))

(deftest-compiled-only macroexpand-hook-compiler.compile
  (and (member 'mhc-mac (mhc-seen-by (lambda () (compile nil '(lambda () (mhc-mac 1)))))) t)
  t)
(deftest-compiled-only macroexpand-hook-compiler.eval
  (and (member 'mhc-mac (mhc-seen-by (lambda () (eval '(let ((a (mhc-mac 3))) a))))) t)
  t)
(deftest-compiled-only macroexpand-hook-compiler.standard-macros
  (let ((seen (mhc-seen-by (lambda () (eval '(defun mhc-f () (when t 1)))))))
    (list (and (member 'defun seen) t) (and (member 'when seen) t)))
  (t t))
(deftest-compiled-only macroexpand-hook-compiler.compile-file
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames "mhc-src.lisp" dir))
         (fasl (merge-pathnames "mhc-src.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 '(defun mhc-g () (mhc-mac 4)) s)))
    (let ((seen (mhc-seen-by (lambda ()
                               (let ((*error-output* (make-broadcast-stream)))
                                 (compile-file src :output-file fasl))))))
      (list (and (member 'defun seen) t) (and (member 'mhc-mac seen) t))))
  (t t))
;; The hook's result is the expansion of a user macro.
(deftest-compiled-only macroexpand-hook-compiler.hook-result-used
  (let ((*macroexpand-hook* (lambda (f fm env)
                              (if (eq (car fm) 'mhc-mac) '(list 99) (funcall f fm env)))))
    (funcall (compile nil '(lambda () (mhc-mac 1)))))
  (99))
;; With the default hook nothing changes.
(deftest-compiled-only macroexpand-hook-compiler.default-hook
  (funcall (compile nil '(lambda () (mhc-mac 5))))
  (5))

;; DEFMACRO is lowered by the compiler itself and still reaches the hook.
(deftest-compiled-only macroexpand-hook-compiler.defmacro
  (let ((seen (mhc-seen-by (lambda () (eval '(defmacro mhc-dm (x) x))))))
    (and (member 'defmacro seen) t))
  t)
(deftest-compiled-only macroexpand-hook-compiler.defmacro-compile-file
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames "mhc-dm.lisp" dir))
         (fasl (merge-pathnames "mhc-dm.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 '(defmacro mhc-dm2 (x) x) s)))
    (let ((seen (mhc-seen-by (lambda ()
                               (let ((*error-output* (make-broadcast-stream)))
                                 (compile-file src :output-file fasl))))))
      (and (member 'defmacro seen) t)))
  t)
