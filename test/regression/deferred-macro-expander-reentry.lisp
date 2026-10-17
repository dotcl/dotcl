;;; COMPILE-FILE of a DEFMACRO whose body names the macro in a list tail, here
;;; (RETURN-FROM ,RX-M ...) inside a MAPCAR, makes the macro's expander a few
;;; times, not once per nesting until the stack runs out.

(defvar *dmer-made* 0)

(deftest-compiled-only deferred-macro-expander-reentry
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "dmer.lisp" dir))
         (fasl (merge-pathnames "dmer.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defmacro dmer-m (&rest xs)
  (load-time-value (incf cl-user::*dmer-made*))
  (let ((dmer-m (gensym)))
    `(block ,dmer-m ,@(mapcar (lambda (x) `(return-from ,dmer-m ,x)) xs))))
(defun dmer-use () (dmer-m 1 2))" s))
    (setq *dmer-made* 0)
    (let ((*error-output* (make-broadcast-stream)))
      (load (compile-file src :output-file fasl)))
    (list (< *dmer-made* 10) (funcall (intern "DMER-USE"))))
  (t 1))
