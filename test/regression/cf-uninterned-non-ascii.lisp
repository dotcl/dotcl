;;; A fasl keeps its uninterned symbols in a table whose entries are
;;; length-prefixed by UTF-8 byte count. A name with a non-ASCII character used
;;; to throw off every entry from it on, and the fasl failed to load (the
;;; type initializer threw).

(defun %cuna-compile-and-load ()
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "cuna.lisp" dir))
         (fasl (merge-pathnames "cuna.fasl" dir))
         (a (make-symbol (coerce (list (code-char 196) #\B #\C) 'string)))
         (b (make-symbol "ZZ"))
         (c (make-symbol (coerce (list (code-char 233) (code-char 12354) #\X) 'string))))
    (with-open-file (s src :direction :output :if-exists :supersede
                           :external-format :utf-8)
      (let ((*package* (find-package :cl-user))
            (*print-circle* t))
        (prin1 `(defun cuna-syms () '(,a ,b ,c ,a)) s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl :external-format :utf-8))
    (load fasl)
    (funcall (intern "CUNA-SYMS"))))

(deftest-compiled-only cuna.names
  (let ((syms (%cuna-compile-and-load)))
    (list (mapcar (lambda (s) (map 'list #'char-code (symbol-name s))) syms)
          (eq (first syms) (fourth syms))
          (symbol-package (second syms))))
  (((196 66 67) (90 90) (233 12354 88) (196 66 67)) t nil))
