;;; A name bound by a SYMBOL-MACROLET around a top level DEFUN is not reported as an
;;; undefined variable, while a name nothing binds still is.

(defun %uvs-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir))
         (undefined '()))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (handler-bind ((warning
                     (lambda (c)
                       (let ((text (princ-to-string c)))
                         (when (search "undefined variable" text)
                           (push text undefined))
                         (muffle-warning c)))))
      (let ((*error-output* (make-broadcast-stream)))
        (compile-file src :output-file fasl)))
    (nreverse undefined)))

(deftest-compiled-only undefined-variable-symbol-macrolet.around-defun
  (%uvs-compile "uvs1" '("(symbol-macrolet ((uvs-x 1)) (defun uvs-f () uvs-x))"
                         "(symbol-macrolet ((uvs-z 3)) (defun uvs-h (a) (+ a uvs-z)))"))
  nil)

(deftest-compiled-only undefined-variable-symbol-macrolet.inside-defun
  (%uvs-compile "uvs2" '("(defun uvs-g () (symbol-macrolet ((uvs-y 2)) (list uvs-y)))"))
  nil)

(deftest-compiled-only undefined-variable-symbol-macrolet.other-name-still-warns
  (length (%uvs-compile "uvs3" '("(symbol-macrolet ((uvs-a 1)) (defun uvs-k () uvs-b))")))
  1)
