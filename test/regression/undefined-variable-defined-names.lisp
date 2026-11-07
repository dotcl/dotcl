;;; The undefined-variable warning leaves out a name the file defines in a way other
;;; than DEFVAR before the use: DEFINE-SYMBOL-MACRO, and the DEFVAR / DEFPARAMETER /
;;; DEFCONSTANT form for its own name (also at read time). A name nothing defines
;;; still gets the warning.

(defun %uvd-compile (name lines)
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

(deftest-compiled-only undefined-variable-defined.symbol-macro
  (%uvd-compile "uvd1" '("(eval-when (:compile-toplevel :load-toplevel :execute)"
                         "  (define-symbol-macro uvd-sm-1 42))"
                         "(defun uvd-f-1 (y) (< y uvd-sm-1))"))
  nil)

(deftest-compiled-only undefined-variable-defined.defparameter-and-defconstant
  (%uvd-compile "uvd2" '("(defparameter *uvd-p-2* '(a b))"
                         "(defconstant +uvd-c-2+ 5)"
                         "(defun uvd-f-2 () (list *uvd-p-2* +uvd-c-2+))"))
  nil)

(deftest-compiled-only undefined-variable-defined.read-time-defvar
  (%uvd-compile "uvd3" '("#.(progn (defvar *uvd-r-3* '(a b)) nil)"
                         "(defun uvd-f-3 () *uvd-r-3*)"))
  nil)

(deftest-compiled-only undefined-variable-defined.typo-still-warns
  (length (%uvd-compile "uvd4" '("(defun uvd-f-4 () *uvd-no-such-variable-4*)")))
  1)
