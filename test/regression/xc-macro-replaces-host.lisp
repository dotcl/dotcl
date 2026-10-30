;;; While cross-compiling, a tree macro/compiler-macro/setf definition must replace
;;; the host's definition of the same name, or later tree code uses the old one.

(defun xcdr-sym (name)
  (let ((a (find-symbol name "DOTCL.CIL-COMPILER"))
        (b (find-symbol name "DOTCL-INTERNAL")))
    (cond ((and a (or (fboundp a) (boundp a))) a)
          ((and b (or (fboundp b) (boundp b))) b)
          (t (or a b)))))

(defun xcdr-cross-compile (form)
  (let ((flag (find-symbol "*CROSS-COMPILING*" "DOTCL-INTERNAL"))
        (compile-toplevel (xcdr-sym "COMPILE-TOPLEVEL")))
    (progv (list flag) '(t)
      (prin1-to-string (funcall compile-toplevel form)))))

(defmacro xcdr-host-macro (x) `(list :host ,x))

(deftest xc-macro-replaces-host.defmacro
  (let* ((macros (symbol-value (xcdr-sym "*MACROS*")))
         (name 'xcdr-host-macro)
         (old (gethash name macros)))
    (unwind-protect
         (let ((out (xcdr-cross-compile
                     '(progn
                        (defmacro xcdr-host-macro (x) (list 'list :tree x))
                        (defun xcdr-macro-probe (x) (xcdr-host-macro x))))))
           (and (search "\"TREE\"" out) (not (search "\"HOST\"" out))))
      (setf (gethash name macros) old)))
  t)

(deftest xc-macro-replaces-host.defmacro-normal-mode-keeps-old
  (let* ((macros (symbol-value (xcdr-sym "*MACROS*")))
         (name 'xcdr-host-macro)
         (old (gethash name macros))
         (compile-toplevel (xcdr-sym "COMPILE-TOPLEVEL")))
    (funcall compile-toplevel
             '(defmacro xcdr-host-macro (x) (list 'list :tree x)))
    (eq old (gethash name macros)))
  t)

(define-compiler-macro xcdr-host-compiler-macro (x) `(list :host ,x))

(deftest xc-macro-replaces-host.compiler-macro
  (let ((old (compiler-macro-function 'xcdr-host-compiler-macro)))
    (unwind-protect
         (let ((out (xcdr-cross-compile
                     '(progn
                        (define-compiler-macro xcdr-host-compiler-macro (x)
                          (list 'list :tree x))
                        (defun xcdr-compiler-macro-probe (x)
                          (xcdr-host-compiler-macro x))))))
           (and (search "\"TREE\"" out) (not (search "\"HOST\"" out))))
      (setf (compiler-macro-function 'xcdr-host-compiler-macro) old)))
  t)

(defun xcdr-setf-place (x) (declare (ignore x)) nil)
(defsetf xcdr-setf-place (x) (v) `(list :host ,x ,v))

(deftest xc-macro-replaces-host.defsetf
  (let* ((expanders (symbol-value (xcdr-sym "*SETF-EXPANDERS*")))
         (counts (symbol-value (xcdr-sym "*SETF-EXPANDER-STORE-COUNTS*")))
         (key (funcall (xcdr-sym "%SETF-KEY") 'xcdr-setf-place))
         (old (gethash key expanders))
         (old-count (gethash key counts)))
    (unwind-protect
         (let ((out (xcdr-cross-compile
                     '(progn
                        (defsetf xcdr-setf-place (x) (v)
                          (list 'list :tree x v))
                        (defun xcdr-setf-probe (x v)
                          (setf (xcdr-setf-place x) v))))))
           (and (search "\"TREE\"" out) (not (search "\"HOST\"" out))))
      (setf (gethash key expanders) old)
      (setf (gethash key counts) old-count)))
  t)

(defun xcdr-dse-place (x) (declare (ignore x)) nil)
(define-setf-expander xcdr-dse-place (x)
  `((g) (,x) (s) (list :host g s) (xcdr-dse-place g)))

(deftest xc-macro-replaces-host.define-setf-expander
  (let* ((expanders (symbol-value (xcdr-sym "*SETF-EXPANSION-FNS*")))
         (key (funcall (xcdr-sym "%SETF-KEY") 'xcdr-dse-place))
         (old (gethash key expanders)))
    (unwind-protect
         (let ((out (xcdr-cross-compile
                     '(progn
                        (define-setf-expander xcdr-dse-place (x)
                          `((g) (,x) (s) (list :tree g s) (xcdr-dse-place g)))
                        (defun xcdr-dse-probe (x v)
                          (setf (xcdr-dse-place x) v))))))
           (and (search "\"TREE\"" out) (not (search "\"HOST\"" out))))
      (setf (gethash key expanders) old)))
  t)
