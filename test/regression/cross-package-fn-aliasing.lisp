;;; Cross-package function aliasing regression tests.
;;;
;;; Bug: GetFunctionBySymbol fell back to FindFunctionAcrossPackages when
;;; sym.Function was null, resolving a package-qualified call to an
;;; unbound symbol (lib:foo) to a same-named bound function in ANY other
;;; package (usr:foo). When usr:foo's body called lib:foo, the call
;;; resolved back to usr:foo -> infinite recursion (stack overflow).
;;; SBCL signals UNDEFINED-FUNCTION; DotCL used to signal STACK-OVERFLOW.
;;;
;;; Fix: GetFunctionBySymbol is authoritative for package-qualified
;;; compiled calls — returns sym.Function or signals UNDEFINED-FUNCTION,
;;; with NO cross-package bridge. The compiler's compile-fn-sym-lookup
;;; emits :load-sym-pkg for package-qualified calls (authoritative) and
;;; :load-sym (bare Startup.Sym, which bridges at symbol-resolution) for
;;; unqualified calls, so the bridge is preserved only for the legitimate
;;; unqualified-call case (e.g. class-precedence-list read in CL-USER,
;;; registered in DOTCL-MOP).

(defpackage #:xpa-lib (:use #:cl) (:export #:foo))
(defpackage #:xpa-usr (:use #:cl) (:export #:foo))
(defpackage #:xpa-a (:use #:cl) (:export #:bar))
(defpackage #:xpa-b (:use #:cl) (:export #:bar))

;;; A compiled package-qualified call to an unbound symbol must signal
;;; UNDEFINED-FUNCTION (matching SBCL), not alias to a same-named bound
;;; function in another package and recurse.
;;;
;;; xpa-usr:foo is bound and its body calls xpa-lib:foo (unbound). A
;;; compiled thunk calling xpa-usr:foo must signal UNDEFINED-FUNCTION
;;; for xpa-lib:foo, not stack-overflow.
(deftest-compiled-only cross-package-fn-aliasing.unbound-qualified-call-signals-undefined
  (progn
    (fmakunbound 'xpa-lib:foo)
    (fmakunbound 'xpa-usr:foo)
    (defun xpa-usr:foo () (xpa-lib:foo))
    (let ((fn (compile nil '(lambda () (xpa-usr:foo)))))
      (list (fboundp 'xpa-lib:foo)
            (fboundp 'xpa-usr:foo)
            (signals-error (funcall fn) undefined-function))))
  (nil t t))

;;; When the target symbol is bound, the package-qualified call resolves
;;; to it directly — no aliasing, no undefined-function.
(deftest-compiled-only cross-package-fn-aliasing.bound-qualified-call-resolves
  (progn
    (fmakunbound 'xpa-lib:foo)
    (fmakunbound 'xpa-usr:foo)
    (defun xpa-lib:foo () :lib)
    (defun xpa-usr:foo () (list :usr (xpa-lib:foo)))
    (let ((fn (compile nil '(lambda () (xpa-usr:foo)))))
      (list (fboundp 'xpa-lib:foo)
            (funcall fn))))
  (t (:usr :lib)))

;;; A package-qualified call to a genuinely unbound symbol (no same-named
;;; bound function anywhere) must signal UNDEFINED-FUNCTION, not be
;;; aliased to some unrelated same-named function in another package.
;;; This is the core "no cross-package alias" guarantee.
(deftest-compiled-only cross-package-fn-aliasing.no-cross-package-alias
  (progn
    ;; xpa-a:bar is bound; xpa-b:bar is NOT bound. A call to xpa-b:bar
    ;; must NOT resolve to xpa-a:bar via the bridge.
    (fmakunbound 'xpa-a:bar)
    (fmakunbound 'xpa-b:bar)
    (defun xpa-a:bar () :a)
    (let ((fn (compile nil '(lambda () (xpa-b:bar)))))
      (signals-error (funcall fn) undefined-function)))
  t)

;;; fe63591 data-symbol identity split: a defclass :accessor named hash-value in a
;;; non-CL package makes that package's HASH-VALUE fbound. loop.lisp's
;;; (check-type which (member hash-key hash-value)) must keep resolving its
;;; hash-value to the stable DOTCL-INTERNAL symbol (data position), NOT bridge to
;;; the foreign fbound accessor. Pre-fix: TYPE-ERROR (datum
;;; DOTCL-INTERNAL::HASH-VALUE, expected (MEMBER DOTCL-INTERNAL::HASH-KEY
;;; QL-CDB::HASH-VALUE)). Post-fix: loop iterates normally.
(defpackage #:xpa-ht (:use #:cl) (:export #:hash-value #:bridge-target #:record-pointer))
(deftest-compiled-only cross-package-fn-aliasing.data-symbol-not-stolen-by-fbound-accessor
  (progn
    (fmakunbound 'xpa-ht:hash-value)
    (defclass xpa-ht:record-pointer ()
      ((hash-value :initarg :hash-value :accessor xpa-ht:hash-value)))
    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1)
      (setf (gethash 'b ht) 2)
      (sort (loop for v being the hash-values of ht collect v) #'<)))
  (1 2))

;;; Interning PKG::NAME must never graft another package's function onto the
;;; fresh symbol. Startup.SymInPkg used to copy the Function slot from any
;;; same-named symbol on first intern ("copy-on-intern" bridge), so loading a
;;; .fasl whose package names a symbol that happens to exist elsewhere (e.g.
;;; asdf/footer::emptyp) made the fresh symbol fbound to the foreign function:
;;; DEFGENERIC then warned "being redefined as a generic function, but it was
;;; previously defined as an ordinary function", and any funcall of that symbol
;;; before its own definition ran would have reached the foreign one.
(defun %xpa-fasl-collide-case ()
  (let* ((tmp (format nil "~a/dotcl-xpafasl-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp))
         (warned nil))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defpackage #:xpafasl-pkg (:use :cl))~%")
      (format s "(in-package #:xpafasl-pkg)~%")
      (format s "(defclass xpafasl-box () ((n :initarg :n :accessor xpafasl-n)))~%")
      (format s "(defgeneric xpafasl-collide (b)~%")
      (format s "  (:method ((b xpafasl-box)) (zerop (xpafasl-n b))))~%")
      ;; A call site is what dragged the symbol through SymInPkg first.
      (format s "(defun xpafasl-user (b) (unless (xpafasl-collide b) :go))~%"))
    (compile-file src)
    ;; compile-file interned the names in XPAFASL-PKG; drop the package so the
    ;; load re-interns them fresh, as it would in a separate session.
    (delete-package (find-package "XPAFASL-PKG"))
    ;; Same-named ordinary function in an unrelated package, bound BEFORE the load.
    (eval (read-from-string "(defpackage #:xpafasl-other (:use :cl))"))
    (eval (read-from-string "(defun xpafasl-other::xpafasl-collide (x) (declare (ignore x)) :other)"))
    (handler-bind ((warning (lambda (c) (setf warned t) (muffle-warning c))))
      (load (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
    (let ((own (find-symbol "XPAFASL-COLLIDE" "XPAFASL-PKG"))
          (other (find-symbol "XPAFASL-COLLIDE" "XPAFASL-OTHER")))
      (list warned
            (typep (fdefinition own) 'generic-function)
            ;; the foreign definition is untouched and still its own function
            (funcall other nil)
            (funcall own (make-instance (find-symbol "XPAFASL-BOX" "XPAFASL-PKG") :n 0))))))

(deftest-compiled-only cross-package-fn-aliasing.fasl-intern-does-not-graft-foreign-function
  (%xpa-fasl-collide-case)
  (nil t :other t))

;;; The unqualified-call bridge exists for one reason: the C# runtime registers
;;; its helpers on CL / DOTCL-INTERNAL symbols, so Lisp code that reads such a
;;; name in another package produces a different symbol object that still has to
;;; reach the registered function. It bridges only from dotcl's own packages —
;;; an arbitrary library's package answering would turn an undefined function
;;; into a silent call of an unrelated same-named one.
(deftest-compiled-only cross-package-fn-aliasing.unqualified-call-bridges-to-dotcl-packages
  (let ((*package* (find-package :cl-user)))
    ;; CLASS-PRECEDENCE-LIST is registered in DOTCL-INTERNAL, not CL.
    (let ((cpl (funcall (compile nil '(lambda ()
                                        (class-precedence-list (find-class 'symbol)))))))
      (and (consp cpl) (member (find-class 't) cpl) t)))
  t)

;;; Same rule on the (funcall 'sym) path, and the hit is never cached on the
;;; caller's symbol — a failed funcall must not leave the symbol FBOUNDP.
(deftest cross-package-fn-aliasing.funcall-does-not-bridge-to-user-packages
  (progn
    (fmakunbound 'xpa-a:bar)
    (fmakunbound 'xpa-b:bar)
    (defun xpa-a:bar () :a)
    (list (signals-error (funcall 'xpa-b:bar) undefined-function)
          (fboundp 'xpa-b:bar)))
  (t nil))

(deftest-compiled-only cross-package-fn-aliasing.unqualified-call-does-not-bridge-to-user-packages
  (progn
    (fmakunbound 'xpa-ht:bridge-target)
    (defun xpa-ht:bridge-target () :bridged)
    (let ((*package* (find-package :cl-user)))
      (let ((fn (compile nil '(lambda () (bridge-target)))))
        (list (funcall 'xpa-ht:bridge-target)
              (signals-error (funcall fn) undefined-function)))))
  (:bridged t))
