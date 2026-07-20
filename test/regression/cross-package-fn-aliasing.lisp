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
(deftest cross-package-fn-aliasing.unbound-qualified-call-signals-undefined
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
(deftest cross-package-fn-aliasing.bound-qualified-call-resolves
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
(deftest cross-package-fn-aliasing.no-cross-package-alias
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
(deftest cross-package-fn-aliasing.data-symbol-not-stolen-by-fbound-accessor
  (progn
    (fmakunbound 'xpa-ht:hash-value)
    (defclass xpa-ht:record-pointer ()
      ((hash-value :initarg :hash-value :accessor xpa-ht:hash-value)))
    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1)
      (setf (gethash 'b ht) 2)
      (sort (loop for v being the hash-values of ht collect v) #'<)))
  (1 2))

(deftest cross-package-fn-aliasing.unqualified-call-still-bridges
  (progn
    (fmakunbound 'xpa-ht:bridge-target)
    (defun xpa-ht:bridge-target () :bridged)
    (let ((*package* (find-package :cl-user)))
      (funcall (compile nil '(lambda () (bridge-target))))))
  :bridged)
