;;; Regression: CLHS 3.2.2.1.1 — a NOTINLINE declaration of a function name
;;; suppresses its compiler macro for calls in the declaration's scope. A
;;; PROCLAIM (or the DECLAIM that expands to one) puts the declaration in scope
;;; everywhere, so it has to suppress too.
;;;
;;; Only the lexical (declare (notinline f)) was honored. DECLAIM dropped every
;;; spec it did not itself understand — it handled SPECIAL and FTYPE and threw
;;; the rest away — and PROCLAIM only ever looked at SPECIAL, so a global
;;; NOTINLINE was silently ignored and the compiler macro kept firing. The
;;; symptom is quietly different code, not an error.
;;;
;;; Compiler macros only fire while compiling, so every test that asserts one
;;; fired — or that a declaration stopped it firing — is compiled-only. Without a
;;; compiler they all answer :FUNCTION, which makes half of them pass vacuously.

(defun gni-f (x) (list :function x))
(define-compiler-macro gni-f (x) `(list :compiler-macro ,x))

;;; Baseline: the compiler macro applies where nothing suppresses it.
(defun gni-plain () (gni-f 1))
(deftest-compiled-only global-notinline-baseline (gni-plain) (:compiler-macro 1))

;;; The lexical declaration (this always worked).
(defun gni-lexical () (declare (notinline gni-f)) (gni-f 2))
(deftest-compiled-only global-notinline-lexical (gni-lexical) (:function 2))

;;; DECLAIM. The effect has to reach the compilation of the forms that follow
;;; it, not only the run time of the file being loaded.
(declaim (notinline gni-f))
(defun gni-after-declaim () (gni-f 3))
(deftest-compiled-only global-notinline-declaim (gni-after-declaim) (:function 3))

;;; INLINE is the way back off.
(declaim (inline gni-f))
(defun gni-after-inline () (gni-f 4))
(deftest-compiled-only global-notinline-inline-restores (gni-after-inline) (:compiler-macro 4))

;;; PROCLAIM directly, which is what DECLAIM expands to.
(proclaim '(notinline gni-f))
(defun gni-after-proclaim () (gni-f 5))
(deftest-compiled-only global-notinline-proclaim (gni-after-proclaim) (:function 5))

;;; A function with no compiler macro is unaffected either way.
(defun gni-plain-fn (x) (* x 10))
(proclaim '(notinline gni-plain-fn))
(deftest global-notinline-no-compiler-macro (gni-plain-fn 4) 40)

;;; Leave the flag off so a later test file sees the default.
(proclaim '(inline gni-f))
