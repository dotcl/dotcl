;;; Regression tests for multi-package lexical variable name collision.
;;;
;;; The compiler's variable lookup and free-var detection used var-name
;;; (bare symbol-name) string matching as a fallback when eq identity
;;; matching failed. This fallback is necessary for closure env-locals
;;; (interned in DOTCL.CIL-COMPILER) and uninterned gensyms, but it also
;;; matched across user packages: two packages interning a symbol with
;;; the same printed name collided, causing a reference to one to resolve
;;; to the other's binding.

(defpackage :lexical-fix-pkg-a (:use))
(defpackage :lexical-fix-pkg-b (:use))

(defvar *lexical-fix-sym-a* (intern "COLLIDE" :lexical-fix-pkg-a))
(defvar *lexical-fix-sym-b* (intern "COLLIDE" :lexical-fix-pkg-b))

;; A lambda uses one symbol as &aux, a nested lambda's let binds the other
;; (same printed name, different package). The nested lambda references the
;; &aux var — it must capture the &aux value, not the let value. Before the
;; fix, lookup-local matched cross-package and returned the wrong binding.
(deftest multipackage-lexical.closure-captures-aux-not-let
  (let ((arg (gensym "ARG"))
        (dummy (gensym "DUMMY")))
    (let ((form
            `(lambda (,arg &aux (,*lexical-fix-sym-a* ,arg))
               (let ((length (length ,*lexical-fix-sym-a*)))
                 (funcall
                  (lambda (,dummy)
                    (let ((,*lexical-fix-sym-b* length))
                      (subseq ,*lexical-fix-sym-a* 0 1)))
                  0)))))
      (let ((fn (compile nil form)))
        (equalp (funcall fn #(1 2 3 4 5)) #(1)))))
  t)

;; Same structure, but the reference is read directly (not via subseq) — the
;; closure must return the &aux value (the array), not the let value (the
;; length, a fixnum).
(deftest multipackage-lexical.closure-returns-aux-value
  (let ((arg (gensym "ARG"))
        (dummy (gensym "DUMMY")))
    (let ((form
            `(lambda (,arg &aux (,*lexical-fix-sym-a* ,arg))
               (let ((length (length ,*lexical-fix-sym-a*)))
                 (funcall
                  (lambda (,dummy)
                    (let ((,*lexical-fix-sym-b* length))
                      ,*lexical-fix-sym-a*))
                  0)))))
      (let ((fn (compile nil form)))
        (equalp (funcall fn #(1 2 3 4 5)) #(1 2 3 4 5)))))
  t)

;; Without a nested closure: a direct reference to the &aux var inside the
;; let body must still read the &aux binding (same package, eq match).
(deftest multipackage-lexical.direct-ref-reads-aux
  (let ((arg (gensym "ARG")))
    (let ((form
            `(lambda (,arg &aux (,*lexical-fix-sym-a* ,arg))
               (let ((,*lexical-fix-sym-b* (length ,*lexical-fix-sym-a*)))
                 ,*lexical-fix-sym-a*))))
      (let ((fn (compile nil form)))
        (equalp (funcall fn #(1 2 3 4 5)) #(1 2 3 4 5)))))
  t)

;; The let-bound same-name var in a different package must still be readable
;; by its own symbol within the let body.
(deftest multipackage-lexical.let-binding-readable-by-own-symbol
  (let ((arg (gensym "ARG")))
    (let ((form
            `(lambda (,arg &aux (,*lexical-fix-sym-a* ,arg))
               (let ((,*lexical-fix-sym-b* (length ,*lexical-fix-sym-a*)))
                 ,*lexical-fix-sym-b*))))
      (let ((fn (compile nil form)))
        (funcall fn #(1 2 3 4 5)))))
  5)
