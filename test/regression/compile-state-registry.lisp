;;; Per-compilation state registry (define-compile-state /
;;; call-with-fresh-closure-state) regression tests.
;;;
;;; The registry drives the closure-boundary reset in compile-closure-body:
;;; every registered variable must read its fresh value inside
;;; call-with-fresh-closure-state, overrides must win, and the outer dynamic
;;; values must be restored when the extent ends. A variable declared with
;;; define-compile-state must participate automatically.

;; Runtime symbol placement of compiler internals varies (DOTCL-INTERNAL vs
;; DOTCL.CIL-COMPILER), so locate by name.
(defun %csr-sym (name)
  (let ((a (find-symbol name "DOTCL-INTERNAL"))
        (b (find-symbol name "DOTCL.CIL-COMPILER")))
    (or (and a (or (boundp a) (fboundp a) (macro-function a)) a)
        b a)))

(deftest compile-state.registry-populated
  ;; The per-expression flags and the *CSTATE* pack (which consolidated the
  ;; scope tables, the key-verified tables, and the TCO state into one entry).
  (<= 4 (length (symbol-value (%csr-sym "*CLOSURE-FRESH-STATE*"))))
  t)

(deftest compile-state.fresh-values-inside
  ;; Inside call-with-fresh-closure-state, every registered variable reads
  ;; its registered fresh value.
  (let ((registry (symbol-value (%csr-sym "*CLOSURE-FRESH-STATE*")))
        (cwf (symbol-function (%csr-sym "CALL-WITH-FRESH-CLOSURE-STATE"))))
    (funcall cwf '()
             (lambda ()
               (notnot
                (every (lambda (e) (eql (symbol-value (car e)) (cdr e)))
                       registry)))))
  t)

(deftest compile-state.sentinel-reset-and-restored
  ;; A dirty outer dynamic value is reset inside and restored afterwards.
  (let* ((registry (symbol-value (%csr-sym "*CLOSURE-FRESH-STATE*")))
         (cwf (symbol-function (%csr-sym "CALL-WITH-FRESH-CLOSURE-STATE")))
         (v (car (first registry)))
         (fresh (cdr (first registry)))
         (sentinel (list :sentinel)))
    (progv (list v) (list sentinel)
      (list (eqt (symbol-value v) sentinel)
            (funcall cwf '() (lambda () (eqlt (symbol-value v) fresh)))
            (eqt (symbol-value v) sentinel))))
  (t t t))

(deftest compile-state.override-wins
  ;; An override for a registered variable beats the registered fresh value.
  (let* ((registry (symbol-value (%csr-sym "*CLOSURE-FRESH-STATE*")))
         (cwf (symbol-function (%csr-sym "CALL-WITH-FRESH-CLOSURE-STATE")))
         (v (car (first registry))))
    (funcall cwf (list (cons v :overridden))
             (lambda () (symbol-value v))))
  :overridden)

(defvar *csr-extra* :outer)

(deftest compile-state.override-non-registry
  ;; An override for a variable NOT in the registry is bound too
  ;; (compile-closure-body uses this for computed *notinline-functions*).
  (let ((cwf (symbol-function (%csr-sym "CALL-WITH-FRESH-CLOSURE-STATE"))))
    (list (funcall cwf (list (cons '*csr-extra* :inner))
                   (lambda () *csr-extra*))
          *csr-extra*))
  (:inner :outer))

(deftest compile-state.define-compile-state-auto-participates
  ;; A NEW variable declared via define-compile-state automatically joins the
  ;; closure-boundary reset: fresh inside, outer value restored outside.
  ;; The variable identity is read back from the registry (the compiled defvar
  ;; may intern the symbol in a different package than this test file's reader
  ;; — the registry entry is the single source of truth).
  (let ((dcs (%csr-sym "DEFINE-COMPILE-STATE"))
        (cwf (symbol-function (%csr-sym "CALL-WITH-FRESH-CLOSURE-STATE")))
        (dummy (intern "*CSR-DUMMY-STATE*" "CL-USER")))
    ;; Direct (eval (list dcs ...)) with a cross-package macro head: the compile
    ;; path resolves the macro through MACRO-FUNCTION's name bridge even when the
    ;; *macros* eq-lookup misses (the head symbol found by name may differ from
    ;; the one the macro was registered under). Previously this compiled the head
    ;; as a function call and errored; it now expands correctly.
    (eval (list dcs dummy :fresh-a))
    (let* ((registry (symbol-value (%csr-sym "*CLOSURE-FRESH-STATE*")))
           (dv (car (assoc "*CSR-DUMMY-STATE*" registry
                           :key #'symbol-name :test #'string=))))
      (setf (symbol-value dv) :dirty)
      (list (funcall cwf '() (lambda () (symbol-value dv)))
            (symbol-value dv))))
  (:fresh-a :dirty))

(deftest compile-state.fresh-init-differs-from-init
  ;; define-compile-state :fresh-init registers a fresh value different from
  ;; the defvar init (the *in-tail-position* NIL-init/T-fresh pattern).
  (let ((dcs (%csr-sym "DEFINE-COMPILE-STATE"))
        (cwf (symbol-function (%csr-sym "CALL-WITH-FRESH-CLOSURE-STATE")))
        (dummy2 (intern "*CSR-DUMMY-STATE-2*" "CL-USER")))
    (eval (list dcs dummy2 nil :fresh-init t))
    (let* ((registry (symbol-value (%csr-sym "*CLOSURE-FRESH-STATE*")))
           (dv (car (assoc "*CSR-DUMMY-STATE-2*" registry
                           :key #'symbol-name :test #'string=))))
      (list (symbol-value dv)
            (funcall cwf '() (lambda () (symbol-value dv))))))
  (nil t))

;; Closure compilation still behaves after the registry conversion: nested
;; closures must not inherit the enclosing body's TCO/native context (the
;; behavior the reset exists to guarantee).
(deftest compile-state.nested-closure-isolation
  (let ((outer 1))
    (funcall (funcall (lambda (a) (lambda (b) (+ outer a b))) 10) 100))
  111)
