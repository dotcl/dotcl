;;; Regression tests for INLINE substitution and MACROLET / SYMBOL-MACROLET
;;; scopes.
;;;
;;; Two things used to go wrong:
;;;
;;; 1. Any active MACROLET at the call site refused the substitution outright,
;;;    even when it bound names the inline body never uses. Libraries commonly
;;;    wrap a whole group of DEFUNs in one top-level MACROLET, so every call to
;;;    a small inline helper from inside such a group stayed a full call.
;;;
;;; 2. A DEFUN proclaimed inline inside a MACROLET had its body recorded even
;;;    when that body used the local macro. Substituted at a call site outside
;;;    the MACROLET, the local macro compiled as a call to an undefined
;;;    function.
;;;
;;; The guard is now per-name at both ends: a body is not recorded when it uses
;;; a name bound by an enclosing MACROLET / SYMBOL-MACROLET / local function,
;;; and it is not substituted where the call site binds one of its free names.

(setf dotcl:*save-sil* t)

(defun %ims-calls-p (fn name)
  (and (search name (princ-to-string (dotcl:function-sil fn))) t))

(declaim (inline %ims-low-byte))
(defun %ims-low-byte (x) (ldb (byte 8 0) x))

;; A top-level MACROLET that binds unrelated names does not block the
;; substitution of a helper defined outside it.
(macrolet ((%ims-twice (form) `(* 2 ,form)))
  (defun %ims-use-in-macrolet (x) (%ims-twice (%ims-low-byte x))))

(deftest-compiled-only ims-unrelated-macrolet-substitutes
  (%ims-calls-p #'%ims-use-in-macrolet "%IMS-LOW-BYTE")
  nil)

(deftest ims-unrelated-macrolet-value
  (%ims-use-in-macrolet #x1234)
  #x68)

;; A MACROLET that binds a name the body DOES use still keeps the body's
;; global meaning.
(defun %ims-helper () :global)
(declaim (inline %ims-calls-helper))
(defun %ims-calls-helper () (%ims-helper))

(defun %ims-macrolet-shadow ()
  (macrolet ((%ims-helper () :macro))
    (list (%ims-helper) (%ims-calls-helper))))

(deftest ims-macrolet-does-not-capture
  (%ims-macrolet-shadow)
  (:macro :global))

;; Same for SYMBOL-MACROLET over a global symbol macro the body reads.
(define-symbol-macro %ims-gsm :global-sm)
(declaim (inline %ims-reads-gsm))
(defun %ims-reads-gsm () %ims-gsm)

(defun %ims-symbol-macrolet-shadow ()
  (symbol-macrolet ((%ims-gsm :local-sm))
    (list %ims-gsm (%ims-reads-gsm))))

(deftest ims-symbol-macrolet-does-not-capture
  (%ims-symbol-macrolet-shadow)
  (:local-sm :global-sm))

;; An inline DEFUN whose body uses a local macro of the MACROLET it is defined
;; in, called from outside that MACROLET: must call the real function, not
;; substitute a body whose macro no longer exists.
(macrolet ((%ims-ref (v i) `(aref ,v (+ 1 ,i))))
  (declaim (inline %ims-in-macrolet))
  (defun %ims-in-macrolet (v i) (%ims-ref v i))
  (defun %ims-call-inside (v) (%ims-in-macrolet v 0)))

(defun %ims-call-outside (v) (%ims-in-macrolet v 0))

(deftest ims-defined-in-macrolet-called-inside
  (%ims-call-inside #(10 20 30))
  20)

(deftest ims-defined-in-macrolet-called-outside
  (%ims-call-outside #(10 20 30))
  20)

;; An inline DEFUN inside a MACROLET whose body does not use the local macros
;; is still recorded and substituted outside.
(macrolet ((%ims-unused () nil))
  (declaim (inline %ims-plain-in-macrolet))
  (defun %ims-plain-in-macrolet (x) (+ x 3)))

(defun %ims-call-plain (x) (%ims-plain-in-macrolet x))

(deftest-compiled-only ims-plain-in-macrolet-substitutes
  (%ims-calls-p #'%ims-call-plain "%IMS-PLAIN-IN-MACROLET")
  nil)

(deftest ims-plain-in-macrolet-value
  (%ims-call-plain 4)
  7)
