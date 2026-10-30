;;; DESTRUCTURING-BIND rejects keyword arguments its lambda list does not name.

(defun %dbuk (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (list :value (eval form))
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

(deftest destructuring-bind-unknown-key.direct
  (handler-case (progn (destructuring-bind (&key a b) '(:bogus 1) (list a b)) :no-error)
    (program-error () :program-error))
  :program-error)

(deftest destructuring-bind-unknown-key.unknown-compiled
  (%dbuk :compile '(destructuring-bind (&key a b) '(:bogus 1) (list a b)))
  :program-error)

(deftest destructuring-bind-unknown-key.unknown-interpreted
  (%dbuk :interpret '(destructuring-bind (&key a b) '(:bogus 1) (list a b)))
  :program-error)

(deftest destructuring-bind-unknown-key.odd-number
  (%dbuk :compile '(destructuring-bind (&key a b) '(:a 1 :b) (list a b)))
  :program-error)

(deftest destructuring-bind-unknown-key.unknown-before-defaults
  (%dbuk :compile '(destructuring-bind (&key (a (error "default ran"))) '(:bogus 1) a))
  :program-error)

(deftest destructuring-bind-unknown-key.allow-other-keys-in-lambda-list
  (%dbuk :compile '(destructuring-bind (&key a b &allow-other-keys) '(:bogus 1) (list a b)))
  (:value (nil nil)))

(deftest destructuring-bind-unknown-key.allow-other-keys-argument-t
  (%dbuk :compile '(destructuring-bind (&key a b) '(:bogus 1 :allow-other-keys t) (list a b)))
  (:value (nil nil)))

(deftest destructuring-bind-unknown-key.allow-other-keys-argument-nil
  (%dbuk :compile '(destructuring-bind (&key a b) '(:bogus 1 :allow-other-keys nil) (list a b)))
  :program-error)

(deftest destructuring-bind-unknown-key.allow-other-keys-leftmost-nil
  (%dbuk :compile '(destructuring-bind (&key a) '(:bogus 1 :allow-other-keys nil :allow-other-keys t) a))
  :program-error)

(deftest destructuring-bind-unknown-key.allow-other-keys-leftmost-t
  (%dbuk :compile '(destructuring-bind (&key a) '(:bogus 1 :allow-other-keys t :allow-other-keys nil) a))
  (:value nil))

(deftest destructuring-bind-unknown-key.nested
  (%dbuk :compile '(destructuring-bind ((&key a)) '((:bogus 1)) a))
  :program-error)

(deftest destructuring-bind-unknown-key.nested-allow-other-keys
  (%dbuk :compile '(destructuring-bind ((&key a)) '((:bogus 1 :allow-other-keys t)) a))
  (:value nil))

(deftest destructuring-bind-unknown-key.explicit-symbol-key
  (%dbuk :compile '(destructuring-bind (&key ((foo value) 0)) '(foo 7) value))
  (:value 7))

(deftest destructuring-bind-unknown-key.macro-compiled
  (%dbuk :compile '(progn (defmacro dbuk-macro-compiled (&key a b) (list 'quote (list a b)))
                          (dbuk-macro-compiled :bogus 1)))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-interpreted
  (%dbuk :interpret '(progn (defmacro dbuk-macro-interpreted (&key a b) (list 'quote (list a b)))
                            (dbuk-macro-interpreted :bogus 1)))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-allow-other-keys
  (%dbuk :compile '(progn (defmacro dbuk-macro-aok (&key a &allow-other-keys) (list 'quote a))
                          (dbuk-macro-aok :bogus 1)))
  (:value nil))

;;; ---- a non-symbol key is not an error when keyword checking is suppressed ----
;;
;;; CLHS 3.5.1.5: the non-symbol check applies unless checking is suppressed as
;;; described in 3.4.1.4.1, which &allow-other-keys in the lambda list or a
;;; true :allow-other-keys argument does. The list still has to have even
;;; length (CLHS 3.5.1.6, as SBCL does).

(deftest destructuring-bind-unknown-key.non-symbol-key-allow-other-keys
  (%dbuk :compile '(destructuring-bind (&key a &allow-other-keys) '(1 2) a))
  (:value nil))

(deftest destructuring-bind-unknown-key.non-symbol-key-allow-other-keys-interpreted
  (%dbuk :interpret '(destructuring-bind (&key a &allow-other-keys) '(1 2) a))
  (:value nil))

(deftest destructuring-bind-unknown-key.non-symbol-key-default-and-supplied-p
  (%dbuk :compile '(destructuring-bind (&key (a 10 a-p) &allow-other-keys) '(1 2)
                      (list a a-p)))
  (:value (10 nil)))

(deftest destructuring-bind-unknown-key.non-keyword-non-symbol-key
  (%dbuk :compile '(destructuring-bind (&key a &allow-other-keys) '((1) 3) a))
  (:value nil))

(deftest destructuring-bind-unknown-key.non-symbol-key-allow-other-keys-argument
  (%dbuk :compile '(destructuring-bind (&key a) '(1 2 :allow-other-keys t) a))
  (:value nil))

(deftest destructuring-bind-unknown-key.non-symbol-key-still-errors
  (%dbuk :compile '(destructuring-bind (&key a) '(1 2) a))
  :program-error)

(deftest destructuring-bind-unknown-key.non-symbol-key-nil-aok-still-errors
  (%dbuk :compile '(destructuring-bind (&key a) '(1 2 :allow-other-keys nil) a))
  :program-error)

(deftest destructuring-bind-unknown-key.odd-number-with-allow-other-keys
  (%dbuk :compile '(destructuring-bind (&key a &allow-other-keys) '(:a 1 :b) a))
  :program-error)

;;; --- the same checks under the tree-walk interpreter, and in macro lambda lists

(deftest destructuring-bind-unknown-key.odd-number-interpreted
  (%dbuk :interpret '(destructuring-bind (&key a b) '(:a 1 :b) (list a b)))
  :program-error)

(deftest destructuring-bind-unknown-key.unknown-interpreted-before-defaults
  (%dbuk :interpret '(destructuring-bind (&key (a (error "default ran"))) '(:bogus 1) a))
  :program-error)

(deftest destructuring-bind-unknown-key.direct-odd-number
  (handler-case (progn (destructuring-bind (&key a) '(:a) a) :no-error)
    (program-error () :program-error))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-odd-number-compiled
  (%dbuk :compile '(progn (defmacro dbuk-odd-macro (&key a b) (list 'quote (list a b)))
                          (dbuk-odd-macro :a 1 :b)))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-odd-number-interpreted
  (%dbuk :interpret '(progn (defmacro dbuk-odd-macro-i (&key a b) (list 'quote (list a b)))
                            (dbuk-odd-macro-i :a 1 :b)))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-odd-number-allow-other-keys
  (%dbuk :compile '(progn (defmacro dbuk-odd-aok (&key a &allow-other-keys)
                            (list 'quote a))
                          (dbuk-odd-aok :a)))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-allow-other-keys-argument-t
  (%dbuk :compile '(progn (defmacro dbuk-aok-t (&key a) (list 'quote a))
                          (dbuk-aok-t :bogus 1 :allow-other-keys t)))
  (:value nil))

(deftest destructuring-bind-unknown-key.macro-allow-other-keys-argument-nil
  (%dbuk :compile '(progn (defmacro dbuk-aok-nil (&key a) (list 'quote a))
                          (dbuk-aok-nil :bogus 1 :allow-other-keys nil)))
  :program-error)

(deftest destructuring-bind-unknown-key.macro-nested-unknown
  (%dbuk :compile '(progn (defmacro dbuk-nested-macro ((&key a)) (list 'quote a))
                          (dbuk-nested-macro (:bogus 1))))
  :program-error)
