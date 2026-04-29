;;; destructuring-bind.lisp — ANSI tests for destructuring-bind

;;; ============================================================
;;; Basic required parameters
;;; ============================================================

(deftest db.basic.1
  (destructuring-bind (a b c) '(1 2 3) (list a b c))
  (1 2 3))

(deftest db.basic.2
  (destructuring-bind (a) '(42) a)
  42)

(deftest db.basic.single-body
  (destructuring-bind (a b) '(10 20) (+ a b))
  30)

;;; ============================================================
;;; Nested patterns
;;; ============================================================

(deftest db.nested.1
  (destructuring-bind ((a b) c) '((1 2) 3) (list a b c))
  (1 2 3))

(deftest db.nested.2
  (destructuring-bind (a (b c)) '(1 (2 3)) (list a b c))
  (1 2 3))

(deftest db.nested.deep
  (destructuring-bind ((a (b c)) d) '((1 (2 3)) 4) (list a b c d))
  (1 2 3 4))

;;; ============================================================
;;; &rest / &body
;;; ============================================================

(deftest db.rest.1
  (destructuring-bind (a &rest b) '(1 2 3) (list a b))
  (1 (2 3)))

(deftest db.rest.empty
  (destructuring-bind (a &rest b) '(1) (list a b))
  (1 nil))

(deftest db.body.1
  (destructuring-bind (a &body b) '(1 2 3) (list a b))
  (1 (2 3)))

;;; ============================================================
;;; &optional
;;; ============================================================

(deftest db.optional.present
  (destructuring-bind (a &optional b) '(1 2) (list a b))
  (1 2))

(deftest db.optional.absent
  (destructuring-bind (a &optional b) '(1) (list a b))
  (1 nil))

(deftest db.optional.default
  (destructuring-bind (a &optional (b 10)) '(1) (list a b))
  (1 10))

(deftest db.optional.default-override
  (destructuring-bind (a &optional (b 10)) '(1 2) (list a b))
  (1 2))

(deftest db.optional.multiple
  (destructuring-bind (a &optional (b 10) (c 20)) '(1) (list a b c))
  (1 10 20))

;;; ============================================================
;;; &key
;;; ============================================================

(deftest db.key.present
  (destructuring-bind (a &key b) '(1 :b 2) (list a b))
  (1 2))

(deftest db.key.absent
  (destructuring-bind (a &key b) '(1) (list a b))
  (1 nil))

(deftest db.key.default
  (destructuring-bind (a &key (b 10)) '(1) (list a b))
  (1 10))

(deftest db.key.default-override
  (destructuring-bind (a &key (b 10)) '(1 :b 99) (list a b))
  (1 99))

(deftest db.key.multiple
  (destructuring-bind (&key x y) '(:y 2 :x 1) (list x y))
  (1 2))

;;; ============================================================
;;; &whole
;;; ============================================================

(deftest db.whole.1
  (destructuring-bind (&whole w a b) '(1 2) (list w a b))
  ((1 2) 1 2))

(deftest db.whole.with-rest
  (destructuring-bind (&whole w a &rest b) '(1 2 3) (list w a b))
  ((1 2 3) 1 (2 3)))

;;; ============================================================
;;; Nested with &rest
;;; ============================================================

(deftest db.nested-rest.1
  (destructuring-bind ((a &rest b) c) '((1 2 3) 4) (list a b c))
  (1 (2 3) 4))

;;; ============================================================
;;; Dotted pair patterns: (a b . c) ≡ (a b &rest c)
;;; ============================================================

(deftest db.dotted.1
  (destructuring-bind (a . b) '(1 2 3) (list a b))
  (1 (2 3)))

(deftest db.dotted.2
  (destructuring-bind (a b . c) '(1 2 3 4) (list a b c))
  (1 2 (3 4)))

;;; ============================================================
;;; Multiple body forms
;;; ============================================================

(deftest db.multi-body
  (destructuring-bind (a b) '(1 2)
    (setq a (+ a 10))
    (+ a b))
  13)

;;; ============================================================
;;; Combined features
;;; ============================================================

(deftest db.required-optional-rest
  (destructuring-bind (a &optional (b 0) &rest c) '(1 2 3 4)
    (list a b c))
  (1 2 (3 4)))

(deftest db.required-key-allow
  (destructuring-bind (a &key b &allow-other-keys) '(1 :c 3 :b 2)
    (list a b))
  (1 2))

(do-tests-summary)
