;;; TYPEP of a constant type with an unknown type name, and of a DEFTYPE redefined or defined after
;;; the TYPEP was compiled. The expected answers are SBCL 2.6.8's (the same on the compiler before
;;; and after the constant-type rewrite): a compound type is tested by its parts, so a part that
;;; decides the answer is not followed by one that would signal; a DEFTYPE known when the TYPEP is
;;; compiled is kept as it was then.

(defmacro %tut-try (form) `(handler-case ,form (error () :error)))

(defun %tut-u1 (x) (typep x 'tut-no-such-type-a))
(defun %tut-u2 (x) (typep x '(or null tut-no-such-type-b)))
(defun %tut-u3 (x) (typep x '(and null tut-no-such-type-c)))
(defun %tut-u4 (x) (typep x '(or tut-no-such-type-d null)))
(defun %tut-u5 (x) (typep x '(and (not null) tut-no-such-type-e)))
(defun %tut-u6 (x) (typep x '(not tut-no-such-type-f)))
(defun %tut-u7 (x) (typep x '(and symbol (member a b) tut-no-such-type-g)))

(deftest typep-unknown-type.single
  (list (%tut-try (%tut-u1 nil)) (%tut-try (%tut-u1 1)))
  (:error :error))

(deftest typep-unknown-type.or-decided-by-first-part
  (list (%tut-try (%tut-u2 nil)) (%tut-try (%tut-u2 1)))
  (t :error))

(deftest typep-unknown-type.and-decided-by-first-part
  (list (%tut-try (%tut-u3 nil)) (%tut-try (%tut-u3 1)))
  (:error nil))

(deftest typep-unknown-type.unknown-first
  (%tut-try (%tut-u4 nil))
  :error)

(deftest typep-unknown-type.not-and-deeper
  (list (%tut-try (%tut-u5 nil)) (%tut-try (%tut-u5 1)) (%tut-try (%tut-u6 1))
        (%tut-try (%tut-u7 1)))
  (nil :error :error nil))

(deftype tut-redef-t () 'integer)
(defun %tut-g1 (x) (typep x 'tut-redef-t))
(defun %tut-g2 (x) (typep x '(or null tut-redef-t)))
(defun %tut-h1 (x) (typep x 'tut-late-t))
(defun %tut-h2 (x) (typep x '(or null tut-late2-t)))
(deftype tut-late-t () 'integer)
(deftype tut-late2-t () 'integer)

(deftest typep-redefined-type.before
  (list (%tut-g1 1) (%tut-g1 :a))
  (t nil))

(deftype tut-redef-t () 'symbol)

(deftest-compiled-only typep-redefined-type.after-redefinition
  (list (%tut-g1 1) (%tut-g1 :a) (%tut-g2 1) (%tut-g2 :a)
        (typep 1 (identity 'tut-redef-t)))
  (t nil t nil nil))

(deftest typep-redefined-type.defined-after-use
  (list (%tut-h1 1) (%tut-h1 :a) (%tut-h2 1) (%tut-h2 :a))
  (t nil t nil))
