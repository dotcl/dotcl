;;; typep.lisp — ANSI tests for TYPEP, TYPECASE, ETYPECASE, CHECK-TYPE

;;; --- Simple types ---

(deftest typep.integer.1
  (typep 42 'integer) t)

(deftest typep.integer.2
  (typep 42 'number) t)

(deftest typep.integer.3
  (typep 42 'fixnum) t)

(deftest typep.string.1
  (typep "hello" 'string) t)

(deftest typep.string.2
  (typep "hello" 'integer) nil)

(deftest typep.cons.1
  (typep '(1 2) 'cons) t)

(deftest typep.cons.2
  (typep nil 'cons) nil)

(deftest typep.null.1
  (typep nil 'null) t)

(deftest typep.null.2
  (typep 42 'null) nil)

(deftest typep.symbol.1
  (typep 'foo 'symbol) t)

(deftest typep.symbol.2
  (typep nil 'symbol) t)

(deftest typep.list.1
  (typep '(1 2) 'list) t)

(deftest typep.list.2
  (typep nil 'list) t)

(deftest typep.list.3
  (typep 42 'list) nil)

(deftest typep.atom.1
  (typep 42 'atom) t)

(deftest typep.atom.2
  (typep '(1 2) 'atom) nil)

(deftest typep.boolean.1
  (typep t 'boolean) t)

(deftest typep.boolean.2
  (typep nil 'boolean) t)

(deftest typep.boolean.3
  (typep 42 'boolean) nil)

(deftest typep.keyword.1
  (typep :foo 'keyword) t)

(deftest typep.keyword.2
  (typep 'foo 'keyword) nil)

(deftest typep.float.1
  (typep 1.0 'float) t)

(deftest typep.float.2
  (typep 1 'float) nil)

(deftest typep.character.1
  (typep #\a 'character) t)

(deftest typep.character.2
  (typep "a" 'character) nil)

(deftest typep.function.1
  (typep #'car 'function) t)

(deftest typep.function.2
  (typep 42 'function) nil)

(deftest typep.function.lambda
  (typep (lambda (x) x) 'function) t)

(deftest typep.real.integer
  (typep 42 'real) t)

(deftest typep.real.float
  (typep 3.14 'real) t)

(deftest typep.real.string
  (typep "hi" 'real) nil)

(deftest typep.rational.integer
  (typep 42 'rational) t)

(deftest typep.rational.float
  (typep 3.14 'rational) nil)

(deftest typep.number.float
  (typep 3.14 'number) t)

(deftest typep.number.string
  (typep "hi" 'number) nil)

(deftest typep.base-char
  (typep #\a 'base-char) t)

(deftest typep.standard-char
  (typep #\a 'standard-char) t)

(deftest typep.sequence.1
  (typep '(1 2) 'sequence) t)

(deftest typep.sequence.2
  (typep nil 'sequence) t)

(deftest typep.sequence.string
  (typep "hello" 'sequence) t)

(deftest typep.sequence.number
  (typep 42 'sequence) nil)

(deftest typep.t.1
  (typep 42 't) t)

(deftest typep.t.nil
  (typep nil 't) t)

(deftest typep.nil.1
  (typep 42 'nil) nil)

(deftest typep.nil.nil
  (typep nil 'nil) nil)

;;; --- Compound types ---

(deftest typep.or.1
  (typep 42 '(or integer string)) t)

(deftest typep.or.2
  (typep "hi" '(or integer string)) t)

(deftest typep.or.3
  (typep '(1) '(or integer string)) nil)

(deftest typep.and.1
  (typep nil '(and symbol list)) t)

(deftest typep.and.2
  (typep 42 '(and number integer)) t)

(deftest typep.and.3
  (typep 42 '(and number string)) nil)

(deftest typep.not.1
  (typep 42 '(not string)) t)

(deftest typep.not.2
  (typep "hi" '(not string)) nil)

(deftest typep.eql.1
  (typep 42 '(eql 42)) t)

(deftest typep.eql.2
  (typep 43 '(eql 42)) nil)

(deftest typep.member.1
  (typep 2 '(member 1 2 3)) t)

(deftest typep.member.2
  (typep 5 '(member 1 2 3)) nil)

(deftest typep.member.symbol
  (typep 'a '(member a b c)) t)

;;; --- Nested compound types ---

(deftest typep.or-and
  (typep 42 '(or string (and number integer))) t)

(deftest typep.not-or
  (typep 42 '(not (or string cons))) t)

(deftest typep.and-not
  (typep "hi" '(and (not integer) (not cons))) t)

;;; --- typecase ---

(deftest typecase.1
  (typecase 42
    (string "string")
    (integer "integer")
    (t "other"))
  "integer")

(deftest typecase.2
  (typecase "hello"
    (integer "int")
    (string "str"))
  "str")

(deftest typecase.3
  (typecase '(1 2)
    (integer "int")
    (string "str")
    (t "other"))
  "other")

(deftest typecase.nil
  (typecase nil
    (null "null")
    (t "other"))
  "null")

;;; --- typecase with no match ---

(deftest typecase.no-match
  (typecase 42
    (string "str")
    (cons "cons"))
  nil)

;;; --- typecase with body forms ---

(deftest typecase.body-forms
  (typecase "hello"
    (integer (+ 1 2) 3)
    (string (+ 10 20) 30))
  30)

;;; --- etypecase ---

(deftest etypecase.1
  (etypecase 42
    (integer "int")
    (string "str"))
  "int")

(deftest etypecase.string
  (etypecase "hi"
    (integer "int")
    (string "str"))
  "str")

;;; --- check-type ---

(deftest check-type.1
  (let ((x 42))
    (check-type x integer)
    x)
  42)

(deftest check-type.string
  (let ((s "hello"))
    (check-type s string)
    s)
  "hello")

;;; ============================================================
;;; etypecase error (from ansi-test/data-and-control-flow/etypecase.lsp)
;;; ============================================================

(deftest etypecase.error.1
  (signals-error (etypecase 42 (string "str") (cons "cons")) error)
  t)

(deftest etypecase.error.is-type-error
  (handler-case (etypecase 42 (string "str") (cons "cons"))
    (error (c) (typep c 'error)))
  t)

;;; check-type error (from ansi-test/conditions/check-type.lsp)

(deftest check-type.error.1
  (signals-error (let ((x 42)) (check-type x string)) error)
  t)

(do-tests-summary)
