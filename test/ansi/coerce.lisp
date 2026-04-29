;;; coerce.lisp — ANSI tests for COERCE

;;; --- list to string ---

(deftest coerce.list-to-string.1
  (coerce '(#\h #\i) 'string)
  "hi")

(deftest coerce.nil-to-string
  (coerce nil 'string)
  "")

(deftest coerce.list-to-string.single
  (coerce '(#\x) 'string)
  "x")

;;; --- string to list ---

(deftest coerce.string-to-list.1
  (coerce "hi" 'list)
  (#\h #\i))

(deftest coerce.string-to-list.empty
  (coerce "" 'list)
  nil)

(deftest coerce.string-to-list.single
  (coerce "x" 'list)
  (#\x))

;;; --- integer to float ---

(deftest coerce.int-to-float.1
  (coerce 42 'float)
  42.0)

(deftest coerce.int-to-float.zero
  (coerce 0 'float)
  0.0)

(deftest coerce.int-to-double
  (coerce 42 'double-float)
  42.0d0)

;;; --- identity coerce ---

(deftest coerce.identity.1
  (coerce 42 't)
  42)

(deftest coerce.identity.string
  (coerce "hello" 't)
  "hello")

(deftest coerce.identity.nil
  (coerce nil 't)
  nil)

;;; --- character coerce ---

(deftest coerce.string-to-char.1
  (coerce "a" 'character)
  #\a)

;;; --- float identity ---

(deftest coerce.float-to-float
  (coerce 3.14 'float)
  3.14)

;;; --- type-error on invalid coerce ---

(deftest coerce.type-error.1
  (signals-error (coerce 42 'cons) type-error)
  t)

(deftest coerce.type-error.2
  (signals-error (coerce "hello" 'integer) type-error)
  t)

(do-tests-summary)
