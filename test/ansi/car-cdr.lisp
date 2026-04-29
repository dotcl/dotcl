;;; car-cdr.lisp — Tests from ansi-test/cons/cxr.lsp
;;; Excluded: c{a,d}{a,d}{a,d}{a,d}r (4-level accessors not yet implemented)
;;; Now includes signals-error type-error tests from cxr.lsp

(deftest cons.23
  (car '(a))
  a)

(deftest cons.24
  (cdr '(a . b))
  b)

(deftest cons.25
  (caar '((a)))
  a)

(deftest cons.26
  (cdar '((a . b)))
  b)

(deftest cons.27
  (cadr '(a b))
  b)

(deftest cons.28
  (cddr '(a b . c))
  c)

(deftest cons.34
  (caddr (cons 'a (cons 'b (cons 'c 'd))))
  c)

;;; car/cdr of nil
(deftest car-nil
  (car nil)
  nil)

(deftest cdr-nil
  (cdr nil)
  nil)

(deftest car-list
  (car '(1 2 3))
  1)

(deftest cdr-list
  (cdr '(1 2 3))
  (2 3))

;;; ============================================================
;;; type-error tests (from ansi-test/cons/cxr.lsp)
;;; ============================================================

(deftest car.error.2
  (signals-error (car 'a) type-error)
  t)

(deftest cdr.error.2
  (signals-error (cdr 'a) type-error)
  t)

(deftest caar.error.1
  (signals-error (caar 'a) type-error)
  t)

(deftest caar.error.2
  (signals-error (caar '(a)) type-error)
  t)

(deftest cadr.error.1
  (signals-error (cadr 'a) type-error)
  t)

(deftest cadr.error.2
  (signals-error (cadr '(a . b)) type-error)
  t)

(deftest cdar.error.1
  (signals-error (cdar 'a) type-error)
  t)

(deftest cdar.error.2
  (signals-error (cdar '(a . b)) type-error)
  t)

(deftest cddr.error.1
  (signals-error (cddr 'a) type-error)
  t)

(deftest cddr.error.2
  (signals-error (cddr '(a . b)) type-error)
  t)

(deftest caddr.error.1
  (signals-error (caddr 'a) type-error)
  t)

(deftest caddr.error.2
  (signals-error (caddr '(a . b)) type-error)
  t)

(deftest caddr.error.3
  (signals-error (caddr '(a c . b)) type-error)
  t)

(do-tests-summary)
