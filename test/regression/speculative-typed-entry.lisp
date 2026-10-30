;;; Speculative typed entries: a self-recursive global function whose parameter
;;; is declared INTEGER (a type containing every fixnum and only integers, too
;;; wide for an int64) gets a typed entry that takes that parameter as a raw
;;; int64, used while the argument is a fixnum. The boxed entry runs the
;;; ordinary body for any other argument, and a self-call whose argument is not
;;; a fixnum (or overflows on the way) goes through the boxed entry. The result
;;; is an object.
;;;
;;; What must hold, compared with the ordinary function:
;;; - the same values around most-positive-fixnum / most-negative-fixnum, for
;;;   bignum arguments, and for bignum results;
;;; - multiple values through the tail and through a fallback call;
;;; - a TYPE-ERROR naming the value for an argument that is not an integer;
;;; - a redefinition is seen by callers, and the old function object keeps
;;;   working;
;;; - deep recursion ends in a STORAGE-CONDITION;
;;; - the same from a fasl.

(defun %ste-sig (name) (dotcl::%typed-entry-signature (fdefinition name)))

(defun %ste-fib (n)
  (declare (type integer n))
  (if (< n 2) 1 (+ (%ste-fib (- n 1)) (%ste-fib (- n 2)))))
(defun %ste-halves (n)
  (declare (type integer n))
  (if (< n 2) 0 (1+ (%ste-halves (floor n 2)))))
(defun %ste-up (n k)
  (declare (integer n k))
  (if (= k 0) n (%ste-up (+ n 1) (- k 1))))
(defun %ste-up-nontail (n k)
  (declare (integer n k))
  (if (= k 0) n (let ((r (%ste-up-nontail (+ n 1) (- k 1)))) r)))
(defun %ste-down (n k)
  (declare (integer n k))
  (if (= k 0) n (%ste-down (- n 1) (- k 1))))
(defun %ste-fact (n)
  (declare (integer n))
  (if (< n 2) 1 (* n (%ste-fact (- n 1)))))
(defun %ste-mv-tail (n)
  (declare (integer n))
  (if (<= n 0) (values :a :b) (%ste-mv-tail (- n 1))))
(defun %ste-mv-double (n)
  (declare (integer n))
  (if (> n most-positive-fixnum) (values n :big) (%ste-mv-double (* n 2))))
(defun %ste-mv-list (n)
  (declare (integer n))
  (if (> n most-positive-fixnum)
      (values n :big)
      (multiple-value-list (%ste-mv-list (* n 2)))))
(defun %ste-tak (x y z)
  (declare (integer x y z))
  (if (not (< y x))
      z
      (%ste-tak (%ste-tak (1- x) y z) (%ste-tak (1- y) z x) (%ste-tak (1- z) x y))))
(defun %ste-mixed (n acc)
  (declare (integer n))
  (if (= n 0) acc (%ste-mixed (- n 1) (cons n acc))))

;; Not eligible.
(defun %ste-assigned (n)
  (declare (integer n))
  (setq n (* n 2))
  (if (> n 100) n (%ste-assigned n)))
(defun %ste-notinline (n)
  (declare (integer n) (notinline %ste-notinline))
  (if (< n 2) 1 (+ (%ste-notinline (- n 1)) (%ste-notinline (- n 2)))))
(defun %ste-ranged (n)
  (declare (type (integer 0) n))
  (if (< n 2) 1 (+ (%ste-ranged (- n 1)) (%ste-ranged (- n 2)))))
(defun %ste-norec (n) (declare (integer n)) (* n 2))

(deftest-emitting-only speculative-typed-entry.signatures
  (mapcar #'%ste-sig '(%ste-fib %ste-halves %ste-up %ste-up-nontail %ste-down %ste-fact
                       %ste-mv-tail %ste-mv-double %ste-mv-list %ste-tak %ste-mixed
                       %ste-assigned %ste-notinline %ste-ranged %ste-norec))
  ("L>O" "L>O" "LL>O" "LL>O" "LL>O" "L>O" "L>O" "L>O" "L>O" "LLL>O" "LO>O"
   nil nil nil nil))

(deftest speculative-typed-entry.values
  (list (%ste-fib 20) (%ste-halves (expt 2 100)) (%ste-halves 1000)
        (%ste-fact 25) (%ste-tak 18 12 6) (%ste-mixed 3 nil)
        (%ste-assigned 3) (%ste-notinline 10) (%ste-ranged 10) (%ste-norec 4))
  (10946 100 9 15511210043330985984000000 7 (1 2 3) 192 89 89 8))

(deftest speculative-typed-entry.overflow
  (list (- (%ste-up (- most-positive-fixnum 2) 5) most-positive-fixnum)
        (- (%ste-up-nontail (- most-positive-fixnum 2) 5) most-positive-fixnum)
        (- (%ste-down (+ most-negative-fixnum 2) 5) most-negative-fixnum)
        (- (%ste-down (- most-negative-fixnum 3) 2) most-negative-fixnum)
        (- (%ste-down (+ most-negative-fixnum 1) 1) most-negative-fixnum)
        (- (%ste-up (- most-negative-fixnum 2) 4) most-negative-fixnum)
        (- (%ste-up (+ most-positive-fixnum 1) 3) most-positive-fixnum))
  (3 3 -3 -5 0 2 4))

(deftest speculative-typed-entry.multiple-values
  (list (multiple-value-list (%ste-mv-tail 5))
        (multiple-value-list (%ste-mv-double 1))
        (let ((x (%ste-mv-list (ash 1 61))))
          (list (length x) (car x) (second (car x)))))
  ((:a :b) (#.(1+ most-positive-fixnum) :big) (1 (#.(1+ most-positive-fixnum) :big) :big)))

(deftest-compiled-only speculative-typed-entry.type-error
  (flet ((te (thunk) (handler-case (funcall thunk)
                       (type-error (e) (list :type-error (type-error-datum e))))))
    (list (te (lambda () (%ste-fib "a")))
          (te (lambda () (%ste-fib 2.5)))
          (te (lambda () (%ste-up 1 "x")))
          (te (lambda () (%ste-ranged -1)))))
  ((:type-error "a") (:type-error 2.5) (:type-error "x") (:type-error -1)))

(defun %ste-redef (n)
  (declare (integer n))
  (if (< n 2) 1 (+ (%ste-redef (- n 1)) (%ste-redef (- n 2)))))
(defun %ste-redef-caller (n) (%ste-redef n))

(deftest speculative-typed-entry.redefine
  (let ((old (fdefinition '%ste-redef))
        (before (%ste-redef-caller 10)))
    (eval '(defun %ste-redef (n)
            (declare (integer n))
            (if (< n 2) 0 (+ (%ste-redef (- n 1)) (%ste-redef (- n 2))))))
    (list before (%ste-redef-caller 10) (funcall old 1)
          (%ste-redef-caller (- most-negative-fixnum 1))))
  (89 0 1 0))

(defun %ste-deep (n)
  (declare (integer n))
  (if (= n 0) 0 (1+ (%ste-deep (- n 1)))))

(deftest speculative-typed-entry.deep-recursion
  (list (%ste-deep 10000)
        (call-on-small-stack
         (lambda ()
           (handler-case (%ste-deep 100000000)
             (storage-condition () :storage-condition))))
        (call-on-small-stack
         (lambda ()
           (handler-case (%ste-deep (+ most-positive-fixnum 100000000))
             (storage-condition () :storage-condition)))))
  (10000 :storage-condition :storage-condition))

(defvar *ste-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-ste-test/")))
    (ensure-directories-exist dir)
    dir))

(deftest-compiled-only speculative-typed-entry.fasl
  (let ((lisp (concatenate 'string *ste-dir* "ste-fasl.lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string
       "(in-package :cl-user)
        (defun %ste-f-fib (n)
          (declare (type integer n))
          (if (< n 2) 1 (+ (%ste-f-fib (- n 1)) (%ste-f-fib (- n 2)))))
        (defun %ste-f-up (n k)
          (declare (integer n k))
          (if (= k 0) n (%ste-f-up (+ n 1) (- k 1))))
        (defun %ste-f-halves (n)
          (declare (type integer n))
          (if (< n 2) 0 (1+ (%ste-f-halves (floor n 2)))))"
       s))
    (load (compile-file lisp))
    (list (%ste-sig (intern "%STE-F-FIB"))
          (funcall (intern "%STE-F-FIB") 20)
          (- (funcall (intern "%STE-F-UP") (- most-positive-fixnum 1) 4) most-positive-fixnum)
          (funcall (intern "%STE-F-HALVES") (expt 2 80))
          (handler-case (funcall (intern "%STE-F-FIB") 'x)
            (type-error (e) (type-error-datum e)))))
  ("L>O" 10946 3 80 x))

;;; The search for a self-call does not look into quoted data, which may be
;;; circular.
(defun %ste-circular (n)
  (declare (integer n))
  (if (< n 1) '#1=(a b . #1#) (%ste-circular (- n 1))))

(deftest speculative-typed-entry.circular-constant
  (let ((x (%ste-circular 3))) (list (first x) (third x)))
  (a a))
