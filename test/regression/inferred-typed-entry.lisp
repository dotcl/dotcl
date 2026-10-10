;;; Typed entries without a declaimed FTYPE: a self-recursive global function
;;; whose required parameters are all declared a numeric type with a raw
;;; representation (an integer type that fits int64, or DOUBLE-FLOAT), and whose
;;; return type is inferred from its body as such a type, gets the same typed
;;; entry an FTYPE would give it (see typed-entry.lisp). Its self-calls are then
;;; direct calls of the typed body, with raw arguments and a raw result.
;;;
;;; What must hold:
;;; - the answers are those of the ordinary function, also after a redefinition;
;;; - a function that is not self-recursive, whose return type is not inferred
;;;   as such a type, with a parameter not declared, or NOTINLINE in its own
;;;   body, gets no typed entry;
;;; - from (safety 1) up a parameter not of the declared type is a TYPE-ERROR
;;;   naming the value;
;;; - a call to an ordinary function in the tail of such a function is still a
;;;   tail call (no typed entry then).

(defun %ite-sig (name) (dotcl::%typed-entry-signature (fdefinition name)))

(defun %ite-fib (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (the fixnum (+ (%ite-fib (the fixnum (- n 1)))
                     (%ite-fib (the fixnum (- n 2)))))))

(defun %ite-tak (x y z)
  (declare (fixnum x y z) (optimize (speed 3) (safety 0) (debug 0)))
  (if (not (< y x))
      z
      (%ite-tak (%ite-tak (the fixnum (- x 1)) y z)
                (%ite-tak (the fixnum (- y 1)) z x)
                (%ite-tak (the fixnum (- z 1)) x y))))

(defun %ite-half (x)
  (declare (double-float x))
  (if (< x 1d0) x (%ite-half (/ x 2d0))))

(defun %ite-sfib (n)
  (declare (fixnum n))
  (if (< n 2) n (the fixnum (+ (%ite-sfib (- n 1)) (%ite-sfib (- n 2))))))

(defun %ite-byte (n)
  (declare (type (unsigned-byte 8) n))
  (if (= n 0) 0 (the fixnum (+ 1 (%ite-byte (- n 1))))))

;; Not eligible.
(defun %ite-norec (n) (declare (fixnum n)) (the fixnum (* n 2)))
(defun %ite-integer (n)
  (declare (fixnum n))
  (if (< n 2) n (+ (%ite-integer (- n 1)) (%ite-integer (- n 2)))))
(defun %ite-undeclared (n m)
  (declare (fixnum n))
  (if (< n 2) m (the fixnum (%ite-undeclared (- n 1) m))))
(defun %ite-notinline (n)
  (declare (fixnum n) (notinline %ite-notinline))
  (if (< n 2) n (the fixnum (+ (%ite-notinline (- n 1)) (%ite-notinline (- n 2))))))

(deftest-emitting-only inferred-typed-entry.signatures
  (mapcar #'%ite-sig '(%ite-fib %ite-tak %ite-half %ite-sfib %ite-byte
                       %ite-norec %ite-integer %ite-undeclared %ite-notinline))
  ("L>L" "LLL>L" "D>D" "L>L" "L>L" nil nil nil nil))

(deftest inferred-typed-entry.values
  (list (%ite-fib 25) (%ite-tak 18 12 6) (%ite-half 100d0) (%ite-sfib 20) (%ite-byte 200)
        (%ite-norec 21) (%ite-integer 20) (%ite-undeclared 5 7) (%ite-notinline 20))
  (75025 7 0.78125d0 6765 200 42 6765 7 6765))

(deftest-compiled-only inferred-typed-entry.checked-at-safety-1
  (handler-case (%ite-sfib "a")
    (type-error (e) (list :type-error (type-error-datum e))))
  (:type-error "a"))

;;; Redefinition into a function without a typed entry, and back.
(defun %ite-redef (n)
  (declare (fixnum n))
  (if (< n 1) 0 (the fixnum (+ n (%ite-redef (- n 1))))))
(defun %ite-redef-caller (n) (%ite-redef n))

(deftest inferred-typed-entry.redefine
  (let ((r '()))
    (push (list (%ite-redef 10) (%ite-redef-caller 10)) r)
    (eval '(defun %ite-redef (n &optional (k 2)) (if (< n 1) 0 (+ k (%ite-redef (- n 1))))))
    (push (list (%ite-redef 10) (%ite-redef-caller 10)) r)
    (eval '(defun %ite-redef (n)
            (declare (fixnum n))
            (if (< n 1) 1 (the fixnum (* 2 (%ite-redef (- n 1)))))))
    (push (list (%ite-redef 10) (%ite-redef-caller 10)) r)
    (nreverse r))
  ((55 55) (20 20) (1024 1024)))

;;; A call to an ordinary function in the tail stays a tail call: a million
;;; round trips through it do not overflow the stack.
(defvar *ite-bounce*)
(defun %ite-bounce (x) (funcall *ite-bounce* (1+ x)))
(defun %ite-mutual (x)
  (declare (fixnum x))
  (cond ((< x 0) (%ite-mutual (- x)))
        ((< x 1000000) (the fixnum (%ite-bounce x)))
        (t x)))

(deftest-emitting-only inferred-typed-entry.tail-call-to-ordinary-function
  (progn (setf *ite-bounce* #'%ite-mutual)
         (%ite-mutual -3))
  1000000)
