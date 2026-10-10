;;; Typed entries: a global function whose DECLAIM FTYPE gives every argument
;;; and the result a numeric type with a raw representation (an integer type
;;; that fits int64, or DOUBLE-FLOAT) is compiled into a body taking and
;;; returning raw values, and its ordinary entry is a shell over that body.
;;;
;;; What must hold:
;;; - the shell and the typed entry give the same answers, also after the
;;;   function is redefined (into another typed body or into one without a
;;;   typed entry), compiled in memory or loaded from a fasl;
;;; - from (safety 1) up the declarations are checked: an argument or a result
;;;   not of the declared type is a TYPE-ERROR naming the value and the type;
;;; - at (safety 0) they are trusted (no range check);
;;; - a self-call in tail position is still a loop.
;;;
;;; DOTCL::%TYPED-ENTRY-SIGNATURE / DOTCL::%INVOKE-TYPED reach the typed entry
;;; directly. Only an image with an emitter makes typed entries, so the
;;; assertions about them are DEFTEST-EMITTING-ONLY, and the checks a typed entry
;;; adds are DEFTEST-COMPILED-ONLY. The values are the same everywhere.

(defun %te-sig (name) (dotcl::%typed-entry-signature (fdefinition name)))

(defun %te-outcome (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e) (type-error-expected-type e)))))

(declaim (ftype (function (fixnum fixnum) fixnum) %te-add))
(defun %te-add (a b) (+ a b))

(declaim (ftype (function (double-float double-float) double-float) %te-addd))
(defun %te-addd (a b) (* (+ a b) 2))

(declaim (ftype (function (fixnum double-float) (values double-float &optional)) %te-mix))
(defun %te-mix (n x)
  (let ((y (* x 2)))
    (if (= n 0) y (%te-mix (1- n) (+ y 1d0)))))

(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %te-inc8))
(defun %te-inc8 (x) (1+ x))

(declaim (ftype (function (fixnum) fixnum) %te-fib))
(defun %te-fib (n) (if (< n 2) n (+ (%te-fib (- n 1)) (%te-fib (- n 2)))))

;; Not eligible: an argument type with no raw representation, a lambda list the
;; FTYPE does not describe, a parameter a closure captures.
(declaim (ftype (function (t fixnum) fixnum) %te-t-arg))
(defun %te-t-arg (a b) (declare (ignore a)) b)
(declaim (ftype (function (fixnum &optional fixnum) fixnum) %te-opt))
(defun %te-opt (a &optional (b 1)) (+ a b))
(declaim (ftype (function (fixnum) fixnum) %te-captured))
(defun %te-captured (a) (funcall (lambda () (+ a 1))))

(deftest-emitting-only typed-entry.signatures
  (mapcar #'%te-sig '(%te-add %te-addd %te-mix %te-inc8 %te-fib %te-t-arg %te-opt %te-captured))
  ("LL>L" "DD>D" "LD>D" "L>L" "L>L" nil nil nil))

(deftest typed-entry.values
  (list (%te-add 1000000000000 7) (%te-addd 1d0 2d0) (%te-mix 3 1d0) (%te-inc8 3)
        (%te-fib 20) (list (%te-fib 10)) (%te-t-arg :x 5) (%te-opt 1) (%te-captured 1))
  (1000000000007 6d0 30d0 4 6765 (55) 5 2 2))

(deftest-emitting-only typed-entry.both-entries-agree
  (list (dotcl::%invoke-typed #'%te-add 1000000000000 7)
        (dotcl::%invoke-typed #'%te-addd 1d0 2d0)
        (dotcl::%invoke-typed #'%te-mix 3 1d0)
        (dotcl::%invoke-typed #'%te-fib 20))
  (1000000000007 6d0 30d0 6765))

;;; (VALUES X &OPTIONAL) is one value, whatever the body's last form returns.
(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) %te-quot))
(defun %te-quot (a b) (floor a b))

(deftest-emitting-only typed-entry.single-value
  (multiple-value-list (%te-quot 7 2))
  (3))

;;; A self-call in tail position is still the loop, through COND, WHEN and LET.
(declaim (ftype (function (fixnum fixnum) fixnum) %te-loop))
(defun %te-loop (i acc)
  (cond ((= i 0) acc)
        (t (let ((j (1- i)))
             (%te-loop j (+ acc i))))))

(declaim (ftype (function (fixnum double-float) double-float) %te-dloop))
(defun %te-dloop (i x)
  (when (= i 0) (return-from %te-dloop x))
  (%te-dloop (1- i) (+ x 0.5d0)))

(deftest-emitting-only typed-entry.tail-self-call-loops
  (list (%te-loop 10000000 0) (%te-dloop 10000000 0d0))
  (50000005000000 5000000d0))

;;; Redefinition: the shell and the typed entry belong to one definition, and a
;;; caller compiled against the old one reaches the new one.
(declaim (ftype (function (fixnum fixnum) fixnum) %te-redef))
(defun %te-redef (a b) (+ a b))
(defun %te-redef-caller (x) (%te-redef x 10))

(deftest-emitting-only typed-entry.redefine
  (let ((r '()))
    (push (list (%te-redef 2 3) (dotcl::%invoke-typed #'%te-redef 2 3)
                (%te-redef-caller 2) (%te-sig '%te-redef))
          r)
    (eval '(defun %te-redef (a b) (* a b)))
    (push (list (%te-redef 2 3) (dotcl::%invoke-typed #'%te-redef 2 3)
                (%te-redef-caller 2) (%te-sig '%te-redef))
          r)
    ;; Into a definition with no typed entry: the ordinary entry answers.
    (eval '(defun %te-redef (a b &optional (c 100)) (+ a b c)))
    (push (list (%te-redef 2 3) (dotcl::%invoke-typed #'%te-redef 2 3)
                (%te-redef-caller 2) (%te-sig '%te-redef))
          r)
    (eval '(defun %te-redef (a b) (- a b)))
    (push (list (%te-redef 2 3) (dotcl::%invoke-typed #'%te-redef 2 3)
                (%te-redef-caller 2) (%te-sig '%te-redef))
          r)
    (nreverse r))
  ((5 5 12 "LL>L") (6 6 20 "LL>L") (105 105 112 nil) (-1 -1 -8 "LL>L")))

;;; From (safety 1) up the declaration is checked, on the way in and out.
(declaim (ftype (function (fixnum) fixnum) %te-lie))
(defun %te-lie (x) (if (> x 0) x "negative"))

(deftest-compiled-only typed-entry.checked-at-safety-1
  (list (%te-outcome (lambda () (%te-add 1 "x")))
        (%te-outcome (lambda () (%te-addd 1 2d0)))
        (%te-outcome (lambda () (%te-inc8 300)))
        (%te-outcome (lambda () (%te-inc8 255)))
        (%te-outcome (lambda () (%te-lie -1)))
        (%te-outcome (lambda () (%te-add most-positive-fixnum 1)))
        (%te-lie 5))
  ((:type-error "x" fixnum)
   (:type-error 1 double-float)
   (:type-error 300 (integer 0 255))
   (:type-error 256 (integer 0 255))
   (:type-error "negative" fixnum)
   (:type-error 9223372036854775808 fixnum)
   5))

;;; At (safety 0) the declaration is trusted: no range check either way.
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %te-inc8-s0))
(defun %te-inc8-s0 (x) (declare (optimize (safety 0))) (1+ x))

(deftest typed-entry.trusted-at-safety-0
  (list (%te-inc8-s0 3) (%te-inc8-s0 255) (%te-inc8-s0 300))
  (4 256 301))

;;; A parameter the body assigns does not keep the declared range for the
;;; arithmetic after the assignment.
(declaim (ftype (function ((unsigned-byte 8)) fixnum) %te-assigned))
(defun %te-assigned (x) (setq x (* x 1000)) (* x 1000))

(deftest typed-entry.assigned-parameter
  (%te-assigned 255)
  255000000)

;;; The calls to the function from a local function or a closure in its own body
;;; are ordinary calls.
(declaim (ftype (function (fixnum) fixnum) %te-outer))
(defun %te-outer (n)
  (labels ((h (k) (if (= k 0) 0 (+ 1 (%te-outer (1- k))))))
    (+ (h n) (funcall (lambda (m) (if (= m 0) 0 (%te-outer 0))) n))))

(deftest typed-entry.calls-from-inner-functions
  (list (%te-outer 0) (%te-outer 5))
  (0 5))

;;; The same definitions from a fasl: the typed body is a method of the fasl.
(defvar *te-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-te-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %te-compile-and-load (source name)
  (let ((lisp (concatenate 'string *te-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-compiled-only typed-entry.fasl
  (progn
    (%te-compile-and-load
     "(in-package :cl-user)
      (declaim (ftype (function (fixnum fixnum) fixnum) %te-f-add))
      (defun %te-f-add (a b) (+ a b))
      (declaim (ftype (function (fixnum double-float) double-float) %te-f-mix))
      (defun %te-f-mix (n x) (if (= n 0) x (%te-f-mix (1- n) (* x 2))))
      (declaim (ftype (function (fixnum) fixnum) %te-f-fib))
      (defun %te-f-fib (n) (if (< n 2) n (+ (%te-f-fib (- n 1)) (%te-f-fib (- n 2)))))
      (declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %te-f-inc8))
      (defun %te-f-inc8 (x) (1+ x))"
     "te-fasl")
    (list (funcall (intern "%TE-F-ADD") 1000000000000 7)
          (funcall (intern "%TE-F-MIX") 3 1.5d0)
          (funcall (intern "%TE-F-FIB") 20)
          (%te-outcome (lambda () (funcall (intern "%TE-F-INC8") 255)))
          (%te-outcome (lambda () (funcall (intern "%TE-F-ADD") 1 2.0d0)))
          (mapcar #'%te-sig (list (intern "%TE-F-ADD") (intern "%TE-F-MIX")
                                  (intern "%TE-F-FIB") (intern "%TE-F-INC8")))
          (dotcl::%invoke-typed (fdefinition (intern "%TE-F-MIX")) 3 1.5d0)
          ;; Redefined in memory over the fasl's definition.
          (progn (eval `(defun ,(intern "%TE-F-ADD") (a b) (- a b)))
                 (list (funcall (intern "%TE-F-ADD") 10 3)
                       (dotcl::%invoke-typed (fdefinition (intern "%TE-F-ADD")) 10 3)))))
  (1000000000007 12d0 6765
   (:type-error 256 (integer 0 255))
   (:type-error 2.0d0 fixnum)
   ("LL>L" "LD>D" "L>L" "L>L")
   12d0
   (7 7)))
