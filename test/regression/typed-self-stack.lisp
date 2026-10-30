;;; A typed body calls itself directly, without the frame and stack check of
;;; the DEFUN entry. Unbounded recursion through it must still end in a
;;; STORAGE-CONDITION a handler can catch, not take the process down.

(defparameter *tss-source* "
(declaim (ftype (function (fixnum) fixnum) %tss-inf %tss-mut-a %tss-mut-b %tss-sum))
(defun %tss-inf (n) (+ 1 (%tss-inf (+ n 1))))
(defun %tss-inf-inferred (n)
  (declare (fixnum n))
  (if (< n -5) 0 (the fixnum (+ 1 (the fixnum (%tss-inf-inferred (+ n 1)))))))
(defun %tss-mut-a (n) (+ 1 (%tss-mut-b (+ n 1))))
(defun %tss-mut-b (n) (+ 1 (%tss-mut-a (+ n 1))))
(defun %tss-sum (n) (if (<= n 0) 0 (+ n (%tss-sum (- n 1)))))
(declaim (ftype (function (fixnum fixnum) fixnum) %tss-uses-self))
(defun %tss-uses-self (n k)
  (if (<= n 0) (length (list #'%tss-uses-self k)) (+ 1 (%tss-uses-self (- n 1) k))))
")

(defun %tss-load ()
  (let ((src (regression-temp-file "typed-self-stack-src.lisp"))
        (fasl (regression-temp-file "typed-self-stack-src.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *tss-source* s))
    (load (compile-file src :output-file fasl))))

(when (emitting-mode-p) (%tss-load))

(defun %tss-overflow (f)
  (call-on-small-stack
   (lambda ()
     (handler-case (progn (funcall f 0) :no-overflow)
       (storage-condition () :storage-condition)
       (error () :error)))))

(deftest-emitting-only typed-self-stack.typed-entries
  (mapcar (lambda (f) (dotnet:static "DotCL.LispFunction" "TypedEntryState" f))
          (list #'%tss-inf #'%tss-inf-inferred #'%tss-mut-a))
  ("framed" "framed" "framed"))

(deftest-emitting-only typed-self-stack.ftype
  (list (%tss-overflow #'%tss-inf) (%tss-overflow #'%tss-inf) (%tss-sum 100))
  (:storage-condition :storage-condition 5050))

(deftest-emitting-only typed-self-stack.inferred
  (list (%tss-overflow #'%tss-inf-inferred) (%tss-sum 10))
  (:storage-condition 55))

(deftest-emitting-only typed-self-stack.mutual
  (list (%tss-overflow #'%tss-mut-a) (%tss-sum 3))
  (:storage-condition 6))

;; Depths across many multiples of the check interval, with and without a body
;; that also uses its function object.
(deftest-emitting-only typed-self-stack.deep-finite
  (list (%tss-sum 5000) (%tss-uses-self 1000 7)
        (call-on-small-stack (lambda () (%tss-sum 20000))))
  (12502500 1002 200010000))
