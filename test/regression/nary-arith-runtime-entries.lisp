;;; N-ary arithmetic arrays, LCM, LOGXOR and the raw fixnum loop test;
;;; values and errors agree with SBCL 2.6.8.

(setf dotcl:*save-sil* t)

(defun %nar-sub (x) (- x 1 2 3 4 5 6 7 8))
(defun %nar-mul (x) (* x 1 2 3 4 5 6 7 8))
(defun %nar-div (x) (/ x 1 2 3 4 5 6 7 8))
(defun %nar-divz (x) (/ x 1 0 2 3 4 5 6 7 8))
(defun %nar-lcm (a b) (lcm a b))
(defun %nar-xor (x) (logxor x 1 2 3 4 5 6 7 8))
(defun %nar-ge (a b) (dotcl-internal::%fixnum-ge-object a b))

(defun %nar-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (division-by-zero () :division-by-zero)
    (type-error () :type-error)
    (error () :error)))

(deftest nary-arith-runtime-entries.values
  (list (%nar-sub 100) (%nar-sub 0) (%nar-sub 40.0d0) (%nar-sub 1.5d0)
        (%nar-mul 2) (%nar-mul -1) (%nar-mul 1.5d0)
        (%nar-div 40320) (%nar-div 80640) (%nar-div -40320) (%nar-div 1))
  (64 -36 4.0d0 -34.5d0 80640 -40320 60480.0d0 1 2 -1 1/40320))

(deftest nary-arith-runtime-entries.errors
  (list (%nar-error (lambda () (%nar-sub 'a)))
        (%nar-error (lambda () (%nar-mul 'a)))
        (%nar-error (lambda () (%nar-div 'a)))
        (%nar-error (lambda () (%nar-divz 1))))
  (:type-error :type-error :type-error :division-by-zero))

(deftest nary-arith-runtime-entries.lcm
  (list (%nar-lcm 4 6) (%nar-lcm -4 6) (%nar-lcm 0 5)
        (%nar-lcm (expt 2 70) 6)
        (%nar-error (lambda () (%nar-lcm 1.5 2)))
        (%nar-error (lambda () (%nar-lcm 'a 2))))
  (12 12 0 3541774862152233910272 :type-error :type-error))

(deftest nary-arith-runtime-entries.logxor
  (list (%nar-xor 0) (%nar-xor -1) (%nar-xor (expt 2 70))
        (%nar-error (lambda () (%nar-xor 'a))))
  (8 -9 1180591620717411303432 :type-error))

(deftest nary-arith-runtime-entries.fixnum-ge-object
  (list (%nar-ge 3 2) (%nar-ge 2 3) (%nar-ge 2 2)
        (%nar-ge 3 2.0) (%nar-ge 3 3.5) (%nar-ge 3 (expt 2 70))
        (%nar-error (lambda () (%nar-ge 3 'a))))
  (t nil t t nil nil :type-error))

(defun %nar-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(deftest-emitting-only nary-arith-runtime-entries.sil
  (list (%nar-has "Runtime.SubtractN" #'%nar-sub)
        (%nar-has "Runtime.MultiplyN" #'%nar-mul)
        (%nar-has "Runtime.DivideN" #'%nar-div)
        (%nar-has "Runtime.DivideN" #'%nar-divz)
        (%nar-has "Runtime.Lcm" #'%nar-lcm)
        (%nar-has "Runtime.Logxor" #'%nar-xor)
        (%nar-has "Runtime.FixnumGeObject" #'%nar-ge))
  (t t t t t t t))
