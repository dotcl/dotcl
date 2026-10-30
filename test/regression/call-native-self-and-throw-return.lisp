;;; Native self-calls and THROW forwarding; values agree with SBCL 2.6.8.

(setf dotcl:*save-sil* t)

(declaim (ftype (function (&rest t) fixnum) %cns-nat1))
(defun %cns-nat1 (n)
  (declare (fixnum n))
  (if (<= n 0) 42
      (let ((r (%cns-nat1 (- n 1)))) (declare (fixnum r)) (1+ r))))

(declaim (ftype (function (&rest t) fixnum) %cns-nat2))
(defun %cns-nat2 (a b)
  (declare (fixnum a b))
  (if (<= a 0) b
      (let ((r (%cns-nat2 (- a 1) (1+ b)))) (declare (fixnum r)) (1+ r))))

(declaim (ftype (function (&rest t) fixnum) %cns-nat3))
(defun %cns-nat3 (a b c)
  (declare (fixnum a b c))
  (if (<= a 0) (+ b c)
      (let ((r (%cns-nat3 (- a 1) (1+ b) (1+ c)))) (declare (fixnum r)) (1+ r))))

(declaim (ftype (function (&rest t) fixnum) %cns-nat4))
(defun %cns-nat4 (a b c d)
  (declare (fixnum a b c d))
  (if (<= a 0) (+ b c d)
      (let ((r (%cns-nat4 (- a 1) (1+ b) (1+ c) (1+ d)))) (declare (fixnum r)) (1+ r))))

(defvar *ctr-f0* 0)
(defvar *ctr-f0m* 0)

(defun %ctr-build-without-modes ()
  (let ((old dotcl-internal::*mv-mode-elision*))
    (setq dotcl-internal::*mv-mode-elision* nil)
    (unwind-protect
        (eval '(progn
                 (defun %ctr-fwd0 ()
                   (if (<= *ctr-f0* 0)
                       (throw '%ctr-fwd0 :f0)
                       (progn (decf *ctr-f0*)
                              (multiple-value-prog1 (%ctr-fwd0) nil))))
                 (defun %ctr-fwd1 (n)
                   (if (< n 0)
                       (throw '%ctr-fwd1 :neg)
                       (if (= n 0) 7
                           (multiple-value-prog1 (%ctr-fwd1 (1- n)) nil))))
                 (defun %ctr-fwd3 (n a b)
                   (if (< n 0)
                       (throw '%ctr-fwd3 (list n a b))
                       (if (= n 0) (list a b)
                           (multiple-value-prog1
                               (%ctr-fwd3 (1- n) (+ a 1) (+ b 2)) nil))))
                 (defun %ctr-fwd4 (n a b c)
                   (if (< n 0)
                       (throw '%ctr-fwd4 (list n a b c))
                       (if (= n 0) (list a b c)
                           (multiple-value-prog1
                               (%ctr-fwd4 (1- n) (+ a 1) (+ b 2) (+ c 3))
                             nil))))))
      (setq dotcl-internal::*mv-mode-elision* old))))
(%ctr-build-without-modes)

(defun %ctr-fwd0m ()
  (if (<= *ctr-f0m* 0)
      (throw '%ctr-fwd0m :f0m)
      (progn (decf *ctr-f0m*) (list (%ctr-fwd0m)))))

(defun %cns-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %cns-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)
    (program-error () :program-error)
    (error () :error)))

(defun %cns-error-info (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (c) (list :type-error (type-error-datum c)
                          (type-error-expected-type c)))
    (error () :error)))

(defun %cns-compile-load (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (load (compile-file src :output-file fasl))
    t))

(deftest call-native-self.values
  (list (%cns-nat1 5) (%cns-nat2 3 10) (%cns-nat3 2 10 20)
        (%cns-nat4 2 10 20 30))
  (47 16 36 68))

(deftest call-native-self.wrong-argument
  (list (%cns-error (lambda () (%cns-nat1 1.5)))
        (%cns-error (lambda () (%cns-nat4 1 2 3)))
        (%cns-error (lambda () (%cns-nat2 1 2 3))))
  (:type-error :program-error :program-error))

(deftest call-native-self.wrong-argument-datum
  (list (%cns-error-info (lambda () (%cns-nat1 1.5)))
        (%cns-error-info (lambda () (%cns-nat2 1 :not-a-fixnum)))
        (%cns-error-info (lambda () (%cns-nat3 1 2 "x")))
        (%cns-error-info (lambda () (%cns-nat4 1 2 3 4.0))))
  ((:type-error 1.5 fixnum)
   (:type-error :not-a-fixnum fixnum)
   (:type-error "x" fixnum)
   (:type-error 4.0 fixnum)))

(deftest-emitting-only call-native-self.fasl-wrong-argument-datum
  (progn
    (%cns-compile-load
     "cns-fasl"
     '("(declaim (ftype (function (&rest t) fixnum) %cns-fasl-nat1))"
       "(defun %cns-fasl-nat1 (n) (declare (fixnum n))"
       "  (if (<= n 0) 42 (1+ (%cns-fasl-nat1 (- n 1)))))"))
    (list (%cns-error-info (lambda () (funcall #'%cns-fasl-nat1 1.5)))
          (%cns-fasl-nat1 2)))
  ((:type-error 1.5 fixnum) 44))

(deftest call-native-self.throw-return-values
  (list (%ctr-fwd1 5)
        (catch '%ctr-fwd1 (%ctr-fwd1 -1))
        (%ctr-fwd3 3 10 20)
        (catch '%ctr-fwd3 (%ctr-fwd3 -1 10 20))
        (%ctr-fwd4 2 10 20 30)
        (catch '%ctr-fwd4 (%ctr-fwd4 -1 10 20 30)))
  (7 :neg (13 26) (-1 10 20) (12 24 36) (-1 10 20 30)))

(deftest call-native-self.throw-return-zero-arity
  (progn
    (setq *ctr-f0* 3)
    (let ((a (catch '%ctr-fwd0 (%ctr-fwd0))))
      (setq *ctr-f0* 0)
      (let ((b (catch '%ctr-fwd0 (%ctr-fwd0))))
        (setq *ctr-f0m* 3)
        (let ((c (catch '%ctr-fwd0m (%ctr-fwd0m))))
          (setq *ctr-f0m* 0)
          (list a b c (catch '%ctr-fwd0m (%ctr-fwd0m)))))))
  (:f0 :f0 :f0m :f0m))

(deftest-emitting-only call-native-self.uses-the-native-entries
  (list (%cns-has "LispFunction.InvokeNative1" #'%cns-nat1)
        (%cns-has "LispFunction.InvokeNative2" #'%cns-nat2)
        (%cns-has "LispFunction.InvokeNative3" #'%cns-nat3)
        (%cns-has "LispFunction.InvokeNative4" #'%cns-nat4))
  (t t t t))

(deftest-compiled-only call-native-self.uses-the-throw-return-entries
  (list (%cns-has "ThrowReturn.Forward0)" #'%ctr-fwd0)
        (%cns-has "ThrowReturn.Forward0M" #'%ctr-fwd0m)
        (%cns-has "ThrowReturn.Forward1)" #'%ctr-fwd1)
        (%cns-has "ThrowReturn.Forward3)" #'%ctr-fwd3)
        (%cns-has "ThrowReturn.Forward4)" #'%ctr-fwd4))
  (t t t t t))
