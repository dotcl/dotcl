;;; Framed typed entries of five and six arguments: a caller that knows the
;;; FTYPE enters the callee's typed body by its address, and the entry links
;;; the callee's frame itself. Values, frames, the frame list after exits,
;;; redefinition, &optional on either side, type errors and fasls.

(defun %tfw-depth () (length (dotcl:backtrace)))
(defun %tfw-probe () (dotcl:backtrace-with-args))
(defvar *tfw-frames* nil)

(defstruct %tfw-box lo hi)

(declaim (notinline %tfw-slab %tfw-six %tfw-five-l %tfw-thrower %tfw-framed5 %tfw-quiet5))
(declaim (ftype (function (double-float double-float double-float double-float %tfw-box)
                          (values t &optional))
                %tfw-slab))
(defun %tfw-slab (o d lo hi b)
  (let ((t0 (/ (- lo o) d)) (t1 (/ (- hi o) d)))
    (if (< (max t0 (%tfw-box-lo b)) (min t1 (%tfw-box-hi b))) (list t0 t1) nil)))
(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum) (values fixnum &optional))
                %tfw-six))
(defun %tfw-six (a b c d e f) (+ a (* 2 b) (* 3 c) (* 4 d) (* 5 e) (* 6 f)))
(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum) (values fixnum &optional))
                %tfw-five-l %tfw-quiet5))
(defun %tfw-five-l (a b c d e) (- (+ a b c d) e))
(defun %tfw-quiet5 (a b c d e) (declare (optimize (debug 0))) (+ a b c d e))
(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum) (values fixnum &optional))
                %tfw-thrower))
(defun %tfw-thrower (n a b c d)
  (cond ((= n 0) (throw :tfw (+ a b c d)))
        ((= n 1) (error "tfw ~a" n))
        (t (+ n a b c d))))
(declaim (ftype (function (t fixnum t double-float fixnum) (values t &optional)) %tfw-framed5))
(defun %tfw-framed5 (a b c d e) (setq *tfw-frames* (%tfw-probe)) (list a b c d e))

(deftest-compiled-only typed-framed-entry-wide.state
  (mapcar (lambda (f) (list (dotcl::%typed-entry-signature (fdefinition f))
                            (dotnet:static "DotCL.LispFunction" "TypedEntryState" (fdefinition f))))
          '(%tfw-slab %tfw-six %tfw-five-l %tfw-quiet5))
  (("DDDDO>O" "framed") ("LLLLLL>L" "framed") ("LLLLL>L" "framed") ("LLLLL>L" "typed")))

(defun %tfw-user (n)
  (let ((b (make-%tfw-box :lo 0d0 :hi 10d0)) (s 0))
    (declare (fixnum s))
    (dotimes (i n)
      (setq s (+ s (%tfw-six i 1 2 3 4 5) (%tfw-five-l i 1 2 3 4) (%tfw-quiet5 i 1 1 1 1))))
    (list s (%tfw-slab 0d0 1d0 2d0 4d0 b) (%tfw-slab 0d0 1d0 20d0 40d0 b))))

(deftest typed-framed-entry-wide.values
  (%tfw-user 10)
  (895 (2d0 4d0) nil))

(deftest typed-framed-entry-wide.boxed-entry
  (list (funcall '%tfw-six 1 1 1 1 1 1)
        (apply '%tfw-five-l '(1 2 3 4 5))
        (funcall #'%tfw-slab 0d0 2d0 2d0 4d0 (make-%tfw-box :lo 0d0 :hi 5d0)))
  (21 5 (1d0 2d0)))

(deftest typed-framed-entry-wide.frame
  (progn (list (%tfw-framed5 "a" 1 'c 2.5d0 3))
         (find "%TFW-FRAMED5" *tfw-frames* :key #'car :test #'string=))
  ("%TFW-FRAMED5" "a" 1 c 2.5d0 3))

(defun %tfw-call-thrower (n) (list (%tfw-thrower n 1 2 3 4)))

(deftest typed-framed-entry-wide.frame-list-after-exits
  (let ((base (%tfw-depth)))
    (list (catch :tfw (%tfw-call-thrower 0))
          (- (%tfw-depth) base)
          (handler-case (%tfw-call-thrower 1) (simple-error () :error))
          (- (%tfw-depth) base)
          (block out (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from out :escaped))))
                       (%tfw-call-thrower 1)))
          (- (%tfw-depth) base)
          (%tfw-call-thrower 5)))
  (10 0 :error 0 :escaped 0 (15)))

;;; A false argument type: the boxed entry and a caller that cannot prove the
;;; type both signal TYPE-ERROR, and the frame list is intact afterwards.
(defun %tfw-call-six (x) (%tfw-six 1 2 3 4 5 x))

(deftest-compiled-only typed-framed-entry-wide.type-error
  (let ((base (%tfw-depth)))
    (list (handler-case (funcall '%tfw-six 1 2 3 4 5 1.5)
            (type-error (e) (list :type-error (type-error-datum e))))
          (handler-case (%tfw-call-six "x")
            (type-error (e) (list :type-error (type-error-datum e))))
          (handler-case (funcall '%tfw-slab 0d0 1d0 2d0 4d0 :not-a-box)
            (type-error (e) (list :type-error (type-error-datum e))))
          (- (%tfw-depth) base)))
  ((:type-error 1.5) (:type-error "x") (:type-error :not-a-box) 0))

;;; Redefinition: into another typed body, into one with &optional, into a
;;; plain function; and a typed caller of an &optional function.
(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum) (values t &optional)) %tfw-redef))
(defun %tfw-redef (a b c d e) (list a b c d e))
(defun %tfw-redef-caller (n) (list (%tfw-redef n 1 2 3 4)))
(defun %tfw-opt (a &optional (b 10)) (+ a b))
(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum) (values t &optional)) %tfw-calls-opt))
(defun %tfw-calls-opt (a b c d e f) (list (%tfw-opt a) (%tfw-opt b c) (+ d e f)))

(deftest typed-framed-entry-wide.redefine
  (let ((r '()))
    (push (%tfw-redef-caller 0) r)
    (eval '(defun %tfw-redef (a b c d e) (+ a b c d e)))
    (push (%tfw-redef-caller 0) r)
    (eval '(defun %tfw-redef (a b c d e &optional (k 100)) (list e d c b a k)))
    (push (%tfw-redef-caller 0) r)
    (setf (fdefinition '%tfw-redef) (lambda (&rest xs) (values (length xs) :mv)))
    (push (%tfw-redef-caller 0) r)
    (push (%tfw-calls-opt 1 2 3 4 5 6) r)
    (nreverse r))
  (((0 1 2 3 4)) (10) ((4 3 2 1 0 100)) (5) (11 5 15)))

;;; Across fasls: the entry is installed by the fasl's registration.
(defvar *tfw-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-tfw-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %tfw-compile-and-load (source name)
  (let ((lisp (concatenate 'string *tfw-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-emitting-only typed-framed-entry-wide.fasl
  (progn
    (%tfw-compile-and-load
     "(in-package :cl-user)
      (declaim (notinline %tfw-f))
      (declaim (ftype (function (t double-float t fixnum fixnum fixnum) (values t &optional)) %tfw-f))
      (defun %tfw-f (a x b n m k) (list a (* x n) b m k))"
     "tfw-callee")
    (%tfw-compile-and-load
     "(in-package :cl-user)
      (defun %tfw-f-caller (n) (list (%tfw-f :a 1.5d0 :b n 7 8)))"
     "tfw-caller")
    (list (funcall (intern "%TFW-F-CALLER") 4)
          (dotnet:static "DotCL.LispFunction" "TypedEntryState" (fdefinition (intern "%TFW-F")))
          (handler-case (funcall (intern "%TFW-F") :a 1.5d0 :b 4 7 "k")
            (type-error (e) (list :type-error (type-error-datum e))))
          (progn (eval `(defun ,(intern "%TFW-F") (a x b n m k) (list b a x n m k)))
                 (funcall (intern "%TFW-F-CALLER") 4))))
  (((:a 6d0 :b 7 8)) "framed" (:type-error "k") ((:b :a 1.5d0 4 7 8))))
